{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (putStrLn, writeFile)
import System.Environment (getArgs)
import Data.Word (Word16)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)
import Data.ByteString (ByteString, writeFile)
import Data.ByteString.Char8 (putStrLn, split, pack, unpack)
import Text.Regex.TDFA (match, makeRegexOpts, blankCompOpt)
import Text.Regex.TDFA.Common
import Data.Conduit.Shell (run, proc, conduit, ($|), Segment)
import Conduit (foldC)

emptyInput = conduit mempty

tlsConnect :: String -> Segment ()
tlsConnect host = proc "openssl" ["s_client", "-showcerts", "-connect", host]

dumpCerts host = emptyInput $| tlsConnect host
dumpCerts' host = dumpCerts host $| conduit foldC

extractCert :: ByteString -> ByteString
extractCert dump = cert
  where
    regex = makeRegexOpts blankCompOpt (ExecOption{captureGroups=False}) ("-----BEGIN CERTIFICATE-----[^-]*-----END CERTIFICATE-----" :: ByteString)
    (_, cert, _) = (match regex dump :: (ByteString, ByteString, ByteString))

verifyCert root cert = proc "openssl" ["verify", "-CAfile", root, cert]

fingerprint cert = proc "openssl" ["x509", "-noout", "-in", cert, "-fingerprint", "-sha256"] $| conduit foldC

toColons = concat . intersperse ":" . words
-- Chrome uses a space separated-format, convert it to the same one used by Firefox and openssl

prepareArgs [root, host] = (root, host, Nothing)
prepareArgs [root, host, hash] = (root, host, Just $ pack $ toColons hash)

checkFingerprint hash Nothing = putStrLn hash
checkFingerprint hash (Just expected) = do
  if hash == expected
    then putStrLn "OK! Fingerprints match"
    else fail $ unpack $ "Failure! Fingerprint differs:\n" <> hash

main = do (root, host, expected) <- fmap prepareArgs getArgs -- TODO use optparse-applicative?
          run $ dumpCerts host -- process could get stuck otherwise... only happens with manfileserver.pentest.ngs?
          cert <- fmap extractCert $ run $ dumpCerts' host
          writeFile "/tmp/tempdownloadedcert" cert -- TODO use System.IO.Temp.systemTempFile
          hash <- run $ do verifyCert root "/tmp/tempdownloadedcert"
                           fingerprint "/tmp/tempdownloadedcert"
          let sha256 = split '=' (head $ split '\n' hash) !! 1
          checkFingerprint sha256 expected
