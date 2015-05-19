{-# LANGUAGE OverloadedStrings #-}

import Data.Default.Class
import Data.Monoid
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (addDays)
import Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit as C
import Network.Connection (TLSSettings (..))

import Network.HTTP.Conduit
import Network.TLS
import Network.TLS.Extra.Cipher
import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import System.Environment
import System.X509
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Web.Scotty

ciphers :: [Cipher]
ciphers =
    [ cipher_DHE_RSA_AES256_SHA256
    , cipher_DHE_RSA_AES128_SHA256
    , cipher_DHE_RSA_AES256_SHA1
    , cipher_DHE_RSA_AES128_SHA1
    , cipher_DHE_DSS_AES256_SHA1
    , cipher_DHE_DSS_AES128_SHA1
    , cipher_AES128_SHA1
    , cipher_AES256_SHA1
    , cipher_RC4_128_MD5
    , cipher_RC4_128_SHA1
    , cipher_RSA_3DES_EDE_CBC_SHA1
    ]

baseHost :: String
baseHost = "www.campus.rwth-aachen.de"

baseUrl :: String
baseUrl = "https://" <> baseHost <> "/office"

fromUrl :: String -> Request
fromUrl = fromJust . parseUrl

getCalendar :: IO LBS.ByteString
getCalendar = do
  user <- BS.pack <$> getEnv "CAMPUS_USER"
  password <- BS.pack <$> getEnv "CAMPUS_PASS"
  curTime <- getCurrentTime
  let startDay   = show $ addDays (-60) $ utctDay curTime
      endDay     = show $ addDays 400 $ utctDay curTime
      iCalReq    = fromUrl $ baseUrl <> "/views/calendar/iCalExport.asp?startdt=" <> startDay <> "&enddt=" <> endDay <> "%2023:59:59"
      authReq    = (fromUrl (baseUrl <> "/views/campus/redirect.asp")) {
         method      = "POST",
         queryString = "?u=" <> user <> "&p=" <> password <> "&login=>%20Login"
        }
      initialReq = fromUrl $ baseUrl <> "/"

  -- Manually create TLSSettings to force using TLS10 or TLS11 as TLS12 does not work currently
  -- https://github.com/vincenthz/hs-tls/issues/87
  certStore <- getSystemCertificateStore
  let tlsSettings = TLSSettings $ (defaultParamsClient baseHost BS.empty)
                      { clientSupported = def { supportedVersions = [TLS10, TLS11], supportedCiphers = ciphers}
                      , clientShared = def { sharedCAStore = certStore }
                      }
      settings = mkManagerSettings tlsSettings Nothing

  withManagerSettings settings $ \mgr -> do
    initialResp <- httpLbs initialReq mgr
    let cookie = responseCookieJar initialResp
    _ <- httpLbs (authReq { cookieJar = Just cookie}) mgr
    icalResp <- httpLbs (iCalReq { cookieJar = Just cookie}) mgr
    return $ responseBody icalResp

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  scotty port $ do
    get "/" $ do
        setHeader "Content-Type" "text/calendar; charset=utf-8"
        setHeader "Content-Disposition" "attachment; filename=calendar.ics"
        cal <- liftIO getCalendar
        raw cal
