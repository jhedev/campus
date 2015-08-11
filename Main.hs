{-# LANGUAGE OverloadedStrings #-}

import Data.Default.Class (def)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (addDays)
import Network.Connection (TLSSettings (..))

import Network.HTTP.Conduit hiding (port)
import Network.HTTP.Types.Status (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.TLS
import Network.TLS.Extra.Cipher
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai (Application, responseLBS)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import System.Environment
import System.X509
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

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

  mgr <- newManager settings
  initialResp <- httpLbs initialReq mgr
  let cookie = responseCookieJar initialResp
  _ <- httpLbs (authReq { cookieJar = Just cookie}) mgr
  icalResp <- httpLbs (iCalReq { cookieJar = Just cookie}) mgr
  return $ responseBody icalResp

serveCal :: Application
serveCal req resp = do
  print req
  cal <- getCalendar
  resp $ responseLBS status200
    [ (hContentType, "text/calendar; charset=utf-8")
    , ("Content-Disposition", "attachment; filename=calendar.ics")] cal

app :: Application
app = logStdout serveCal

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  run port app
