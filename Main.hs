{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (addDays)

import Network.HTTP.Client hiding (port)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai (Application, responseLBS)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import System.Environment
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

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

  mgr <- newManager tlsManagerSettings
  initialResp <- httpLbs initialReq mgr
  let cookie = responseCookieJar initialResp
  _ <- httpLbs (authReq { cookieJar = Just cookie}) mgr
  icalResp <- httpLbs (iCalReq { cookieJar = Just cookie}) mgr
  return $ responseBody icalResp

serveCal :: Application
serveCal _ resp = do
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
