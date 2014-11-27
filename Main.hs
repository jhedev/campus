{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (addDays)
import Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit as C
import Network.HTTP.Conduit
import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import System.Environment
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Web.Scotty

fromUrl :: String -> Request
fromUrl = fromJust . parseUrl

getCalendar :: IO LBS.ByteString
getCalendar = do
  user <- BS.pack <$> getEnv "CAMPUS_USER"
  password <- BS.pack <$> getEnv "CAMPUS_PASS"
  curTime <- getCurrentTime
  let startDay = show $ addDays (-60) $ utctDay curTime
      endDay = show $ addDays 400 $ utctDay curTime
      iCalReq = fromUrl $ "https://www.campus.rwth-aachen.de/office/views/calendar/iCalExport.asp?startdt=" <> startDay <> "&enddt=" <> endDay <> "%2023:59:59"
      authReq = (fromUrl "https://www.campus.rwth-aachen.de/office/views/campus/redirect.asp") {
         method = "POST",
         queryString = "?u=" <> user <> "&p=" <> password <> "&login=>%20Login"
        }
      initialReq = fromUrl "https://www.campus.rwth-aachen.de/office/"
  withManager $ \mgr -> do
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
        setHeader "Content" "text/calendar"
        setHeader "Content-Disposition" "attachment; filename=calendar.ics"
        cal <- liftIO $ getCalendar
        raw cal
