module FreePalace.Media.Loader where

import qualified Data.ByteString as ByteString
import qualified Network.HTTP as HTTP
import qualified Network.URI as URI ( parseURI, escapeURIString )
import qualified Network.Stream as Stream
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.List
import Data.Char
import Data.Maybe
import System.IO
import System.FilePath as Path
import System.Directory as Directory
import Text.Regex
import System.Log.Logger as Log

import qualified FreePalace.Net.Types as Net
import qualified FreePalace.Media.Types as Media

-- Cache directory may start with ~
fetchCachedBackgroundImagePath :: Net.Hostname -> Net.PortId -> Net.URL -> Path.FilePath -> IO (Maybe Path.FilePath)
fetchCachedBackgroundImagePath host port mediaServerUrl imageName
  | isSuffixOf ".gif" imageName = (join . find isJust) <$> mapM (findCachedMedia host port mediaServerUrl) [ pngFile, jpegFile, gifFile ]
  | otherwise                   = findCachedMedia host port mediaServerUrl imageName
  where pngFile = Path.replaceExtension imageName ".png"
        jpegFile = Path.replaceExtension imageName ".jpg"
        gifFile = imageName

findCachedMedia :: Net.Hostname -> Net.PortId -> Net.URL -> Path.FilePath -> IO (Maybe Path.FilePath)
findCachedMedia host port url filename =
  do
    cacheDirectory <- cacheDirectoryName host port
    let cachedFilePath = Path.combine cacheDirectory $ escapeFilename filename
    isCached <- doesFileExist cachedFilePath
    if isCached
      then return $ Just cachedFilePath
      else do
             possibleImage <- httpGet $ createResourceUrl url filename
             case possibleImage of
               Just imageData ->
                 do
                   Log.debugM "Load.BackgroundImage" $ "Writing file " ++ filename ++ " to cache directory " ++ cacheDirectory
                   createDirectoryIfMissing True cacheDirectory
                   ByteString.writeFile cachedFilePath imageData 
                   return $ Just cachedFilePath
               Nothing -> return Nothing

cacheDirectoryName :: Net.Hostname -> Net.PortId -> IO Path.FilePath
cacheDirectoryName host port =
  do
    appDataDirectory <- Directory.getAppUserDataDirectory "freepalace"
    return $ Path.combine appDataDirectory (host ++ "." ++ port)
  
-- Allowable characters in filename [A-Za-z0-9._-] all else URL escaped.
escapeFilename :: Path.FilePath -> Path.FilePath
escapeFilename filename = URI.escapeURIString doNotEscape filename
  where doNotEscape character =
          let asciiValue = ord character
          in asciiValue == 45  || asciiValue == 46 || asciiValue == 95 ||
             (asciiValue > 47 && asciiValue < 58) || (asciiValue > 64 && asciiValue < 91) ||
             (asciiValue > 96 && asciiValue < 123)

createResourceUrl :: Net.URL -> FilePath -> Net.URL
createResourceUrl baseUrl resourceName
  | last baseUrl == '/'   = baseUrl ++ resourceName   -- We won't be getting query strings, anchors etc. here
  | otherwise             = baseUrl ++ "/" ++ resourceName

httpGet :: Net.URL -> IO (Maybe ByteString.ByteString)
httpGet url =
  do
    case (HTTP.defaultGETRequest_ <$> URI.parseURI url) of
      Just request ->
        do
          Log.debugM "Load.HTTP" $ "Fetching resource from " ++ url
          result <- HTTP.simpleHTTP request
          case result of
            Right response ->
              case HTTP.rspCode response of
                (2,0,0) -> tryGetResponseBody result         -- TODO Maybe deal with response codes 301 and 307, forget the rest.
                _ -> return Nothing
            Left _ -> return Nothing
      Nothing -> return Nothing
    where
      handleFailureToGetResponseBody = \(SomeException exception) -> return Nothing
      tryGetResponseBody result = catch (Just <$> HTTP.getResponseBody result) handleFailureToGetResponseBody



