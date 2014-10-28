module FreePalace.Media.Loader where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString       as ByteString
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Network.HTTP          as HTTP
import qualified Network.URI           as Network
import           System.Directory      as Directory
import           System.FilePath       as Path
import           System.Log.Logger     as Log

import qualified FreePalace.Domain.Net as Net

-- Cache directory may start with ~
fetchCachedBackgroundImagePath :: Net.Hostname -> Net.PortId -> Network.URI -> Path.FilePath -> IO (Maybe Path.FilePath)
fetchCachedBackgroundImagePath host port mediaServerUrl imageName
  | isSuffixOf ".gif" imageName = (join . find isJust) <$> mapM (findCachedMedia host port mediaServerUrl) [ pngFile, jpegFile, gifFile ]
  | otherwise                   = findCachedMedia host port mediaServerUrl imageName
  where pngFile = Path.replaceExtension imageName ".png"
        jpegFile = Path.replaceExtension imageName ".jpg"
        gifFile = imageName

findCachedMedia :: Net.Hostname -> Net.PortId -> Network.URI -> Path.FilePath -> IO (Maybe Path.FilePath)
findCachedMedia host port url filename =
  do
    cacheDirectory <- cacheDirectoryName host port
    let cachedFilePath = Path.combine cacheDirectory $ escapeFilename filename
    isCached <- doesFileExist cachedFilePath
    if isCached
    then return $ Just cachedFilePath -- TODO Still need to see if the cached version is older than the one on the server
    else do
         let possibleUrl = createResourceUrl url filename
             fetchImage Nothing = return Nothing
             fetchImage (Just mediaUrl) = httpGet mediaUrl

         possibleImage <- fetchImage possibleUrl
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
escapeFilename filename = Network.escapeURIString doNotEscape filename
  where doNotEscape character =
          let asciiValue = ord character
          in asciiValue == 45  || asciiValue == 46 || asciiValue == 95 ||
             (asciiValue > 47 && asciiValue < 58) || (asciiValue > 64 && asciiValue < 91) ||
             (asciiValue > 96 && asciiValue < 123)

createResourceUrl :: Network.URI -> FilePath -> Maybe Network.URI
createResourceUrl baseUrl resourceName =
  let baseUrlString = show baseUrl -- This removes any passwords in the URI, which is ok for us, I hope!
      separator = if last baseUrlString == '/' then "" else "/"
      fullUrlString = baseUrlString ++ separator ++ resourceName   -- We won't be getting query strings, anchors etc. here
  in Network.parseURI fullUrlString

httpGet :: Network.URI -> IO (Maybe ByteString.ByteString)
httpGet url =
  do
    Log.debugM "Load.HTTP" $ "Fetching resource from " ++ (show url)
    result <- HTTP.simpleHTTP request
    case result of
      Right response ->
         case HTTP.rspCode response of
           (2,0,0) -> tryGetResponseBody result         -- TODO Maybe deal with response codes 301 and 307, forget the rest.
           _ -> return Nothing
      Left _ -> return Nothing
    where
      request = HTTP.defaultGETRequest_ url
      handleFailureToGetResponseBody = \(SomeException _) -> return Nothing
      tryGetResponseBody result = catch (Just <$> HTTP.getResponseBody result) handleFailureToGetResponseBody



