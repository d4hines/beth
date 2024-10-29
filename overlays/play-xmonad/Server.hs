{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Server (startServer) where 

import Web.Scotty
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import System.Process (spawnCommand)
import System.Environment (lookupEnv, getEnv)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status404)
import Data.Maybe (listToMaybe)
import Data.Aeson.Types (Parser, Value (..), (.:), (.=), object)
import Data.Aeson (Options(..), FromJSON(..), ToJSON, toEncoding, defaultOptions, genericParseJSON, genericToEncoding, encode, eitherDecode)
import System.Environment (getArgs)
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)

customOptions :: Data.Aeson.Options
customOptions = Data.Aeson.defaultOptions
  { fieldLabelModifier = \label -> case label of
      "categoryName" -> "name"
      "gameId" -> "id"
      "gameName" -> "name"
      "gameThumbnail" -> "thumbnail"
      "showName" -> "name"
      "showId" -> "id"
      "showThumbnail" -> "thumbnail"
      "musicName" -> "name"
      "musicId" -> "id"
      "musicThumbnail" -> "thumbnail"
      "shows_" -> "shows"
      other -> other
  }

-- Define data structures matching the JSON
data Database = Database
  { games :: [Category Game]
  , shows_ :: [Category ShowItem]
  , music :: [Category MusicItem]
  } deriving (Show, Generic)

instance ToJSON Database where
  toEncoding = genericToEncoding customOptions

instance FromJSON Database where
  parseJSON = genericParseJSON customOptions

-- Category data structure
data Category a = Category
  { categoryName :: T.Text
  , items :: [a]
  } deriving (Show, Generic)

instance ToJSON a => ToJSON (Category a) where
  toEncoding = genericToEncoding customOptions

instance FromJSON a => FromJSON (Category a) where
  parseJSON = genericParseJSON customOptions

-- Game data structure
data Game = Game
  { gameId :: T.Text
  , gameName :: T.Text
  , gameThumbnail :: T.Text
  , command :: T.Text
  } deriving (Show, Generic)

instance ToJSON Game where
  toEncoding = genericToEncoding customOptions

instance FromJSON Game where
  parseJSON = genericParseJSON customOptions

-- Show item data structure
data ShowItem = ShowItem
  { showId :: T.Text
  , showName :: T.Text
  , showThumbnail :: T.Text
  } deriving (Show, Generic)

instance ToJSON ShowItem where
  toEncoding = genericToEncoding customOptions

instance FromJSON ShowItem where
  parseJSON = genericParseJSON customOptions

-- Music item data structure
data MusicItem = MusicItem
  { musicId :: T.Text
  , musicName :: T.Text
  , musicThumbnail :: T.Text
  } deriving (Show, Generic)

instance ToJSON MusicItem where
  toEncoding = genericToEncoding customOptions

instance FromJSON MusicItem where
  parseJSON = genericParseJSON customOptions

-- Main function to run the application
startServer :: IO ()
startServer = do
  homeDir <- getEnv "HOME"
  let defaultPath = homeDir ++ "/.config/play/content.json"
  filePath <- maybe defaultPath id <$> lookupEnv "PLAY_CONTENT"
  -- Read and parse the JSON file
  jsonData <- B.readFile filePath
  let db = eitherDecode jsonData :: Either String Database
  case db of
    Left err -> putStrLn $ "Error parsing JSON: " ++ err
    Right database -> scotty 3000 $ do
      middleware corsMiddleware
      app database
  
-- Application with endpoints
app :: Database -> ScottyM ()
app db = do
  options (regex ".*") $ do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
    addHeader "Access-Control-Allow-Headers" "Content-Type, Authorization"
    text ""

  -- Endpoint to get shows
  get "/api/shows" $ do
    json (shows_ db)

  -- Endpoint to get music
  get "/api/music" $ do
    json (music db)

  -- Endpoint to get games
  get "/api/games" $ do
    json (games db)

  -- Endpoint to run a game by id
  get "/api/games/:id/run" $ do
    gameId <- param "id" :: ActionM T.Text
    let mGame = findNameByID (games db) gameId
    case mGame of
      Nothing -> do
        status status404
        json $ object ["error" .= ("Game not found" :: T.Text)]
      Just game -> do
        liftIO $ spawnCommand (T.unpack $ command game)
        json $ object ["status" .= ("Game launched" :: T.Text)]

-- Helper function to find a game by id
findNameByID :: [Category Game] -> T.Text -> Maybe Game
findNameByID categories gameIdToFind =
  listToMaybe [game | Category _ items <- categories, game <- items, gameId game == gameIdToFind]

-- CORS Middleware: Add the necessary headers to allow cross-origin requests.
corsMiddleware :: Middleware
corsMiddleware = addHeaders
  [ ("Access-Control-Allow-Origin", "*")  -- Allow requests from any origin
  , ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")  -- Allowed HTTP methods
  , ("Access-Control-Allow-Headers", "Content-Type, Authorization")  -- Allowed headers
  ]