{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import Monomer
import TextShow

import qualified Monomer.Lens as L
import Data.String (IsString)
import GHC.Generics

data Tip = Tip
  { _ts :: Int
  , _title :: Text
  , _content :: Text
  , _titleVisible :: Bool
  , _contentVisible :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON Tip
instance ToJSON Tip

data AppModel = AppModel
  { _tips :: [Tip]
  , _searchPromptText :: Text
  , _searchBoxText :: Text
  , _clickCount :: Int
  } deriving (Eq, Show)

data AppEvent
  = LoadTips
  | SetTips [Tip]
  | AddTip
  | SearchTip
  | RemoveTip Int
  deriving (Eq, Show)

makeLenses 'Tip
makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack
    [ titleText (model^.searchPromptText)
    , spacer
    , hstack [ textField_ searchBoxText [placeholder "Write your search here..."]
                 `nodeKey` tipSearchBoxKey
             , spacer
             , button "Search" SearchTip
                 `styleBasic` [paddingH 5]
             , spacer
             , button "Add" AddTip
                 `styleBasic` [paddingH 5]
                 `nodeEnabled` (model^.searchBoxText /= "")
             ]

    , separatorLine `styleBasic` [paddingT 20]

    , vstack (zipWith listTips [0..] (model^.tips))
    ] `styleBasic` [padding 10]

  titleText text = label text `styleBasic` [textFont "Medium", textSize 20]
  listTips id tip = vstack
    [ label_ (tip^.title) [ellipsis] `styleBasic` [paddingH 10] `nodeVisible` tip^.titleVisible
    , label_ (tip^.content) [ellipsis] `styleBasic` [paddingH 10] `nodeVisible` tip^.contentVisible
    ]
      `nodeKey` showt (tip^.ts)

      `styleBasic` [paddingT 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  LoadTips -> [ Task $ SetTips . fromJust . decode <$> B.readFile "tips-list.json"
              ]
  -- show all tips and reset search box
  SetTips ts -> [ Model $ model
                    & searchBoxText .~ ""
                    & tips .~ ts
                    & (tips . traverse . titleVisible) %~ const True
                , SetFocusOnKey tipSearchBoxKey
                ]
  SearchTip ->
    [ Model $ case model^.searchBoxText of
        -- set all tips visible
        "" -> model & (tips . traverse . titleVisible) %~ const True
        -- only match tips with same content as text-field
        _  -> model & tips %~ map (\x -> x & titleVisible .~ ((model^.searchBoxText) == (x^.title)))
    ]
  AddTip | model^.searchBoxText /= "" ->
    [ Task $ SetTips <$> do
        let newTips = newTip : (model^.tips)
        B.writeFile "tips-list.json" $ encode newTips
        return newTips
    ]
  -- AddTip | model^.searchBoxText /= "" ->
  --   [ Model $ model
  --       & searchBoxText .~ ""
  --       -- add new tip to list
  --       & tips %~ (newTip :)
  --       -- set all tips visible
  --       & (tips . traverse . titleVisible) %~ const True
  --   , SetFocusOnKey tipSearchBoxKey
  --   ]
  RemoveTip id -> []
  _ -> []
  where
    newTip = Tip (wenv ^. L.timestamp) (model^.searchBoxText) "dummycontent" True False

tipSearchBoxKey :: (IsString a) => a
tipSearchBoxKey = "tipSearchBoxKey"

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Hello world",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf",
      appInitEvent LoadTips
      ]
    model = AppModel
      { _tips = []
      , _searchPromptText = "Search tip"
      , _searchBoxText = ""
      , _clickCount = 0
      }

