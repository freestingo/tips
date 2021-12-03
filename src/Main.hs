{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import qualified Monomer.Lens as L
import Data.String (IsString)

data Tip = Tip
  { _ts :: Int
  , _content :: Text
  , _visible :: Bool
  } deriving (Eq, Show)

data AppModel = AppModel
  { _tips :: [Tip]
  , _searchPromptText :: Text
  , _searchBoxText :: Text
  , _clickCount :: Int
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AddTip
  | SearchTip
  | RemoveTip Int
  | AppIncrease
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

    , vstack (zipWith listTips [0..] (filter (^.visible) (model^.tips)))
    ] `styleBasic` [padding 10]

  titleText text = label text `styleBasic` [textFont "Medium", textSize 20]
  listTips id tip = vstack
    [ label_ (tip^.content) [ellipsis] `styleBasic` [paddingH 10]
    ]
      `nodeKey` showt (tip^.ts) `styleBasic` [paddingT 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  SearchTip ->
    [ Model $ case model^.searchBoxText of
        -- TODO find a better way to do this...
        -- set all tips visible
        "" -> model & (tips . traverse . visible) %~ const True
        -- only match tips with same content as text-field
        _  -> model & tips %~ map (\x -> x & visible .~ ((model^.searchBoxText) == (x^.content)))
    ]
  AddTip | model^.searchBoxText /= "" ->
    [ Model $ model
        & searchBoxText .~ ""
        -- add new tip to list
        & tips %~ (newTip :)
        -- set all tips visible
        & (tips . traverse . visible) %~ const True
    , SetFocusOnKey tipSearchBoxKey
    ]
  RemoveTip id -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]
  _ -> []
  where
    newTip = Tip (wenv ^. L.timestamp) (model^.searchBoxText) True

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
      appInitEvent AppInit
      ]
    model = AppModel
      { _tips = [Tip 1 "tippo" True, Tip 2 "tappo" True]
      , _searchPromptText = "Search tip"
      , _searchBoxText = ""
      , _clickCount = 0
      }

