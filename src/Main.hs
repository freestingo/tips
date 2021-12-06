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

data Screen
  = MainMenu
  | NewTipForm
  | Details Tip
  deriving (Eq, Show)

data Tip = Tip
  { _ts :: Int
  , _title :: Text
  , _content :: Text
  , _visible :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON Tip
instance ToJSON Tip

data AppModel = AppModel
  { _tips :: [Tip]
  , _currentScreen :: Screen
  , _searchBoxText :: Text
  , _newTipTitle :: Text
  , _newTipContent :: Text
  } deriving (Eq, Show)

data AppEvent
  = LoadTips
  | SetTips [Tip]
  | AddTip
  | SearchTip
  | RemoveTip Int
  | CancelNewTip
  | OpenNewTipForm
  | GoToMainMenu
  | ShowDetails Tip
  deriving (Eq, Show)

makeLenses 'Tip
makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = case model^.currentScreen of
    MainMenu -> vstack
      [ titleText "Search tip"
      , spacer
      , hstack [ textField_ searchBoxText [placeholder "Write your search here..."]
                   `nodeKey` tipSearchBoxKey
               , spacer
               , button "Search" SearchTip
                   `styleBasic` [paddingH 5]
               , spacer
               , button "Add new" OpenNewTipForm
                   `styleBasic` [paddingH 5]
               ]

      , separatorLine `styleBasic` [paddingT 20]

      , vstack (zipWith listTips [0..] (model^.tips))
      ] `styleBasic` [padding 10]
    NewTipForm -> vstack
      [ titleText "Add new tip"
      , spacer
      , subtitleText "Title"
      , spacer
      , textField newTipTitle
      , spacer
      , subtitleText "Content"
      , spacer
      , textArea newTipContent
      , spacer
      , hstack [ button "Save" AddTip
                   `styleBasic` [padding 5]
                   `nodeEnabled` newTipFieldsValidated model
               , spacer
               , button "Back" CancelNewTip `styleBasic` [padding 5]
               ]
      ] `styleBasic` [padding 10]
    Details tip -> vstack
      [ titleText (tip^.title)
      , spacer
      , label_ (tip^.content) [multiline]
      , spacer

      , hstack [ button "Back" GoToMainMenu `styleBasic` [padding 5]
               , spacer
               , button "Delete" (RemoveTip (tip^.ts)) `styleBasic` [padding 5]
               ]
      ] `styleBasic` [padding 10]

  titleText text = label text `styleBasic` [textFont "Medium", textSize 20]
  subtitleText text = label text `styleBasic` [textFont "Regular", textSize 18]
  listTips id tip = vstack
    [ hstack [ label_ (tip^.title) [ellipsis] `styleBasic` [paddingH 10]
             , spacer
             , button "Open" (ShowDetails tip)
             ] `nodeVisible` tip^.visible
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
  -- show all tips, reset search box and go back to main menu
  SetTips ts -> [ Model $ model
                    & searchBoxText .~ ""
                    & tips .~ ts
                    & (tips . traverse . visible) %~ const True
                    & currentScreen .~ MainMenu
                , SetFocusOnKey tipSearchBoxKey
                ]
  SearchTip ->
    [ Model $ case model^.searchBoxText of
        -- set all tips visible
        "" -> model & (tips . traverse . visible) %~ const True
        -- only match tips with same content as text-field
        _  -> model & tips %~ map (\x -> x & visible .~ ((model^.searchBoxText) == (x^.title)))
    ]
  AddTip | newTipFieldsValidated model ->
    [ Task $ SetTips <$> overwriteTipsFile (newTip : (model^.tips))
    ]
  RemoveTip id ->
    [ Task $ SetTips <$> overwriteTipsFile (filter (\t -> (t^.ts) /= id) (model^.tips))
    ]
  OpenNewTipForm -> [ Model $ model & currentScreen .~ NewTipForm ]
  CancelNewTip -> [ Model $ model
                      & currentScreen .~ MainMenu
                      & newTipTitle .~ ""
                      & newTipContent .~ ""
                  ]
  GoToMainMenu -> [ Model $ model & currentScreen .~ MainMenu ]
  ShowDetails tip -> [ Model $ model & currentScreen .~ Details tip ]
  _ -> []
  where
    newTip :: Tip
    newTip = Tip (wenv ^. L.timestamp) (model^.newTipTitle) (model^.newTipContent) True
    overwriteTipsFile :: [Tip] -> IO [Tip]
    overwriteTipsFile tips = do
      B.writeFile "tips-list.json" $ encode tips
      return tips

tipSearchBoxKey :: (IsString a) => a
tipSearchBoxKey = "tipSearchBoxKey"

newTipFieldsValidated :: AppModel -> Bool
newTipFieldsValidated model = model^.newTipTitle /= "" && model^.newTipContent /= ""

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
      , _currentScreen = MainMenu
      , _searchBoxText = ""
      , _newTipTitle = ""
      , _newTipContent = ""
      }

