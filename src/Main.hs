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
import Data.Default (Default(def))

data Screen
  = MainMenu
  | NewTipForm
  | EditTipForm ID
  | Details Tip
  deriving (Eq, Show)

type ID = Int

data Tip = Tip
  { _ts :: ID
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
  , _editedTipTitle :: Text
  , _editedTipContent :: Text
  } deriving (Eq, Show)

data AppEvent
  = LoadTips
  | SetTips [Tip]
  | AddTip
  | EditTip ID
  | SearchTip
  | RemoveTip ID
  | CancelNewTip
  | CancelEditTip
  | OpenNewTipForm
  | OpenEditTipForm Tip
  | GoToMainMenu
  | ShowDetails Tip
  deriving (Eq, Show)

makeLenses 'Tip
makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = case model^.currentScreen of
  MainMenu -> mainMenuScreen
  NewTipForm -> newTipFormScreen
  EditTipForm id -> editTipFormScreen id
  Details tip -> detailsScreen tip
  where
    mainMenuScreen = keystroke [("Enter", SearchTip)] $ vstack
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

    newTipFormScreen = vstack
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

    editTipFormScreen id = vstack
      [ titleText "Edit tip"
      , spacer
      , subtitleText "Title"
      , spacer
      , textField editedTipTitle
      , spacer
      , subtitleText "Content"
      , spacer
      , textArea editedTipContent
      , spacer
      , hstack [ button "Save" (EditTip id)
                   `styleBasic` [padding 5]
                   `nodeEnabled` editedTipFieldsValidated model
               , spacer
               , button "Back" CancelEditTip `styleBasic` [padding 5]
               ]
      ] `styleBasic` [padding 10]

    detailsScreen tip = vstack
      [ titleText (tip^.title)
      , spacer
      , label_ (tip^.content) [multiline]
      , spacer

      , hstack [ button "Back" GoToMainMenu `styleBasic` [padding 5]
               , spacer
               , button "Edit" (OpenEditTipForm tip) `styleBasic` [padding 5]
               , spacer
               , button "Delete" (RemoveTip (tip^.ts)) `styleBasic` [padding 5]
               ]
      ] `styleBasic` [padding 10]

    listTips id tip = vstack
      [ boxRow
          (ShowDetails tip)
          (hstack [ filler
                  , label_ (tip^.title) [ellipsis]
                  , filler
                  ] `nodeVisible` tip^.visible
          )
      ]
        `nodeKey` showt (tip^.ts)

    titleText text = label text `styleBasic` [textFont "Medium", textSize 20]
    subtitleText text = label text `styleBasic` [textFont "Regular", textSize 18]
    boxRow clickAction content = box_ [onClick clickAction] content
      `styleBasic` [paddingV 10]
      `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
    rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  LoadTips -> [ Task $ SetTips <$> parseTips
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
  EditTip id | editedTipFieldsValidated model ->
    [ Task $ SetTips <$> overwriteTipsFile (map (\t -> if t^.ts == id then edit t else t) (model^.tips))
    ]
  RemoveTip id ->
    [ Task $ SetTips <$> overwriteTipsFile (filter (\t -> (t^.ts) /= id) (model^.tips))
    ]
  OpenNewTipForm -> [ Model $ model & currentScreen .~ NewTipForm ]
  OpenEditTipForm tip -> [ Model $ model
                             & currentScreen .~ EditTipForm (tip^.ts)
                             & editedTipTitle .~ tip^.title
                             & editedTipContent .~ tip^.content
                         ]
  CancelNewTip -> [ Model $ model
                      & currentScreen .~ MainMenu
                      & newTipTitle .~ ""
                      & newTipContent .~ ""
                  ]
  CancelEditTip -> [ Model $ model
                      & currentScreen .~ MainMenu
                      & editedTipTitle .~ ""
                      & editedTipContent .~ ""
                  ]
  GoToMainMenu -> [ Model $ model & currentScreen .~ MainMenu ]
  ShowDetails tip -> [ Model $ model & currentScreen .~ Details tip ]
  _ -> []
  where
    newTip :: Tip
    newTip = Tip (wenv ^. L.timestamp) (model^.newTipTitle) (model^.newTipContent) True
    edit :: Tip -> Tip
    edit tip = Tip (tip^.ts) (model^.editedTipTitle) (model^.editedTipContent) True
    parseTips :: IO [Tip]
    parseTips = fromJust . decode <$> B.readFile "tips-list.json"
    overwriteTipsFile :: [Tip] -> IO [Tip]
    overwriteTipsFile tips = do
      B.writeFile "tips-list.json" $ encode tips
      return tips

tipSearchBoxKey :: (IsString a) => a
tipSearchBoxKey = "tipSearchBoxKey"

newTipFieldsValidated :: AppModel -> Bool
newTipFieldsValidated model = model^.newTipTitle /= "" && model^.newTipContent /= ""

editedTipFieldsValidated :: AppModel -> Bool
editedTipFieldsValidated model = model^.editedTipTitle /= "" && model^.editedTipContent /= ""

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hello world"
      , appTheme customDarkTheme
      , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
      , appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf"
      , appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf"
      , appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf"
      , appInitEvent LoadTips
      ]
    model = AppModel
      { _tips = []
      , _currentScreen = MainMenu
      , _searchBoxText = ""
      , _newTipTitle = ""
      , _newTipContent = ""
      , _editedTipTitle = ""
      , _editedTipContent = ""
      }

