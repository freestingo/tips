{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Monomer hiding (MoveFocus)
import TextShow

import qualified Monomer.Lens as L
import Data.String (IsString)
import GHC.Generics
import Data.Default (Default(def))
import Text.FuzzyFind
import Data.List
import System.Hclip
import Monomer.Core.Themes.BaseTheme

import Theme

data Screen
  = MainMenu
  | NewTipForm
  | EditTipForm TipID
  | Details Tip
  deriving (Eq, Show)

type TipID = Int
type SnippetID = Int
type SnippetsField = Lens' AppModel [Snippet]

data Snippet = Snippet
  { _id :: SnippetID
  , _text :: T.Text
  } deriving (Eq, Show, Generic)

data Tip = Tip
  { _ts :: TipID
  , _title :: T.Text
  , _content :: T.Text
  , _snippets :: [Snippet]
  } deriving (Eq, Show, Generic)

instance FromJSON Snippet
instance ToJSON Snippet
instance FromJSON Tip
instance ToJSON Tip

data AppModel = AppModel
  { _tips :: [Tip]
  , _currentScreen :: Screen
  , _searchBoxText :: T.Text
  , _newTipTitle :: T.Text
  , _newTipContent :: T.Text
  , _newTipSnippets :: [Snippet]
  , _editedTipTitle :: T.Text
  , _editedTipContent :: T.Text
  , _editedTipSnippets :: [Snippet]
  } deriving (Eq, Show)

data AppEvent
  = LoadTips
  | SetTips [Tip]
  | AddTip
  | EditTip TipID
  | ShowBestMatchingTip [Tip]
  | RemoveTip TipID
  | AddSnippet SnippetsField
  | RemoveSnippet SnippetsField SnippetID
  | CancelNewTip
  | CancelEditTip
  | OpenNewTipForm
  | OpenEditTipForm Tip
  | GoToMainMenu
  | ShowDetails Tip
  | CopyToClipboard T.Text
  | MoveFocus FocusDirection

makeLenses 'Snippet
makeLenses 'Tip
makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = keystroke [("C-n", MoveFocus FocusFwd), ("C-p", MoveFocus FocusBwd)]
  $ case model^.currentScreen of
    MainMenu -> mainMenuScreen
    NewTipForm -> newTipFormScreen
    EditTipForm id -> editTipFormScreen id
    Details tip -> detailsScreen tip
  where
    mainMenuScreen = keystroke [("Enter", ShowBestMatchingTip matchedTips)] $ vstack
      [ titleText "Search tip"
      , spacer
      , hstack [ textField_ searchBoxText [placeholder "Write your search here..."]
                   `nodeKey` tipSearchBoxKey
               , spacer
               , button "Add new" OpenNewTipForm
                   `styleBasic` [paddingH 5]
               ]
      , separatorLine `styleBasic` [paddingT 20]
      , vstack (zipWith listTips [0..] matchedTips)
      ] `styleBasic` [padding 20]
        where matchedTips = fuzzyTips (model^.searchBoxText) (model^.tips)

    newTipFormScreen = vstack
      [ titleText "Add new tip"
      , spacer
      , subtitleText "Title"
      , spacer
      , textField newTipTitle `nodeKey` newTipTitleBoxKey
      , spacer
      , subtitleText "Content"
      , spacer
      , textArea newTipContent
      , spacer
      , subtitleText "Snippets"
      , spacer
      , button "Add snippet" (AddSnippet newTipSnippets)
      , vstack (zipWith (listItem newTipSnippets) [0..] (model^.newTipSnippets))
      , spacer
      , hstack [ button "Back" CancelNewTip `styleBasic` [padding 5]
               , spacer
               , mainButton "Save" AddTip
                   `styleBasic` [padding 5]
                   `nodeEnabled` newTipFieldsValidated model
               ]
      ] `styleBasic` [padding 20]

    editTipFormScreen id = vstack
      [ titleText "Edit tip"
      , spacer
      , subtitleText "Title"
      , spacer
      , textField editedTipTitle `nodeKey` editedTipTitleBoxKey
      , spacer
      , subtitleText "Content"
      , spacer
      , textArea editedTipContent
      , spacer
      , subtitleText "Snippets"
      , spacer
      , button "Add snippet" (AddSnippet editedTipSnippets)
      , spacer
      , vstack (zipWith (listItem editedTipSnippets) [0..] (model^.editedTipSnippets))
      , spacer
      , hstack [ button "Back" CancelEditTip `styleBasic` [padding 5]
               , spacer
               , mainButton "Save" (EditTip id)
                   `styleBasic` [padding 5]
                   `nodeEnabled` editedTipFieldsValidated model
               ]
      ] `styleBasic` [padding 20]

    detailsScreen tip = vstack
      [ titleText (tip^.title)
      , spacer
      , label_ (tip^.content) [multiline]
      , vstack [ spacer
               , titleText "Snippets"
               , spacer
               , vstack (zipWith listSnippets [1..] (_text <$> tip^.snippets))
               ] `nodeVisible` ((T.length . T.unlines) (_text <$> tip^.snippets) > 0)
      , spacer
      , hstack [ button "Back" GoToMainMenu `nodeKey` detailsBackButtonKey `styleBasic` [padding 5]
               , spacer
               , mainButton "Edit" (OpenEditTipForm tip) `styleBasic` [padding 5]
               , spacer
               , mainButton "Delete" (RemoveTip (tip^.ts)) `styleBasic` [padding 5]
               ]
      ] `styleBasic` [padding 20]

    listItem :: SnippetsField -> SnippetID -> Snippet -> WidgetNode AppModel AppEvent
    listItem snippetsField idx item = vstack [
        hstack [
            textField (snippetsField . singular (ix idx) . text)
          , spacer
          , button "Delete" (RemoveSnippet snippetsField idx)
        ]
      ] `nodeKey` showt (item ^. Main.id) `styleBasic` [paddingT 10]

    listTips id tip = vstack
      [ boxRow
          (ShowDetails tip)
          (hstack [ filler
                  , label_ (tip^.title) [ellipsis]
                  , filler
                  ]
          )
      ]
        `nodeKey` showt (tip^.ts)

    listSnippets id snippet = vstack
      [ boxRow
          (CopyToClipboard snippet)
          (hstack [ filler
                  , label_ snippet [ellipsis]
                  , filler
                  ]
          )
      ]
        `nodeKey` T.pack (show id)

    titleText text = label text `styleBasic` [textFont "Medium", textSize 20, paddingV 10]
    subtitleText text = label text `styleBasic` [textFont "Regular", textSize 18]
    boxRow clickAction content = box_ [onClick clickAction] content
      `styleBasic` [paddingV 10]
      `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
    rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def

fuzzyTips :: T.Text -> [Tip] -> [Tip]
fuzzyTips query tips = snd <$> fuzzyFindOn (T.unpack . _title) [T.unpack query] tips

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  LoadTips -> [ Task $ SetTips <$> parseTips
              ]
  SetTips ts -> [ Model $ model
                    & searchBoxText .~ ""
                    & tips .~ ts
                    & currentScreen .~ MainMenu
                , SetFocusOnKey tipSearchBoxKey
                ]
  ShowBestMatchingTip matchedTips -> case matchedTips of
    t : ts -> [ Model $ model & currentScreen .~ Details t
              , SetFocusOnKey detailsBackButtonKey
              ]
    _ -> []
  AddTip | newTipFieldsValidated model ->
    [ Task $ SetTips <$> overwriteTipsFile (newTip : (model^.tips))
    ]
  EditTip id | editedTipFieldsValidated model ->
    [ Task $ SetTips <$> overwriteTipsFile (map (\t -> if t^.ts == id then edit t else t) (model^.tips))
    ]
  RemoveTip id ->
    [ Task $ SetTips <$> overwriteTipsFile (filter (\t -> (t^.ts) /= id) (model^.tips))
    ]
  AddSnippet snippetsField ->
    [ Model $ model
        & snippetsField .~ ((model^.snippetsField) |> newSnippet)
    ]
  RemoveSnippet snippetsField snippetId ->
    [ Model $ model
        & snippetsField .~ removeIdx snippetId (model^.snippetsField)
    ]
      where removeIdx :: Int -> [a] -> [a]
            removeIdx idx lst = part1 ++ drop 1 part2 where
              (part1, part2) = splitAt idx lst
  OpenNewTipForm -> [ Model $ model
                        & currentScreen .~ NewTipForm
                        & newTipTitle .~ ""
                        & newTipContent .~ ""
                        & newTipSnippets .~ []
                    , SetFocusOnKey newTipTitleBoxKey
                    ]
  OpenEditTipForm tip -> [ Model $ model
                             & currentScreen .~ EditTipForm (tip^.ts)
                             & editedTipTitle .~ tip^.title
                             & editedTipContent .~ tip^.content
                             & editedTipSnippets .~ tip^.snippets
                         , SetFocusOnKey editedTipTitleBoxKey
                         ]
  CancelNewTip -> [ Model $ model
                      & currentScreen .~ MainMenu
                      & searchBoxText .~ ""
                      & newTipTitle .~ ""
                      & newTipContent .~ ""
                      & newTipSnippets .~ []
                  , SetFocusOnKey tipSearchBoxKey
                  ]
  CancelEditTip -> [ Model $ model
                      & currentScreen .~ MainMenu
                      & searchBoxText .~ ""
                      & editedTipTitle .~ ""
                      & editedTipContent .~ ""
                      & editedTipSnippets .~ []
                   , SetFocusOnKey tipSearchBoxKey
                   ]
  GoToMainMenu -> [ Model $ model
                     & currentScreen .~ MainMenu
                     & searchBoxText .~ ""
                  , SetFocusOnKey tipSearchBoxKey
                  ]
  ShowDetails tip -> [ Model $ model & currentScreen .~ Details tip
                     , SetFocusOnKey detailsBackButtonKey
                     ]
  CopyToClipboard snippet ->
    [ Task $ do
        setClipboard (T.unpack snippet)
        return GoToMainMenu
    ]
  MoveFocus focusDirection -> [ MoveFocusFromKey Nothing focusDirection ]
  _ -> []
  where
    newTip :: Tip
    newTip = Tip (wenv ^. L.timestamp) (model^.newTipTitle) (model^.newTipContent) (model^.newTipSnippets)
    newSnippet :: Snippet
    newSnippet = Snippet (wenv ^. L.timestamp) ""
    edit :: Tip -> Tip
    edit tip = Tip (tip^.ts) (model^.editedTipTitle) (model^.editedTipContent) (model^.editedTipSnippets)
    parseTips :: IO [Tip]
    parseTips = fromJust . decode <$> B.readFile "tips-list.json"
    overwriteTipsFile :: [Tip] -> IO [Tip]
    overwriteTipsFile tips = do
      B.writeFile "tips-list.json" $ encode tips
      return tips

tipSearchBoxKey :: (IsString a) => a
tipSearchBoxKey = "tipSearchBoxKey"

newTipTitleBoxKey :: (IsString a) => a
newTipTitleBoxKey = "newTipTitleBoxKey"

editedTipTitleBoxKey :: (IsString a) => a
editedTipTitleBoxKey = "editedTipTitleBoxKey"

detailsBackButtonKey :: (IsString a) => a
detailsBackButtonKey = "detailsBackButtonKey"

newTipFieldsValidated :: AppModel -> Bool
newTipFieldsValidated model = model^.newTipTitle /= "" && model^.newTipContent /= ""

editedTipFieldsValidated :: AppModel -> Bool
editedTipFieldsValidated model = model^.editedTipTitle /= "" && model^.editedTipContent /= ""

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Tips"
      , appTheme customDarkTheme
      , appFontDef "Regular" "./assets/fonts/hasklug-mono.otf"
      , appFontDef "Medium" "./assets/fonts/hasklug-medium-mono.otf"
      , appFontDef "Bold" "./assets/fonts/hasklug-bold-mono.otf"
      , appFontDef "Italic" "./assets/fonts/hasklug-italic-mono.otf"
      , appInitEvent LoadTips
      ]
    model = AppModel
      { _tips = []
      , _currentScreen = MainMenu
      , _searchBoxText = ""
      , _newTipTitle = ""
      , _newTipContent = ""
      , _newTipSnippets = []
      , _editedTipTitle = ""
      , _editedTipContent = ""
      , _editedTipSnippets = []
      }

