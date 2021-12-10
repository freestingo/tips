{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module UI where

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

import Model
import Theme
import Data.Typeable (Typeable)

mkTipFormScreen ::
     AppModel
  -> FormTitle
  -> (TitleField, TitleNodeKey)
  -> ContentField
  -> SnippetsField
  -> (AppEvent, AppEvent)
  -> (AppModel -> Bool)
  -> WidgetNode AppModel AppEvent
mkTipFormScreen
  model
  screenTitle
  (titleField, titleKey)
  contentField
  snippetsField
  (cancelAction, successAction)
  validateFields = vstack
    [ titleText screenTitle
    , spacer
    , subtitleText "Title"
    , spacer
    , textField titleField `nodeKey` titleKey
    , spacer
    , subtitleText "Content"
    , spacer
    , textArea contentField
    , spacer
    , hstack [ subtitleText "Snippets"
             , filler
             , button "Add snippet" (AddSnippet snippetsField)
             ]
    , spacer
    , vstack (zipWith (listItem snippetsField) [0..] (model^.snippetsField))
    , spacer
    , hstack [ button "Back" cancelAction `styleBasic` [padding 5]
             , spacer
             , mainButton "Save" successAction
                 `styleBasic` [padding 5]
                 `nodeEnabled` validateFields model
             ]
    ] `styleBasic` [padding 20]

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = withVimBindings $ case model^.currentScreen of
  MainMenu -> mainMenuScreen
  NewTipForm -> newTipFormScreen
  EditTipForm id -> editTipFormScreen id
  Details tip -> detailsScreen tip
  where
    rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def

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
      , vstack (zipWith (listTips rowBgColor) [0..] matchedTips)
      ] `styleBasic` [padding 20]
        where matchedTips = fuzzyTips (model^.searchBoxText) (model^.tips)


    newTipFormScreen = mkTipFormScreen
      model
      "Add new tip"
      (newTipTitle, newTipTitleBoxKey)
      newTipContent
      newTipSnippets
      (CancelNewTip, AddTip)
      newTipFieldsValidated

    editTipFormScreen id = mkTipFormScreen
      model
      "Edit tip"
      (editedTipTitle, editedTipTitleBoxKey)
      editedTipContent
      editedTipSnippets
      (CancelEditTip, EditTip id)
      editedTipFieldsValidated

    detailsScreen tip = vstack
      [ titleText (tip^.title)
      , spacer
      , label_ (tip^.content) [multiline]
      , vstack [ spacer
               , titleText "Snippets"
               , spacer
               , vstack (zipWith (listSnippets rowBgColor) [1..] (_text <$> tip^.snippets))
               ] `nodeVisible` ((T.length . T.unlines) (_text <$> tip^.snippets) > 0)
      , spacer
      , hstack [ button "Back" GoToMainMenu `nodeKey` detailsBackButtonKey `styleBasic` [padding 5]
               , spacer
               , mainButton "Edit" (OpenEditTipForm tip) `styleBasic` [padding 5]
               , spacer
               , mainButton "Delete" (RemoveTip (tip^.ts)) `styleBasic` [padding 5]
               ]
      ] `styleBasic` [padding 20]

withVimBindings :: WidgetNode s AppEvent -> WidgetNode s AppEvent
withVimBindings = keystroke vimKeys
  where vimKeys =
          [ ("C-n", MoveFocus FocusFwd)
          , ("C-p", MoveFocus FocusBwd)
          ]

listItem :: SnippetsField -> SnippetID -> Snippet -> WidgetNode AppModel AppEvent
listItem snippetsField idx item = vstack [
    hstack [
        textField (snippetsField . singular (ix idx) . text)
      , spacer
      , button "Delete" (RemoveSnippet snippetsField idx)
    ]
  ] `nodeKey` showt (item ^. Model.id) `styleBasic` [paddingT 10]

-- TODO create one single "list" widget for displaying list of things
--      and binding custom actions to click events and such
listTips :: Typeable s => Color -> p -> Tip -> WidgetNode s AppEvent
listTips rowBgColor id tip = vstack
  [ boxRow
      rowBgColor
      (ShowDetails tip)
      (hstack [ filler, label_ (tip^.title) [ellipsis], filler ])
  ]
    `nodeKey` showt (tip^.ts)

listSnippets :: (Typeable s, Show a) => Color -> a -> T.Text -> WidgetNode s AppEvent
listSnippets rowBgColor id snippet = vstack
  [ boxRow
      rowBgColor
      (CopyToClipboard snippet)
      (hstack [ filler, label_ snippet [ellipsis], filler ])
  ]
    `nodeKey` T.pack (show id)

titleText :: T.Text -> WidgetNode s e
titleText text = label text `styleBasic` [textFont "Medium", textSize 20, paddingV 10]

subtitleText :: T.Text -> WidgetNode s e
subtitleText text = label text `styleBasic` [textFont "Regular", textSize 18]

boxRow :: (Typeable s, Typeable e) => Color -> e -> WidgetNode s e -> WidgetNode s e
boxRow rowBgColor clickAction content = box_ [onClick clickAction] content
  `styleBasic` [paddingV 10]
  `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]

fuzzyTips :: T.Text -> [Tip] -> [Tip]
fuzzyTips query tips = snd <$> fuzzyFindOn (T.unpack . _title) [T.unpack query] tips

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  LoadTips ->
    [ Task $ SetTips <$> parseTips
    ]
  SetTips ts ->
    [ Model $ model
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
    [ Task $ SetTips <$>
        overwriteTipsFile (newTip : (model^.tips))
    ]
  EditTip id | editedTipFieldsValidated model ->
    [ Task $ SetTips <$>
        overwriteTipsFile (map (\t -> if t^.ts == id then edit t else t) (model^.tips))
    ]
  RemoveTip id ->
    [ Task $ SetTips <$>
        overwriteTipsFile (filter (\t -> (t^.ts) /= id) (model^.tips))
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
  OpenNewTipForm ->
    [ Model $ model
        & currentScreen .~ NewTipForm
        & newTipTitle .~ ""
        & newTipContent .~ ""
        & newTipSnippets .~ []
    , SetFocusOnKey newTipTitleBoxKey
    ]
  OpenEditTipForm tip ->
    [ Model $ model
        & currentScreen .~ EditTipForm (tip^.ts)
        & editedTipTitle .~ tip^.title
        & editedTipContent .~ tip^.content
        & editedTipSnippets .~ tip^.snippets
    , SetFocusOnKey editedTipTitleBoxKey
    ]
  CancelNewTip ->
    [ Model $ model
        & currentScreen .~ MainMenu
        & searchBoxText .~ ""
        & newTipTitle .~ ""
        & newTipContent .~ ""
        & newTipSnippets .~ []
    , SetFocusOnKey tipSearchBoxKey
    ]
  CancelEditTip ->
    [ Model $ model
       & currentScreen .~ MainMenu
       & searchBoxText .~ ""
       & editedTipTitle .~ ""
       & editedTipContent .~ ""
       & editedTipSnippets .~ []
    , SetFocusOnKey tipSearchBoxKey
    ]
  GoToMainMenu ->
    [ Model $ model
       & currentScreen .~ MainMenu
       & searchBoxText .~ ""
    , SetFocusOnKey tipSearchBoxKey
    ]
  ShowDetails tip ->
    [ Model $ model & currentScreen .~ Details tip
    , SetFocusOnKey detailsBackButtonKey
    ]
  CopyToClipboard snippet ->
    [ Task $ do
        setClipboard (T.unpack snippet)
        return GoToMainMenu
    ]
  MoveFocus focusDirection ->
    [ MoveFocusFromKey Nothing focusDirection ]
  _ -> []
  where
    newTip :: Tip
    newTip = Tip (wenv ^. L.timestamp) (model^.newTipTitle) (model^.newTipContent) (filterNonEmpty $ model^.newTipSnippets)

    newSnippet :: Snippet
    newSnippet = Snippet (wenv ^. L.timestamp) ""

    edit :: Tip -> Tip
    edit tip = Tip (tip^.ts) (model^.editedTipTitle) (model^.editedTipContent) (filterNonEmpty $ model^.editedTipSnippets)

    filterNonEmpty :: [Snippet] -> [Snippet]
    filterNonEmpty = filter ((/= "") . (^.text))

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

areNonEmpty :: AppModel -> [Getting T.Text AppModel T.Text] -> Bool
areNonEmpty model = notElem T.empty . map (model^.)

newTipFieldsValidated :: AppModel -> Bool
newTipFieldsValidated model = areNonEmpty model [newTipTitle, newTipContent]

editedTipFieldsValidated :: AppModel -> Bool
editedTipFieldsValidated model = areNonEmpty model [editedTipTitle, editedTipContent]

main :: IO ()
main = do
  startApp initialModel handleEvent buildUI config
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
    initialModel = AppModel
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

