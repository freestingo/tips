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


buildUI :: TipsWenv -> AppModel -> TipsNode
buildUI wenv model = withVimBindings $ case model ^. currentScreen of
  NewTipForm -> tipFormScreen "Add new tip" newTipTitleBoxKey (OnCancel CancelNewTip, OnSuccess AddTip)
  EditTipForm id -> tipFormScreen "Edit tip" editedTipTitleBoxKey (OnCancel CancelEditTip, OnSuccess (EditTip id))
  MainMenu -> mainMenuScreen
  Details tip -> detailsScreen tip

  where
    rowBgColor :: Color
    rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def

    tipFormScreen :: FormTitle -> TitleNodeKey -> (ActionOnCancel, ActionOnSuccess) -> TipsNode
    tipFormScreen screenTitle titleKey (OnCancel cancelAction, OnSuccess successAction) =
      vstack
        [ titleText screenTitle
        , spacer
        , subtitleText "Title"
        , spacer
        , textField (activeTip . title) `nodeKey` titleKey
        , spacer
        , subtitleText "Content"
        , spacer
        , textArea (activeTip . content)
        , spacer
        , hstack [ subtitleText "Snippets"
                 , filler
                 , button "Add snippet" AddSnippet
                 ]
        , spacer
        , vstack (zipWith listItem [0..] (model ^. activeTip . snippets))
        , spacer
        , hstack [ button "Back" cancelAction `styleBasic` [padding 5]
                 , spacer
                 , mainButton "Save" successAction
                     `styleBasic` [padding 5]
                     `nodeEnabled` isValidForm model
                 ]
        ] `styleBasic` [padding 20]

    mainMenuScreen :: TipsNode
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
        where matchedTips = fuzzyTips (model ^. searchBoxText) (model ^. tips)

    detailsScreen :: Tip -> TipsNode
    detailsScreen tip = vstack
      [ titleText (tip ^. title)
      , spacer
      , label_ (tip ^. content) [multiline]
      , vstack [ spacer
               , titleText "Snippets"
               , spacer
               , vstack (zipWith (listSnippets rowBgColor) [1..] (_text <$> tip ^. snippets))
               ] `nodeVisible` ((T.length . T.unlines) (_text <$> tip ^. snippets) > 0)
      , spacer
      , hstack [ button "Back" GoToMainMenu `nodeKey` detailsBackButtonKey `styleBasic` [padding 5]
               , spacer
               , mainButton "Edit" (OpenEditTipForm tip) `styleBasic` [padding 5]
               , spacer
               , mainButton "Delete" (RemoveTip (tip ^. ts)) `styleBasic` [padding 5]
               ]
      ] `styleBasic` [padding 20]

withVimBindings :: TipsNode -> TipsNode
withVimBindings = keystroke vimKeys
  where vimKeys =
          [ ("C-n", MoveFocus FocusFwd)
          , ("C-p", MoveFocus FocusBwd)
          ]

listItem :: SnippetID -> Snippet -> TipsNode
listItem idx item = vstack [
    hstack [
        textField (activeTip . snippets . singular (ix idx) . text)
      , spacer
      , button "Delete" (RemoveSnippet idx)
    ]
  ] `nodeKey` showt (item ^. Model.id) `styleBasic` [paddingT 10]

-- TODO create one single "list" widget for displaying list of things
--      and binding custom actions to click events and such
listTips :: Color -> p -> Tip -> TipsNode
listTips rowBgColor id tip = vstack
  [ boxRow
      rowBgColor
      (ShowDetails tip)
      (hstack [ filler, label_ (tip ^. title) [ellipsis], filler ])
  ]
    `nodeKey` showt (tip ^. ts)

listSnippets :: Color -> SnippetID -> T.Text -> TipsNode
listSnippets rowBgColor id snippet = vstack
  [ boxRow
      rowBgColor
      (CopyToClipboard snippet)
      (hstack [ filler, label_ snippet [ellipsis], filler ])
  ]
    `nodeKey` T.pack (show id)

titleText :: T.Text -> TipsNode
titleText text = label text `styleBasic` [textFont "Medium", textSize 20, paddingV 10]

subtitleText :: T.Text -> TipsNode
subtitleText text = label text `styleBasic` [textFont "Regular", textSize 18]

boxRow :: (Typeable s, Typeable e) => Color -> e -> WidgetNode s e -> WidgetNode s e
boxRow rowBgColor clickAction content = box_ [onClick clickAction] content
  `styleBasic` [paddingV 10]
  `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]

fuzzyTips :: T.Text -> [Tip] -> [Tip]
fuzzyTips query tips = snd <$> fuzzyFindOn (T.unpack . _title) [T.unpack query] tips

handleEvent
  :: TipsWenv
  -> TipsNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  LoadTips ->
    [ Task $ SetTips <$> parseTips
    ]
  SaveTips ->
    [ Task $ SetTips <$> overwriteTipsFile (model ^. tips)
    ]
  SetTips ts ->
    [ Model $ model
        & searchBoxText .~ ""
        & tips .~ ts
        & currentScreen .~ MainMenu
        & activeTip .~ def
    , SetFocusOnKey tipSearchBoxKey
    ]
  ShowBestMatchingTip matchedTips -> case matchedTips of
    t : ts -> [ Model $ model & currentScreen .~ Details t
              , SetFocusOnKey detailsBackButtonKey
              ]
    _ -> []
  AddTip | isValidForm model ->
    [ Model $ model & tips .~ (newTip : (model ^. tips))
    , Event SaveTips
    ]
  EditTip id | isValidForm model ->
    [ Model $ model & tips .~ map (\t -> if t ^. ts == id then edit t else t) (model ^. tips)
    , Event SaveTips
    ]
  RemoveTip id ->
    [ Model $ model & tips .~ filter ((/= id) . (^. ts)) (model ^. tips)
    , Event SaveTips
    ]
  AddSnippet ->
    [ Model $ model
        & (activeTip . snippets) .~ ((model ^. activeTip . snippets) |> newSnippet)
    ]
  RemoveSnippet snippetId ->
    [ Model $ model
        & (activeTip . snippets) .~ removeIdx snippetId (model ^. activeTip . snippets)
    ]
  OpenNewTipForm ->
    [ Model $ model
        & currentScreen .~ NewTipForm
        & activeTip .~ def
    , SetFocusOnKey newTipTitleBoxKey
    ]
  OpenEditTipForm tip ->
    [ Model $ model
        & currentScreen .~ EditTipForm (tip ^. ts)
        & activeTip .~ tip
    , SetFocusOnKey editedTipTitleBoxKey
    ]
  CancelNewTip ->
    [ Model $ model
        & currentScreen .~ MainMenu
        & searchBoxText .~ ""
        & activeTip .~ def
    , SetFocusOnKey tipSearchBoxKey
    ]
  CancelEditTip ->
    [ Model $ model
       & currentScreen .~ MainMenu
       & searchBoxText .~ ""
       & activeTip .~ def
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
    [ MoveFocusFromKey Nothing focusDirection
    ]
  _ -> []
  where
    newTip :: Tip
    newTip = model ^. activeTip
      & ts .~ (wenv ^. L.timestamp)
      & snippets .~ filterNonEmpty (model ^. activeTip . snippets)

    newSnippet :: Snippet
    newSnippet = def (wenv ^. L.timestamp)

    edit :: Tip -> Tip
    edit tip = model ^. activeTip
      & snippets .~ filterNonEmpty (model ^. activeTip . snippets)

    removeIdx :: Int -> [a] -> [a]
    removeIdx idx lst = part1 ++ drop 1 part2 where
      (part1, part2) = splitAt idx lst

    filterNonEmpty :: [Snippet] -> [Snippet]
    filterNonEmpty = filter ((/= "") . (^. text))

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

isValidForm :: AppModel -> Bool
isValidForm model = notElem T.empty . map (model ^.) $ [activeTip . title, activeTip . content]

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
      , _activeTip = def
      }

