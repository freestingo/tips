{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module Model where

import GHC.Generics
import Control.Lens
import Data.Text
import Data.Aeson

import Monomer
import Data.Default

data Screen
  = MainMenu
  | NewTipForm
  | EditTipForm TipID
  | Details Tip
  deriving (Eq, Show)

type TipID = Int
type SnippetID = Int

type FormTitle = Text
type TitleNodeKey = Text
type TitleField = ALens' AppModel Text
type ContentField = ALens' AppModel Text
type SnippetsField = Lens' AppModel [Snippet]

type TipsWenv = WidgetEnv AppModel AppEvent
type TipsNode = WidgetNode AppModel AppEvent

newtype ActionOnSuccess = OnSuccess AppEvent
newtype ActionOnCancel = OnCancel AppEvent

data Snippet = Snippet
  { _id :: SnippetID
  , _text :: Text
  } deriving (Eq, Show, Generic)

instance Default Snippet where
  def = Snippet
    { _id = 0
    , _text = ""
    }

data Tip = Tip
  { _ts :: TipID
  , _title :: Text
  , _content :: Text
  , _snippets :: [Snippet]
  } deriving (Eq, Show, Generic)

instance Default Tip where
  def = Tip
    { _ts = 0
    , _title = ""
    , _content = ""
    , _snippets = []
    }

instance FromJSON Snippet
instance ToJSON Snippet
instance FromJSON Tip
instance ToJSON Tip

data AppModel = AppModel
  { _tips :: [Tip]
  , _currentScreen :: Screen
  , _activeTip :: Tip
  , _searchBoxText :: Text
  } deriving (Eq, Show)

data AppEvent
  = LoadTips
  | SetTips [Tip]
  | SaveTips
  | AddTip
  | EditTip TipID
  | ShowBestMatchingTip [Tip]
  | RemoveTip TipID
  | AddSnippet
  | RemoveSnippet SnippetID
  | CancelNewTip
  | CancelEditTip
  | OpenNewTipForm
  | OpenEditTipForm Tip
  | GoToMainMenu
  | ShowDetails Tip
  | CopyToClipboard Text
  | MoveFocus FocusDirection

makeLenses 'Snippet
makeLenses 'Tip
makeLenses 'AppModel

