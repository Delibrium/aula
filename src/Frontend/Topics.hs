{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Topics
where

import Control.Lens
import Control.Monad
import Data.Foldable (for_)
import Data.String.Conversions
import Prelude
import Lucid hiding (for_)

import Types
import Frontend.Core


-- | 4 Topic overview
data PageTopicOverview
  = PageTopicOverviewRefinementPhase' PageTopicOverviewRefinementPhase
  | PageTopicOverviewJuryPhase'       PageTopicOverviewJuryPhase
  | PageTopicOverviewVotingPhase'     PageTopicOverviewVotingPhase
  | PageTopicOverviewResultPhase'     PageTopicOverviewResultPhase
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverview where
    toHtmlRaw = toHtml
    toHtml = \case
      PageTopicOverviewRefinementPhase' p -> toHtml p
      PageTopicOverviewJuryPhase'       p -> toHtml p
      PageTopicOverviewVotingPhase'     p -> toHtml p
      PageTopicOverviewResultPhase'     p -> toHtml p

data TabTopicOverview
  = TabAllIdeas
  | TabVotingIdeas
  | TabWinningIdeas
  | TabDelegation
  deriving (Eq, Show, Read)

tabSelected :: Eq tab => tab -> tab -> ST
tabSelected curTab targetTab
    | curTab == targetTab = "tab-selected"
    | otherwise           = "tab-not-selected"

tabLink :: Monad m => TabTopicOverview -> TabTopicOverview -> HtmlT m ()
tabLink curTab targetTab =
  case targetTab of
    TabAllIdeas     -> a_ [id_ "tab-ideas",       attr] "Alle Ideen"
    TabVotingIdeas  -> a_ [id_ "tab-voting",      attr] "Ideen in der Abstimmung"
    TabWinningIdeas -> a_ [id_ "tab-winning",     attr] "Gewinner"
    TabDelegation   -> a_ [id_ "tab-delegations", attr] "Beauftragen Stimmen"
  where
    attr = class_ $ tabSelected curTab targetTab

pageTopicOverview :: Monad m => TabTopicOverview -> Topic -> [Idea] -> HtmlT m ()
pageTopicOverview tab topic ideas = do
    div_ $ do
        div_ [id_ "navigation"] $ do
            a_ [id_ "back-themes"] "<- Zu Allen Themen"
            a_ $ span_ [id_ "pen"] ":pen:" <> " bearbeiten"
        h2_ . toHtml $ phaseName phase
        div_ $ do
            p_   [id_ "topic-title"] $ topic ^. topicTitle . html
            div_ [id_ "topic-desc"] $ topic ^. topicDesc . html
            when (phase == PhaseRefinement) $
                a_   [id_ "add-idea"] "+ Neue Idee"
            when (phase < PhaseResult) $
                a_  [id_ "delegate-vote"] $ span_ [id_ "bullhorn"] ":bullhorn:" <> " Stimme Beauftragen"
        div_ [id_ "tabs"] $ do
            tabLink tab TabAllIdeas
            when (phase >= PhaseVoting) $ tabLink tab TabVotingIdeas
            when (phase >= PhaseResult) $ tabLink tab TabWinningIdeas
            tabLink tab TabDelegation
    div_ $ do
        a_ [id_ "settings"] $ span_ [id_ "gear"] ":gear:"
        div_ [id_ "ideas"] . for_ ideas $ \idea ->
            ListItemIdea (Just phase) idea ^. html
  where
    phase = topic ^. topicPhase

-- | 4.1 Topic overview: Refinement phase
data PageTopicOverviewRefinementPhase = PageTopicOverviewRefinementPhase Topic [Idea]
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverviewRefinementPhase where
    toHtmlRaw = toHtml
    toHtml p@(PageTopicOverviewRefinementPhase topic ideas) =
        -- FIXME: assert topicPhase is PhaseRefinement
        semanticDiv p $ pageTopicOverview TabAllIdeas topic ideas


-- | 4.2 Topic overview: Jury (assessment) phase
data PageTopicOverviewJuryPhase = PageTopicOverviewJuryPhase Topic [Idea]
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverviewJuryPhase where
    toHtmlRaw = toHtml
    toHtml p@(PageTopicOverviewJuryPhase topic ideas) =
        -- FIXME: assert topicPhase is PhaseJury
        semanticDiv p $ pageTopicOverview TabAllIdeas topic ideas


-- | 4.3 Topic overview: Voting phase
data PageTopicOverviewVotingPhase = PageTopicOverviewVotingPhase Topic [Idea]
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverviewVotingPhase where
    toHtmlRaw = toHtml
    toHtml p@(PageTopicOverviewVotingPhase topic ideas) =
        -- FIXME: assert topicPhase is PhaseVoting
        semanticDiv p $ pageTopicOverview TabAllIdeas topic ideas


-- | 4.4 Topic overview: Result phase
data PageTopicOverviewResultPhase = PageTopicOverviewResultPhase Topic [Idea]
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverviewResultPhase where
    toHtmlRaw = toHtml
    toHtml p@(PageTopicOverviewResultPhase topic ideas) =
        -- FIXME: assert topicPhase is PhaseResult
        semanticDiv p $ pageTopicOverview TabAllIdeas topic ideas


-- | 4.5 Topic overview: Delegations
data PageTopicOverviewDelegations = PageTopicOverviewDelegations
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverviewDelegations where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageTopicOverviewDelegations"
