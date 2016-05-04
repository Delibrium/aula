{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE Rank2Types                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module EventLog
where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.String.Conversions
import GHC.Generics (Generic)
import Servant

import qualified Data.Csv as CSV
import qualified Data.Text as ST
import qualified Generics.SOP as SOP

import Action
import Action.Implementation
import Data.UriPath
import Frontend.Path as U
import Persistent
import Types


-- * types

data EventLog = EventLog URL [EventLogItemWarm]
  deriving (Generic)

data EventLogItem' user topic idea comment =
    EventLogItem' IdeaSpace Timestamp user (EventLogItemValue' user topic idea comment)
  deriving (Generic)

data EventLogItemValue' user topic idea comment =
    EventLogUserCreates           (Either3 topic idea comment)
  | EventLogUserEdits             (Either3 topic idea comment)
  | EventLogUserMarksIdeaFeasible idea IdeaJuryResultType
  | EventLogUserVotesOnIdea       idea IdeaVoteValue
  | EventLogUserVotesOnComment    idea comment (Maybe comment) UpDown
  | EventLogUserDelegates         ST user
  | EventLogTopicNewPhase         topic Phase Phase PhaseTransitionTriggeredBy
  | EventLogIdeaNewTopic          idea (Maybe topic) (Maybe topic)
  | EventLogIdeaReachesQuorum     idea
  deriving (Generic)

data PhaseTransitionTriggeredBy =
    PhaseTransitionTriggeredBy User
  | PhaseTransitionTriggeredByTimeout
  | PhaseTransitionTriggeredByAllIdeasMarked
  deriving (Eq, Ord, Show, Read, Generic)


type EventLogItemCold = EventLogItem' (AUID User) (AUID Topic) (AUID Idea) CommentKey
type EventLogItemWarm = EventLogItem' User Topic Idea Comment

type EventLogItemValueCold = EventLogItemValue' (AUID User) (AUID Topic) (AUID Idea) CommentKey
type EventLogItemValueWarm = EventLogItemValue' User Topic Idea Comment


instance SOP.Generic EventLog
instance SOP.Generic (EventLogItem' u t i c)
instance SOP.Generic (EventLogItemValue' u t i c)
instance SOP.Generic PhaseTransitionTriggeredBy


instance HasUILabel PhaseTransitionTriggeredBy where
    uilabel = \case
        (PhaseTransitionTriggeredBy _)           -> "von Hand ausgelöst"
        PhaseTransitionTriggeredByTimeout        -> "Zeit ist abgelaufen"
        PhaseTransitionTriggeredByAllIdeasMarked -> "alle Ideen sind geprüft"


-- * flatten after de-serialization

class WarmUp m cold warm where
    warmUp :: cold -> m warm

instance WarmUp Action EventLogItemCold EventLogItemWarm where
    warmUp (EventLogItem' ispace tstamp usr val) =
        EventLogItem' ispace tstamp <$> warmUp' usr <*> warmUp val

instance WarmUp Action EventLogItemValueCold EventLogItemValueWarm where
    warmUp = \case
        EventLogUserCreates c
            -> EventLogUserCreates <$> warmUp c
        EventLogUserEdits c
            -> EventLogUserCreates <$> warmUp c
        EventLogUserMarksIdeaFeasible i t
            -> do i' <- warmUp' i; pure $ EventLogUserMarksIdeaFeasible i' t
        EventLogUserVotesOnIdea i v
            -> do i' <- warmUp' i; pure $ EventLogUserVotesOnIdea i' v
        EventLogUserVotesOnComment i c mc ud
            -> do i' <- warmUp' i; c' <- warmUp' c; mc' <- mapM warmUp' mc;
                  pure $ EventLogUserVotesOnComment i' c' mc' ud
        EventLogUserDelegates s u
            -> EventLogUserDelegates s <$> warmUp' u
        EventLogTopicNewPhase t p1 p2 tb
            -> do t' <- warmUp' t; pure $ EventLogTopicNewPhase t' p1 p2 tb
        EventLogIdeaNewTopic i mt1 mt2
            -> do i' <- warmUp' i; mt1' <- mapM warmUp' mt1; mt2' <- mapM warmUp' mt2;
                  pure $ EventLogIdeaNewTopic i' mt1' mt2'
        EventLogIdeaReachesQuorum i
            -> EventLogIdeaReachesQuorum <$> warmUp' i


type ContentCold = Either3 (AUID Topic) (AUID Idea) CommentKey
type ContentWarm = Either3 Topic Idea Comment

instance WarmUp Action ContentCold ContentWarm where
    warmUp = \case
        Left3   t -> Left3   <$> warmUp' t
        Middle3 t -> Middle3 <$> warmUp' t
        Right3  t -> Right3  <$> warmUp' t

-- | for internal use only.
class WarmUp' m a where
    warmUp' :: KeyOf a -> m a

instance WarmUp' Action User where
    warmUp' k = equery (maybe404 =<< findUser k)

instance WarmUp' Action Topic where
    warmUp' k = equery (maybe404 =<< findTopic k)

instance WarmUp' Action Idea where
    warmUp' k = equery (maybe404 =<< findIdea k)

instance WarmUp' Action Comment where
    warmUp' k = equery (maybe404 =<< findComment k)


-- * delivering the event log

filterEventLog :: Maybe IdeaSpace -> EventLog -> EventLog
filterEventLog mspc (EventLog domainUrl rows) = EventLog domainUrl $ filter f rows
  where
    f (EventLogItem' spc' _ _ _) = maybe True (== spc') mspc


eventLogItemCsvHeaders :: [String]
eventLogItemCsvHeaders = ["Ideenraum", "Zeitstempel", "Login", "Event", "Link"]


data WithURL a = WithURL URL a

instance MimeRender CSV EventLog where
    mimeRender Proxy (EventLog _ []) = "[Keine Daten]"
    mimeRender Proxy (EventLog domainUrl rows) =
        cs (intercalate "," eventLogItemCsvHeaders <> "\n")
        <> CSV.encode (WithURL domainUrl <$> rows)

instance CSV.ToRecord (WithURL EventLogItemWarm) where
    toRecord (WithURL domainUrl (EventLogItem' ispace timestamp user ev)) = CSV.toRecord
        [ showIdeaSpace ispace
        , showTimestamp timestamp
        , user ^. userLogin . unUserLogin . csi
        ] <> f ev
      where
        objDesc :: ContentWarm -> ST
        objDesc (Left3   t) = "Thema " <> t ^. topicTitle . showed . csi
        objDesc (Middle3 i) = "Idee "  <> i ^. ideaTitle  . showed . csi
        objDesc (Right3  c) =
            chop $ "Verbesserungsvorschlag " <> (c ^. commentText . _Markdown . csi)

        chop :: ST -> ST
        chop s = if ST.length s <= 60 then s else ST.take 57 s <> "..."

        objLink :: ContentWarm -> ST
        objLink = (domainUrl <>) . absoluteUriPath . relPath . objLink'

        objLink' :: ContentWarm -> U.Main
        objLink' (Left3   t) = U.listTopicIdeas t
        objLink' (Middle3 i) = U.IdeaPath (i ^. ideaLocation) (U.ViewIdea (i ^. _Id) Nothing)
        objLink' (Right3  c) = U.IdeaPath iloc (U.ViewIdea iid (Just $ c ^. _Id))
          where
            iloc = c ^. _Key . ckIdeaLocation
            iid = c ^. _Key . ckIdeaId

        f (EventLogUserCreates obj) = CSV.toRecord
            [ "legt " <> objDesc obj <> " an.", objLink obj ]

        f (EventLogUserEdits obj) = CSV.toRecord
            [ "bearbeitet " <> objDesc obj <> ".", objLink obj ]

        f (EventLogUserMarksIdeaFeasible (Middle3 -> idea) isFeasible) = CSV.toRecord
            [ "bewertet Idee als " <> what, objLink idea ]
          where
            what = case isFeasible of
                     IdeaFeasible    -> "durchführbar."
                     IdeaNotFeasible -> "nicht durchführbar."

        f (EventLogUserVotesOnIdea (Middle3 -> idea) voteValue) = CSV.toRecord
            [ "stimmt " <> how <> " " <> objDesc idea <> ".", objLink idea ]
          where
            how = case voteValue of
                    Yes     -> "für"
                    No      -> "gegen"

        f (EventLogUserVotesOnComment (Middle3 -> idea) comment mcomment updown) = CSV.toRecord
            [ "stimmt " <> how <> " " <> what <> ".", objLink idea ]
          where
            how = case updown of
                    Up   -> "für"
                    Down -> "gegen"
            what = objDesc (Right3 $ fromMaybe comment mcomment)

        f (EventLogUserDelegates ctxDesc toUser) = CSV.toRecord
            [ "delegiert in " <> ctxDesc <> " an " <> toUser ^. userLogin . _UserLogin . csi
            , "(kein Link verfügbar)"
            ]

        f (EventLogTopicNewPhase (Left3 -> topic) fromPhase toPhase trigger) = CSV.toRecord
            [ objDesc topic <> " geht von " <> uilabel fromPhase
                            <> " nach "     <> uilabel toPhase
                            <> " ("         <> uilabel trigger <> ")"
            , objLink topic
            ]

        f (EventLogIdeaNewTopic (Middle3 -> idea) mt1 mt2) = CSV.toRecord
            [ "verschiebt " <> objDesc idea <> " von " <> show_ mt1 <> " nach " <> show_ mt2 <> "."
            , objLink idea
            ]
          where
            show_ :: Maybe Topic -> ST
            show_ mt = maybe "wilde Ideen" (view topicTitle) mt ^. showed . csi

        f (EventLogIdeaReachesQuorum (Middle3 -> idea)) = CSV.toRecord
            [ objDesc idea <> " erreicht das Quorum.", objLink idea ]
