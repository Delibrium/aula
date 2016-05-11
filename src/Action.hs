{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Werror -Wall       #-}

-- | The 'Action' module contains an API which
module Action
    ( -- * constraint types
      ActionM
    , ActionLog(log, readEventLog)
    , ActionPersist(queryDb, query, equery, mquery, update), maybe404
    , ActionUserHandler(login, logout, userState, addMessage, flushMessages)
    , ActionRandomPassword(mkRandomPassword)
    , ActionCurrentTimestamp(getCurrentTimestamp)
    , ActionSendMail
    , ActionAddDb
    , ActionError
    , ActionExcept(..)
    , ActionEnv(..), envRunPersist, envConfig, envLogger
    , StatusMessage

      -- * user handling
    , loginByUser, loginByName
    , userLoggedOut
    , addWithUser
    , currentUserAddDb
    , currentUserAddDb_
    , currentUser
    , currentUserId
    , modifyCurrentUser
    , isLoggedIn
    , validUserState
    , validLoggedIn
    , getSpacesForCurrentUser
    , deleteUser

      -- * user state
    , UserState(..), usUserId, usCsrfToken, usSessionToken, usMessages

      -- * vote handling
    , likeIdea
    , voteIdea
    , voteIdeaComment
    , voteIdeaCommentReply
    , markIdeaInJuryPhase
    , markIdeaInResultPhase
    , removeVote
    , Action.setCreatorStatement
    , revokeWinnerStatusOfIdea

      -- * reporting and deleting comments
    , deleteIdeaComment
    , deleteIdeaCommentReply
    , reportIdeaComment
    , reportIdeaCommentReply

      -- * topic handling
    , topicInRefinementTimedOut
    , topicInVotingTimedOut

      -- * page handling
    , createTopic
    , Action.editTopic
    , createIdea
    , Action.editIdea

      -- * admin activity
    , topicForceNextPhase
    , topicInVotingResetToJury

      -- * extras
    , ReadTempFile(readTempFile), readTempCsvFile
    , CleanupTempFiles(cleanupTempFiles)
    , decodeCsv
    , ActionAvatar(readImageFile, savePngImageFile)

    , MonadServantErr, ThrowServantErr(..)

    , module Action.Smtp
    , sendMailToRole

    -- * moderator's event log (FIXME: this section should all be local to this module)
    , eventLogUserCreatesTopic
    , eventLogUserCreatesIdea
    , eventLogUserCreatesComment
    , eventLogUserEditsTopic
    , eventLogUserEditsIdea
    , eventLogUserEditsComment
    , eventLogUserMarksIdeaFeasible
    , eventLogUserVotesOnIdea
    , eventLogUserVotesOnComment
    , eventLogUserDelegates
    , eventLogIdeaNewTopic
    , eventLogIdeaReachesQuorum
    , WarmUp, warmUp
    )
where

import Codec.Picture (DynamicImage)
import Control.Exception (SomeException, assert)
import Control.Lens
import Control.Monad ((>=>), void, when)
import Control.Monad.Reader (runReader, runReaderT)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Except (runExcept)
import Data.Char (ord)
import Data.Maybe (isJust)
import Data.Monoid
import Data.String.Conversions (ST, LBS, cs)
import Data.Typeable (Typeable)
import Data.Foldable (forM_)
import Prelude hiding (log)
import Servant
import Servant.Missing
import Thentos.Frontend.CSRF (HasSessionCsrfToken(..), GetCsrfSecret(..), CsrfToken)
import Thentos.Types (GetThentosSessionToken(..), ThentosSessionToken)

import qualified Data.Csv as Csv
import qualified Data.Text as ST
import qualified Data.Vector as V

import Action.Smtp
import Config (Config, GetConfig(..), exposedUrl)
import Data.UriPath (absoluteUriPath, relPath)
import LifeCycle
import Logger
import Logger.EventLog
import Persistent
import Persistent.Api
import Types
import qualified Frontend.Path as U


-- * constraint types

type StatusMessage = ST

-- | User representation during an action
data UserState = UserState
    { _usSessionToken :: Maybe ThentosSessionToken
    , _usCsrfToken    :: Maybe CsrfToken
    , _usUserId       :: Maybe (AUID User)
    , _usMessages     :: [StatusMessage]
    }
  deriving (Show, Eq)

makeLenses ''UserState

userLoggedOut :: UserState
userLoggedOut = UserState Nothing Nothing Nothing []

data ActionEnv = ActionEnv
    { _envRunPersist :: RunPersist
    , _envConfig     :: Config
    , _envLogger     :: SendLogMsg
    }

makeLenses ''ActionEnv

instance GetCsrfSecret ActionEnv where
    csrfSecret = envConfig . csrfSecret

instance GetConfig ActionEnv where
    getConfig = envConfig

-- | Top level errors can happen.
--
-- FIXME: 'ServantErr' should be abstracted away.
data ActionExcept
    = ActionExcept { unActionExcept :: ServantErr }
    | ActionPersistExcept PersistExcept
    | ActionSendMailExcept SendMailError
    | ActionEventLogExcept SomeException
    | ActionIOExcept SomeException
    deriving (Show)

makePrisms ''ActionExcept

instance ThrowSendMailError ActionExcept where
    _SendMailError = _ActionSendMailExcept

type ActionSendMail = HasSendMail ActionExcept ActionEnv

type ActionAddDb m = (ActionUserHandler m, ActionPersist m, ActionCurrentTimestamp m)

type ActionM m =
      ( ActionLog m
      , ActionPersist m
      , ActionUserHandler m
      , ActionError m
      , ReadTempFile m
      , ActionAvatar m
      , CleanupTempFiles m
      , ActionRandomPassword m
      , ActionCurrentTimestamp m
      , ActionSendMail m
      )

class Monad m => ActionLog m where
    -- | Log system event
    log :: LogEntry -> m ()
    readEventLog :: m EventLog

-- | A monad that can run acid-state.
--
-- See 'Query', 'EQuery', 'AUpdate' in "Persistent.Pure" for more a deeper understanging of this.
class (MonadError ActionExcept m) => ActionPersist m where
    queryDb :: m AulaData
    update  :: HasAUpdate ev a => ev -> m a

    query :: Query a -> m a
    query q = runReader q <$> queryDb

    mquery :: Typeable a => MQuery a -> m a
    mquery q = equery (maybe404 =<< q)

    equery :: EQuery a -> m a
    equery q = do
        db <- queryDb
        either (throwError . ActionPersistExcept) pure $ runExcept (runReaderT q db)

class ActionRandomPassword m where
    mkRandomPassword :: m UserPass

class ActionCurrentTimestamp m where
    getCurrentTimestamp :: m Timestamp

instance HasSessionCsrfToken UserState where
    sessionCsrfToken = usCsrfToken

instance GetThentosSessionToken UserState where
    getThentosSessionToken = usSessionToken

instance ThrowError500 ActionExcept where
    error500 = _ServantErr . error500

class ActionError m => ActionUserHandler m where
    -- | Make the user logged in
    login  :: AUID User -> m ()
    -- | Read the current user state
    userState :: Getting a UserState a -> m a
    -- | Add ui messages
    addMessage :: StatusMessage -> m ()
    -- | Flush all ui messages
    flushMessages :: m [StatusMessage]
    -- | Make the user log out
    logout :: m ()

instance ThrowServantErr ActionExcept where
    _ServantErr = _ActionExcept

type ActionError m = (MonadError ActionExcept m)


-- * User Handling

checkActiveUser :: ActionUserHandler m => User -> m User
checkActiveUser user =
    if isDeletedUser user
        then logout >> throwError500 "Unknown user identitifer"
        else pure user

loginByUser :: ActionUserHandler m => User -> m ()
loginByUser = checkActiveUser >=> login . view _Id

loginByName :: (ActionPersist m, ActionUserHandler m) => UserLogin -> m ()
loginByName u = loginByUser =<< mquery (findUserByLogin u)

-- | Returns the current user ID
currentUserId :: ActionUserHandler m => m (AUID User)
currentUserId = userState usUserId >>= \case
    Nothing -> throwError500 "User is logged out"
    Just uid -> pure uid

addWithUser :: (HasAUpdate ev a, ActionPersist m, ActionCurrentTimestamp m) =>
               (EnvWithProto a -> ev) -> User -> Proto a -> m a
addWithUser addA user protoA = do
    now <- getCurrentTimestamp
    update $ addA (EnvWith user now protoA)

-- FIXME: rename @{currentUserAddDb,addWithCurrentUser}*@ for consistency with 'addWithUser'.

currentUserAddDb :: (HasAUpdate ev a, ActionAddDb m) => (EnvWithProto a -> ev) -> Proto a -> m a
currentUserAddDb addA protoA = do
    cUser <- currentUser
    addWithUser addA cUser protoA

currentUserAddDb_ :: (HasAUpdate ev a, ActionAddDb m) => (EnvWithProto a -> ev) -> Proto a -> m ()
currentUserAddDb_ addA protoA = void $ currentUserAddDb addA protoA

-- | Returns the current user
currentUser :: (ActionPersist m, ActionUserHandler m) => m User
currentUser = do
    uid <- currentUserId
    muser <- query (findUser uid)
    case muser of
        Just user | isActiveUser user
            -> pure user
        _   -> logout >> throwError500 "Unknown user identitifer"

-- | Modify the current user.
modifyCurrentUser :: (ActionPersist m, ActionUserHandler m, HasAUpdate ev a)
                  => (AUID User -> ev) -> m a
modifyCurrentUser ev =
    currentUser >>= checkActiveUser >>= update . ev . view _Id

isLoggedIn :: ActionUserHandler m => m Bool
isLoggedIn = userState $ to validLoggedIn

validLoggedIn :: UserState -> Bool
validLoggedIn us = isJust (us ^. usUserId) && isJust (us ^. usSessionToken)

validUserState :: UserState -> Bool
validUserState us = us == userLoggedOut || validLoggedIn us

getSpacesForCurrentUser :: (ActionUserHandler m, ActionPersist m) => m [IdeaSpace]
getSpacesForCurrentUser = do
    -- FIXME: remove the isLoggedIn check.
    b <- isLoggedIn
    if b then do
        user <- currentUser
        query $ getSpacesForRole (user ^. userRole)
    else
        pure []

-- FIXME: Authorization
deleteUser :: (ActionPersist m) => AUID User -> m ()
deleteUser = update . DeactivateUser


-- * Phase Transitions

topicPhaseChange :: (ActionM m) => Topic -> PhaseChange -> m ()
topicPhaseChange topic change = do
    case phaseTrans (topic ^. topicPhase) change of
        Nothing -> throwError500 "Invalid phase transition"
        Just (phase', actions) -> do
            update $ SetTopicPhase (topic ^. _Id) phase'
            mapM_ (phaseAction topic) actions

topicTimeout :: (ActionM m) => PhaseChange -> AUID Topic -> m ()
topicTimeout phaseChange tid = do
    topic <- mquery $ findTopic tid
    topicPhaseChange topic phaseChange

sendMailToRole :: (ActionPersist m, ActionSendMail m) => Role -> EmailMessage -> m ()
sendMailToRole role msg = do
    users <- query $ findUsersByRole role
    forM_ users $ \user ->
        sendMailToUser [IgnoreMissingEmails] user msg

phaseAction :: (ActionM m) => Topic -> PhaseAction -> m ()
phaseAction topic phasact = do
    cfg <- viewConfig
    let topicTemplate addr phase = ST.unlines
            [ "Liebe " <> addr <> ","
            , ""
            , "das Thema:"
            , ""
            , "    " <> topic ^. topicTitle  -- FIXME: sanity checking!
            , "    " <> (cfg ^. exposedUrl . csi)
                     <> (absoluteUriPath . relPath $ U.listTopicIdeas topic)
                -- FIXME: do we want to send urls by email?  phishing and all?
            , ""
            , "hat die " <> phase <> " erreicht und bedarf Ihrer Aufmerksamkeit."
            , ""
            , "hochachtungsvoll,"
            , "Ihr Aula-Benachrichtigungsdienst"
            ]

    case phasact of
      JuryPhasePrincipalEmail ->
          sendMailToRole Principal EmailMessage
              { _msgISpace  = topic ^. topicIdeaSpace
              , _msgSubject = "Thema in der Prüfungsphase"
              , _msgBody    = topicTemplate "Schulleitung" "Prüfungsphase"
              , _msgHtml    = Nothing -- Not supported yet
              }
      ResultPhaseModeratorEmail ->
          sendMailToRole Moderator EmailMessage
              { _msgISpace  = topic ^. topicIdeaSpace
              , _msgSubject = "Thema in der Ergebnisphase"
              , _msgBody    = topicTemplate "Moderatoren" "Ergebnisphase"
              , _msgHtml    = Nothing -- Not supported yet
              }
      UnmarkAllIdeas -> do
          ideas :: [Idea] <- query $ findIdeasByTopic topic
          (\idea -> unmarkIdeaInJuryPhase (idea ^. _Id)) `mapM_` ideas



-- * Page Handling

type Create  a = forall m. (ActionM m) => Proto a -> m a
type Create_ a = forall m. (ActionM m) => Proto a -> m ()

createTopic :: Create Topic
createTopic proto = do
    now <- getCurrentTimestamp
    topic <- currentUserAddDb (AddTopic now) proto
    eventLogUserCreatesTopic topic
    pure topic

editTopic :: ActionM m => AUID Topic -> EditTopicData -> m ()
editTopic topicId topic = do
    update $ EditTopic topicId topic
    eventLogUserEditsTopic =<< mquery (findTopic topicId)

createIdea :: Create Idea
createIdea proto = do
    idea <- currentUserAddDb AddIdea proto
    eventLogUserCreatesIdea idea
    pure idea

editIdea :: ActionM m => AUID Idea -> ProtoIdea -> m ()
editIdea ideaId idea = do
    update $ EditIdea ideaId idea
    eventLogUserEditsIdea =<< mquery (findIdea ideaId)


-- * Vote Handling

likeIdea :: ActionM m => AUID Idea -> m ()
likeIdea ideaId = do
    currentUserAddDb_ (AddLikeToIdea ideaId) ()
    do (idea, info) <- equery $ do
          ide <- maybe404 =<< findIdea ideaId
          inf <- getListInfoForIdea ide
          pure (ide, inf)
       when (ideaReachedQuorum info) $ eventLogIdeaReachesQuorum idea

voteIdea :: AUID Idea -> Create_ IdeaVote
voteIdea ideaId vote = do
    currentUserAddDb_ (AddVoteToIdea ideaId) vote
    (`eventLogUserVotesOnIdea` Just vote) =<< mquery (findIdea ideaId)

-- FIXME: make 'voteIdeaComment' and 'voteIdeaCommentReply' one function that takes a 'CommentKey'.

-- ASSUMPTION: Idea is in the given idea location.
voteIdeaComment :: IdeaLocation -> AUID Idea -> AUID Comment -> Create_ CommentVote
voteIdeaComment loc ideaId commentId vote = do
    let ck = CommentKey loc ideaId [] commentId
    currentUserAddDb_ (AddCommentVote ck) vote
    eventLogUserVotesOnComment ck vote

-- ASSUMPTION: Idea is in the given idea location.
voteIdeaCommentReply :: IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment -> Create_ CommentVote
voteIdeaCommentReply loc ideaId commentId =
    currentUserAddDb_ . AddCommentVote . CommentKey loc ideaId [commentId]

-- | FIXME: don't pass user as an explicit argument here.  do it like voteIdea.
removeVote :: (ActionM m) => AUID Idea -> AUID User -> m ()
removeVote ideaId user = do
    update $ RemoveVoteFromIdea ideaId user
    (`eventLogUserVotesOnIdea` Nothing) =<< mquery (findIdea ideaId)


-- * Reporting and deleting comments

-- ASSUMPTION: Idea is in the given idea location.
deleteIdeaComment :: IdeaLocation -> AUID Idea -> AUID Comment -> ActionPersist m => m ()
deleteIdeaComment loc ideaId = update . DeleteComment . CommentKey loc ideaId []

-- ASSUMPTION: Idea is in the given idea location.
deleteIdeaCommentReply :: IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment ->
                          ActionPersist m => m ()
deleteIdeaCommentReply loc ideaId commentId =
    update . DeleteComment . CommentKey loc ideaId [commentId]

-- FIXME:
-- More generally: do we do anything to prevent abuse of the report system?
-- One thing could be log the event or count the reports made by one user.
-- Since no record of the report are kept in base not only multiple users can
-- report the same comment but the same user can report multiple times.
reportCommentById :: CommentKey -> Document -> (ActionPersist m, ActionSendMail m) => m ()
reportCommentById ck doc = do
    comment <- mquery $ findComment ck
    let uri = relPath $ U.viewComment comment
    cfg <- viewConfig
    sendMailToRole Moderator EmailMessage
        { _msgISpace  = comment ^. _Key . ckIdeaLocation . ideaLocationSpace
        , _msgSubject = "Problematischer Verbesserungsvorschlag."
        , _msgBody = ST.unlines
            [ "Liebe Moderatoren,"
            , ""
            , "Ein Verbesserungsvorschlag wurde als problematisch gemeldet:"
            , ""
            , "    " <> (cfg ^. exposedUrl . csi) <> absoluteUriPath uri
                -- FIXME: do we want to send urls by email?  phishing and all?
            , ""
            , ""
            , cs $ unMarkdown doc
            , ""
            , "hochachtungsvoll,"
            , "Ihr Aula-Benachrichtigungsdienst"
            ]
        , _msgHtml = Nothing -- Not supported yet
        }

-- ASSUMPTION: Idea is in the given idea location.
reportIdeaComment
    :: IdeaLocation -> AUID Idea -> AUID Comment -> Document
    -> (ActionPersist m, ActionSendMail m) => m ()
reportIdeaComment loc ideaId commentId
    = reportCommentById (CommentKey loc ideaId [] commentId)

reportIdeaCommentReply
    :: IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment -> Document
    -> (ActionPersist m, ActionSendMail m) => m ()
reportIdeaCommentReply loc ideaId parentCommentId commentId
    = reportCommentById (CommentKey loc ideaId [parentCommentId] commentId)

getIdeaTopicInJuryPhase :: ActionM m => AUID Idea -> m Topic
getIdeaTopicInJuryPhase iid = do
    -- FIXME: should this be one transaction?
    idea  <- mquery $ findIdea iid
    topic <- mquery $ ideaTopic idea
    equery $ checkInPhase (PhaseJury ==) idea topic
    pure topic

-- | Mark idea as feasible if the idea is in the Jury phase, if not throws an exception.
-- It runs the phase change computations if happens.
-- FIXME: Authorization
-- FIXME: Compute value in one persistent computation
markIdeaInJuryPhase :: ActionM m => AUID Idea -> IdeaJuryResultValue -> m ()
markIdeaInJuryPhase iid rv = do
    topic <- getIdeaTopicInJuryPhase iid
    currentUserAddDb_ (AddIdeaJuryResult iid) rv
    eventLogUserMarksIdeaFeasible iid . Just $ ideaJuryResultValueToType rv
    checkCloseJuryPhase topic

unmarkIdeaInJuryPhase :: ActionM m => AUID Idea -> m ()
unmarkIdeaInJuryPhase iid = do
    void $ getIdeaTopicInJuryPhase iid
    eventLogUserMarksIdeaFeasible iid Nothing
    update $ RemoveIdeaJuryResult iid

checkCloseJuryPhase :: ActionM m => Topic -> m ()
checkCloseJuryPhase topic = do
    -- FIXME: should this be one transaction?  [~~mf] -- I think so, and the same above. I think an
    -- alternative is to check (in the operations above that modify the DB, internally, necessarily
    -- within a single transaction with the update) that the current values are as expected, and if
    -- not abort with an error like "the ideal is not in the expected phase".  [~~mk]
    allMarked <- query $ checkAllIdeasMarked topic
    when allMarked $ do
        days <- getCurrentTimestamp >>= \now -> query $ phaseEndVote now
        topicPhaseChange topic (AllIdeasAreMarked days)

-- | Mark idea as winner or not enough votes if the idea is in the Result phase,
-- if not throws an exception.
-- FIXME: Authorization
-- FIXME: Compute value in one persistent computation
-- FIXME: redundant code between this and 'markIdeaInJuryPhase'.
markIdeaInResultPhase :: ActionM m => AUID Idea -> IdeaVoteResultValue -> m ()
markIdeaInResultPhase iid rv = do
    idea  <- mquery $ findIdea iid
    topic <- mquery $ ideaTopic idea
    equery $ checkInPhase (PhaseResult ==) idea topic
    currentUserAddDb_ (AddIdeaVoteResult iid) rv

setCreatorStatement :: ActionM m => AUID Idea -> Document -> m ()
setCreatorStatement = update <..> SetCreatorStatement

revokeWinnerStatusOfIdea :: ActionM m => AUID Idea -> m ()
revokeWinnerStatusOfIdea = update . RevokeWinnerStatus


-- * Topic handling

topicInRefinementTimedOut :: (ActionM m) => AUID Topic -> m ()
topicInRefinementTimedOut tid = do
    topic  <- mquery $ findTopic tid
    topicTimeout RefinementPhaseTimeOut tid
    topic' <- mquery $ findTopic tid
    eventLogTopicNewPhase topic (topic ^. topicPhase) (topic' ^. topicPhase)

topicInVotingTimedOut :: (ActionM m) => AUID Topic -> m ()
topicInVotingTimedOut = topicTimeout VotingPhaseTimeOut

topicInVotingResetToJury :: (ActionM m) => AUID Topic -> m ()
topicInVotingResetToJury tid = do
    topic <- mquery $ findTopic tid
    case topic ^. topicPhase of
        PhaseVoting _ -> topicPhaseChange topic VotingPhaseSetbackToJuryPhase
        _             -> pure ()


-- * Admin activities

-- | Make a topic timeout if the timeout is applicable.
-- FIXME: Only admin can do that
topicForceNextPhase :: (ActionM m) => AUID Topic -> m ()
topicForceNextPhase tid = do
    topic <- mquery $ findTopic tid
    when (topic ^. topicPhase . to isPhaseFrozen) $
        throwError500 "Cannot transition from a frozen phase"
    case topic ^. topicPhase of
        PhaseWildIdea{}   -> throwError500 "Cannot force-transition from the wild idea phase"
        PhaseRefinement{} -> topicInRefinementTimedOut tid
        PhaseJury         -> makeEverythingFeasible topic
        PhaseVoting{}     -> topicInVotingTimedOut tid
        PhaseResult       -> throwError500 "No phase after result phase!"
  where
    makeEverythingFeasible topic = do
        ideas :: [Idea] <- query $ findIdeasByTopic topic
        (\idea -> markIdeaInJuryPhase (idea ^. _Id) (Feasible Nothing)) `mapM_` ideas


-- * files

class Monad m => ActionAvatar m where
    readImageFile :: FilePath -> m (Either String DynamicImage)
    savePngImageFile :: FilePath -> DynamicImage -> m ()

class Monad m => ReadTempFile m where
    readTempFile :: FilePath -> m LBS

class Monad m => CleanupTempFiles m where
    cleanupTempFiles :: FormData -> m ()

readTempCsvFile :: (ReadTempFile m, Csv.FromRecord r) => FilePath -> m (Either String [r])
readTempCsvFile = fmap decodeCsv . readTempFile

decodeCsv :: Csv.FromRecord r => LBS -> Either String [r]
decodeCsv = fmap V.toList . Csv.decodeWith opts Csv.HasHeader
  where
    opts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }


-- * moderator's event log

eventLogUserCreatesTopic :: (ActionCurrentTimestamp m, ActionLog m) => Topic -> m ()
eventLogUserCreatesTopic topic = do
    eventLog (topic ^. topicIdeaSpace) (topic ^. createdBy) $
        EventLogUserCreates (Left3 $ topic ^. _Key)

eventLogUserCreatesIdea :: (ActionCurrentTimestamp m, ActionLog m) => Idea -> m ()
eventLogUserCreatesIdea idea = do
    eventLog (idea ^. ideaLocation . ideaLocationSpace) (idea ^. createdBy) $
        EventLogUserCreates (Middle3 $ idea ^. _Key)

eventLogUserCreatesComment :: (ActionCurrentTimestamp m, ActionLog m) => Comment -> m ()
eventLogUserCreatesComment comment = do
    eventLog (comment ^. _Key . ckIdeaLocation . ideaLocationSpace) (comment ^. createdBy) $
        EventLogUserCreates (Right3 $ comment ^. _Key)

eventLogUserEditsTopic :: (ActionCurrentTimestamp m, ActionLog m) => Topic -> m ()
eventLogUserEditsTopic topic = do
    eventLog (topic ^. topicIdeaSpace) (topic ^. createdBy) $
            -- FIXME: 'createdBy' should be 'lastChangedBy' or 'currentUser', but given the current
            -- authorization policy this works as well.  See also: 'eventLogUserEditsIdea',
            -- 'eventLogUserEditsComment'.
        EventLogUserEdits (Left3 $ topic ^. _Key)

eventLogUserEditsIdea :: (ActionCurrentTimestamp m, ActionLog m) => Idea -> m ()
eventLogUserEditsIdea idea = do
    eventLog (idea ^. ideaLocation . ideaLocationSpace) (idea ^. createdBy) $
        EventLogUserEdits (Middle3 $ idea ^. _Key)

eventLogUserEditsComment :: (ActionCurrentTimestamp m, ActionLog m) => Comment -> m ()
eventLogUserEditsComment comment = do
    eventLog (comment ^. _Key . ckIdeaLocation . ideaLocationSpace) (comment ^. createdBy) $
        EventLogUserEdits (Right3 $ comment ^. _Key)

eventLogUserMarksIdeaFeasible ::
      (ActionUserHandler m, ActionPersist m, ActionCurrentTimestamp m, ActionLog m)
      => AUID Idea -> Maybe IdeaJuryResultType -> m ()
eventLogUserMarksIdeaFeasible iid jrt = do
    uid <- currentUserId
    idea <- mquery $ findIdea iid
    eventLog (idea ^. ideaLocation . ideaLocationSpace) uid $
        EventLogUserMarksIdeaFeasible iid jrt

eventLogUserVotesOnIdea ::
      (ActionUserHandler m, ActionCurrentTimestamp m, ActionLog m)
      => Idea -> Maybe IdeaVoteValue -> m ()
eventLogUserVotesOnIdea idea v = do
    uid <- currentUserId
    eventLog (idea ^. ideaLocation . ideaLocationSpace) uid $
        EventLogUserVotesOnIdea (idea ^. _Key) v

eventLogUserVotesOnComment ::
      (ActionUserHandler m, ActionCurrentTimestamp m, ActionPersist m, ActionLog m)
      => KeyOf Comment -> UpDown -> m ()
eventLogUserVotesOnComment ck@(CommentKey _ ideaId parentIds _) v = do
    uid <- currentUserId
    idea <- mquery $ findIdea ideaId
    (comment, mcomment) <- do
        child :: Comment <- mquery $ findComment ck
        case parentIds of
            []    -> pure (child, Nothing)
            [pid] -> do
                parent <- mquery $ findComment' ideaId [] pid
                pure (parent, Just child)
            _     -> assert False $ error "eventLogUserVotesOnComment: too many parents."
    eventLog (idea ^. ideaLocation . ideaLocationSpace) uid $
        EventLogUserVotesOnComment (idea ^. _Key) (comment ^. _Key) (view _Key <$> mcomment) v

-- FIXME: throw this in all applicable situations.
eventLogUserDelegates ::
      (ActionUserHandler m, ActionPersist m, ActionCurrentTimestamp m, ActionLog m)
      => DelegationContext -> User -> m ()
eventLogUserDelegates ctx toUser = do
    fromUser <- currentUser
    ispace <- case ctx of
        DlgCtxGlobal           -> pure . ClassSpace $ fromUser ^?! userRole . roleSchoolClass
        DlgCtxIdeaSpace ispace -> pure ispace
        DlgCtxTopicId   tid    -> view topicIdeaSpace <$> mquery (findTopic tid)
        DlgCtxIdeaId    iid    -> view (ideaLocation . ideaLocationSpace)
                                  <$> mquery (findIdea iid)
    eventLog ispace (fromUser ^. _Key) $ EventLogUserDelegates ctx (toUser ^. _Key)

-- FIXME: throw this in all applicable situations.
eventLogTopicNewPhase :: (ActionCurrentTimestamp m, ActionLog m) => Topic -> Phase -> Phase -> m ()
eventLogTopicNewPhase topic fromPhase toPhase =
    eventLog (topic ^. topicIdeaSpace) (topic ^. createdBy) $
            -- FIXME: the triggering user should not always be the creator of the topic.
        EventLogTopicNewPhase (topic ^. _Id) fromPhase toPhase

-- FIXME: throw this in all applicable situations.
eventLogIdeaNewTopic :: (ActionUserHandler m, ActionCurrentTimestamp m, ActionLog m)
      => Idea -> Maybe Topic -> Maybe Topic -> m ()
eventLogIdeaNewTopic idea mfrom mto = do
    uid <- currentUserId
    eventLog (idea ^. ideaLocation . ideaLocationSpace) uid $
        EventLogIdeaNewTopic (idea ^. _Key) (view _Key <$> mfrom) (view _Key <$> mto)

-- FIXME: throw this in all applicable situations.
eventLogIdeaReachesQuorum :: (ActionCurrentTimestamp m, ActionLog m) => Idea -> m ()
eventLogIdeaReachesQuorum idea = do
    eventLog (idea ^. ideaLocation . ideaLocationSpace) (idea ^. createdBy) $
        EventLogIdeaReachesQuorum (idea ^. _Key)


eventLog :: (ActionCurrentTimestamp m, ActionLog m)
    => IdeaSpace -> AUID User -> EventLogItemValueCold -> m ()
eventLog ispace uid value = do
    now    <- getCurrentTimestamp
    log . LogEntryForModerator $ EventLogItem ispace now uid value


class WarmUp m cold warm where
    warmUp :: cold -> m warm

instance ActionM m => WarmUp m EventLogItemCold EventLogItemWarm where
    warmUp (EventLogItem ispace tstamp usr val) =
        EventLogItem ispace tstamp <$> warmUp' usr <*> warmUp val

instance ActionM m => WarmUp m EventLogItemValueCold EventLogItemValueWarm where
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
        EventLogTopicNewPhase t p1 p2
            -> do t' <- warmUp' t; pure $ EventLogTopicNewPhase t' p1 p2
        EventLogIdeaNewTopic i mt1 mt2
            -> do i' <- warmUp' i; mt1' <- mapM warmUp' mt1; mt2' <- mapM warmUp' mt2;
                  pure $ EventLogIdeaNewTopic i' mt1' mt2'
        EventLogIdeaReachesQuorum i
            -> EventLogIdeaReachesQuorum <$> warmUp' i


instance ActionM m => WarmUp m ContentCold ContentWarm where
    warmUp = \case
        Left3   t -> Left3   <$> warmUp' t
        Middle3 t -> Middle3 <$> warmUp' t
        Right3  t -> Right3  <$> warmUp' t

-- | for internal use only.
class WarmUp' m a where
    warmUp' :: KeyOf a -> m a

instance ActionM m => WarmUp' m User where
    warmUp' k = mquery (findUser k)

instance ActionM m => WarmUp' m Topic where
    warmUp' k = mquery (findTopic k)

instance ActionM m => WarmUp' m Idea where
    warmUp' k = mquery (findIdea k)

instance ActionM m => WarmUp' m Comment where
    warmUp' k = mquery (findComment k)
