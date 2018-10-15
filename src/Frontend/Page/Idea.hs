{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Idea
  ( ViewIdea(..), viNow, viCtx, viStats
  , CreateIdea(..), ciCtx, ciLoc
  , EditIdea(..), eiCtx, eiIdea
  , MoveIdea(..), miCtx, miIdea, miTopicChoices
  , ReportIdea(..), riCtx, riIdea
  , CommentOnIdea(..), coiCtx, coiIdea, coiComment
  , EditComment(..), ecCtx, ecIdea, ecComment
  , JudgeIdea(..), jiCtx, jiResult, jiIdea, jiTopic
  , CreatorStatement(..), csCtx, csIdea
  , ReportComment(..), rcCtx, rcComment
  , ReportCommentContent(..)
  , viewIdea
  , createIdea
  , editIdea
  , moveIdea
  , Frontend.Page.Idea.reportIdea
  , commentOnIdea
  , replyToComment
  , editComment
  , editReply
  , judgeIdea
  , creatorStatement
  , reportComment
  , reportReply
  )
where

import Access
import Action ( ActionM, ActionPersist, ActionUserHandler, ActionExcept, ActionLog
              , addWithCurrentUser, equery, mquery, update
              , locationCapCtx, ideaCapCtx, ideaCapCtx', commentCapCtx, commentCapCtx'
              , markIdeaInJuryPhase
              , setCreatorStatement
              , reportIdeaComment, reportIdeaCommentReply
              , eventLogUserCreatesComment, eventLogUserEditsComment
              , reportIdea
              , ActionCurrentTimestamp, getCurrentTimestamp
              )
import Control.Arrow ((&&&))
import Frontend.Fragment.Category
import Frontend.Fragment.Comment
import Frontend.Fragment.ContextMenu
import Frontend.Fragment.Note
import Frontend.Fragment.PhaseTime (displayPhaseWithTime)
import Frontend.Fragment.VotesBar
import Frontend.Prelude hiding ((<|>), MoveIdea)
import Frontend.Validation
import Persistent.Api
    ( AddCommentToIdea(AddCommentToIdea)
    , AddReply(AddReply)
    , SetCommentDesc(SetCommentDesc)
    )
import Persistent.Idiom
    ( IdeaStats(IdeaStats)
    )
import Persistent
    ( findComment
    , findTopicsBySpace
    , getIdeaStats
    , ideaReachedQuorum
    , moveIdeaToLocation
    )

import qualified Action (createIdea, editIdea, moveIdeaToTopic)
import qualified Data.Map as Map
import qualified Frontend.Path as U
import qualified Generics.SOP as SOP
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.View as DF
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Types (MoveIdea)
import qualified Lucid


-- * types

-- | 5 Idea detail page
-- This includes the pages 5.1 to 5.7 excluding 5.5 (PageIdeaDetailMoveIdeaToTopic) which needs its
-- own endpoint.
--
-- * 5.1 Idea detail page: New ideas
-- * 5.2 Idea detail page: Refinement phase
-- * 5.3 Idea detail page: Jury (assessment) phase
-- * 5.4 Idea detail page: Voting phase
-- * 5.6 Idea detail page: Feasible / not feasible
-- * 5.7 Idea detail page: Winner
data ViewIdea = ViewIdea
    { _viNow :: Timestamp
    , _viCtx :: CapCtx
    , _viStats :: IdeaStats
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic ViewIdea

makeLenses ''ViewIdea

instance Page ViewIdea where
    isAuthorized = authNeedCaps [CanView] viCtx

data ViewDeletedIdea = ViewDeletedIdea Idea
  deriving (Eq, Show, Read)

-- | 6. Create idea
data CreateIdea = CreateIdea { _ciCtx :: CapCtx, _ciLoc :: IdeaLocation }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic CreateIdea

makeLenses ''CreateIdea

instance Page CreateIdea where
    isAuthorized = authNeedCaps [CanCreateIdea] ciCtx

-- | 7. Edit idea
data EditIdea = EditIdea { _eiCtx :: CapCtx, _eiIdea :: Idea }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic EditIdea

makeLenses ''EditIdea

instance Page EditIdea where
    isAuthorized = authNeedCaps [CanEditAndDeleteIdea] eiCtx

-- | X. Move idea
-- Move idea to a topic.
data MoveIdea = MoveIdea { _miCtx :: CapCtx, _miIdea :: Idea, _miTopicChoices :: [Topic] }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic MoveIdea

makeLenses ''MoveIdea

instance Page MoveIdea where
    isAuthorized = authNeedCaps [CanMoveBetweenLocations] miCtx

data ReportIdea = ReportIdea { _riCtx :: CapCtx, _riIdea :: Idea }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic ReportIdea

makeLenses ''ReportIdea

instance Page ReportIdea where
    -- You can report iff you can view the idea.
    isAuthorized = authNeedCaps [CanView] riCtx

-- | X. Comment idea
data CommentOnIdea = CommentOnIdea
    { _coiCtx :: CapCtx, _coiIdea :: Idea, _coiComment :: Maybe Comment }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic CommentOnIdea

makeLenses ''CommentOnIdea

instance Page CommentOnIdea where
    isAuthorized = authNeedCaps [CanComment] coiCtx

-- | X. Deem idea feasible / not feasible
-- Assumption: The idea is located in the topic (via 'IdeaLocation').
data JudgeIdea = JudgeIdea
    { _jiCtx    :: CapCtx
    , _jiResult :: IdeaJuryResultType
    , _jiIdea   :: Idea
    , _jiTopic  :: Topic
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic JudgeIdea

makeLenses ''JudgeIdea

instance Page JudgeIdea where
    isAuthorized = authNeedCaps [CanJudge] jiCtx

data CreatorStatement = CreatorStatement { _csCtx :: CapCtx, _csIdea :: Idea }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic CreatorStatement

makeLenses ''CreatorStatement

instance Page CreatorStatement where
    isAuthorized = authNeedCapsAnyOf [CanEditCreatorStatement, CanAddCreatorStatement] csCtx

data ReportComment = ReportComment { _rcCtx :: CapCtx, _rcComment :: Comment }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic ReportComment

makeLenses ''ReportComment

instance Page ReportComment where
    -- You can report iff you can view the comment/idea.
    isAuthorized = authNeedCaps [CanView] rcCtx

-- We could track wether or not the comment is a reply, but this information is not used yet.
data EditComment
    = EditComment { _ecCtx :: CapCtx, _ecIdea :: Idea, _ecComment :: Comment }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic EditComment

makeLenses ''EditComment

instance Page EditComment where
    isAuthorized = authNeedCaps [CanEditComment] ecCtx

-- * templates

numberWithUnit :: Monad m => Int -> ST -> ST -> ST -> HtmlT m ()
numberWithUnit i i18n singular_ plural_ = do
    toHtml (show i)
    toHtmlRaw nbsp
    if i == 1
      then
        span_ [data_ "i18n" $ i18n <> "-singular"] $ toHtml singular_
      else
        span_ [data_ "i18n" $ i18n <> "-plural"] $ toHtml plural_

linkToIdeaLocation :: Monad m => Idea -> HtmlT m ()
linkToIdeaLocation idea = do
    a_ [ class_ "btn m-back detail-header-back"
       , href_ . U.listIdeas $ idea ^. ideaLocation
       ] $ case idea ^. ideaLocation of
         IdeaLocationSpace{} -> span_ [data_ "i18n" "move-to-wild-ideas"] "Zum Ideenraum"
         IdeaLocationTopic{} -> span_ [data_ "i18n" "idea-back-to-topic"] "Zum Thema"

instance ToHtml ViewIdea where
    toHtmlRaw = toHtml
    toHtml (ViewIdea _now _ctx (IdeaStats idea _phase _quo _voters))
        | idea ^. ideaDeleted = toHtml $ ViewDeletedIdea idea

    toHtml p@(ViewIdea now ctx stats@(IdeaStats idea phase _quo _voters)) = semanticDiv p $ do
        let totalLikes    = numLikes idea
            totalVotes    = Map.size $ idea ^. ideaVotes
            totalComments = idea ^. ideaComments . commentsCount
            spc           = idea ^. ideaLocation ^. ideaLocationSpace
            caps          = capabilities ctx

        div_ [class_ $ "hero-unit narrow-container phase-" <> cs (show phase)] $ do
            header_ [class_ "detail-header"] $ do
                linkToIdeaLocation idea

                contextMenu
                    [ ( CanEditAndDeleteIdea `elem` caps
                      , "icon-pencil"
                      , a_ [href_ $ U.editIdea idea, data_ "i18n" "idea-suggestion-edit"] "bearbeiten"
                      )
                    , ( ideaReachedQuorum stats && CanCreateTopic `elem` caps
                      , "icon-asterisk"
                      , a_ [href_ $ U.createTopic spc, data_ "i18n" "topiccreation-creation"] "Thema erstellen"
                      )
                    , ( CanMoveBetweenLocations `elem` caps
                      , "icon-sign-in"
                      , a_ [href_ $ U.moveIdea idea, data_ "i18n" "idea-move"] "Idee verschieben"
                      )
                    , ( True
                      , "icon-flag"
                      , a_ [href_ (U.reportIdea idea), data_ "i18n" "idea-suggestion-report"] "melden"
                      )
                    ]

            h1_ [class_ "main-heading"] $ do
                displayPhaseWithTime now phase
                idea ^. ideaTitle . html
            div_ [class_ "sub-header meta-text"] $ do
                idea ^. createdAt . to simpleTimestampToHtmlDate . html
            div_ [class_ "sub-header meta-text"] $ do
                span_ [data_ "i18n" "idea-from"] "von "
                a_ [ href_ $ U.userIdeas (idea ^. createdBy)
                   ] $ idea ^. createdByLogin . unUserLogin . html
                " / "
                let l = do
                        numberWithUnit totalLikes "idea-quorum-votes" "Quorum-Stimme" "Quorum-Stimmen"
                        toHtmlRaw (" " <> nbsp <> " / " <> nbsp <> " ")
                    v = do
                        numberWithUnit totalVotes "idea-votes" "Stimme" "Stimmen"
                        toHtmlRaw (" " <> nbsp <> " / " <> nbsp <> " ")
                    c = do
                        numberWithUnit totalComments "idea-suggestions" "Verbesserungsvorschlag" "Verbesserungsvorschläge"

                case phase of
                    PhaseWildIdea{}   -> l >> c
                    PhaseRefinement{} -> c
                    PhaseJury         -> c
                    PhaseVoting{}     -> v >> c
                    PhaseResult       -> v >> c

            -- bars
            toHtml $ IdeaVoteLikeBars stats

            -- indicators
            div_ [class_ "table-actions m-no-hover"] $ do
                div_ [class_ "icon-list m-inline"] . ul_ $ do
                    when (has _PhaseWildIdea phase && ideaReachedQuorum stats) $ do
                        li_ [class_ "icon-table"] $ span_ [data_ "i18n" "idea-on-table"] "Kann auf den Tisch"
                        feasibilityIndicator idea
                    when (has _PhaseResult phase) $
                        if isWinning idea
                            then li_ [class_ "icon-winner"] $ span_ "gewonnen"
                            else li_ [class_ "icon-hourglass"] $ span_ "nicht gewonnen"


            -- explanation by the dean why the idea is feasible or not (if available)
            feasibilityVerdict idea

            when (has _PhaseWildIdea phase &&
                  ideaReachedQuorum stats &&
                  CanCreateTopic `elem` caps) $ do
                div_ [class_ "table-actions m-no-hover"] $ do
                    button_ [ class_ "btn-cta m-valid"
                            , onclick_ $ U.createTopic spc
                            ] $ do
                        i_ [class_ "icon-check"] nil
                        span_ [data_ "i18n" "idea-create-topic"] "Thema anlegen"

            div_ [class_ "button-group"] $ do
                -- buttons
                ideaVoteLikeButtons ctx stats
                feasibilityButtons True idea caps

            -- creator statement
            mapM_
                (div_ [class_ "creator-statement"] . view html)
                (creatorStatementOfIdea idea)

            div_ [class_ "button-group"] $ do
                when (any (`elem` caps) [CanAddCreatorStatement, CanEditCreatorStatement]) $ do
                    button_ [ class_ "button-group-item btn-cta m-valid"
                            , onclick_ $ U.creatorStatement idea
                            ] $ do
                        i_ [class_ "icon-check"] nil
                        if isNothing $ creatorStatementOfIdea idea
                            then span_ [data_ "i18n" "idea-statement"] "Statement abgeben"
                            else span_ [data_ "i18n" "idea-suggestion-edit"] "Statement ändern"
                -- mark winning idea
                when (isFeasibleIdea idea) $ do
                    when (CanMarkWinner `elem` caps) $ do
                        let winnerButton =
                                postButton_
                                    [ class_ "btn-cta mark-winner-button button-group-item"
                                    , jsReloadOnClick
                                    ]

                        when (isNothing (idea ^. ideaVoteResult)) $
                            winnerButton (U.markIdeaAsWinner idea) "als \"gewonnen\" markieren"
                        when (isWinning idea) $
                            winnerButton (U.unmarkIdeaAsWinner idea) "\"gewonnen\" zurücknehmen"

        -- article
        div_ [class_ "container-narrow text-markdown"] $ do
            idea ^. ideaDesc . html

            div_ [class_ "view-category"] $ do
                case idea ^. ideaCategory of
                    Nothing -> do
                        h2_ [class_ "sub-header", data_ "i18n" "idea-without-category"] "Diese Idee gehört zu keiner Kategorie"
                    Just cat -> do
                        h2_ [class_ "sub-header", data_ "i18n" "idea-categories"] "Diese Idee gehört zur Kategorie"
                        div_ [class_ "icon-list m-inline"] .
                            ul_ . toHtml $ CategoryLabel cat

            div_ [class_ "translate"] $ do
                h2_ [class_ "btn-cta comments-header-button", data_ "i18n" "translate"] "Übersetzen"

        -- comments
        section_ [class_ "comments"] $ do
            header_ [class_ "comments-header"] $ do
                div_ [class_ "grid"] $ do
                    div_ [class_ "container-narrow"] $ do
                        h2_ [class_ "comments-header-heading"] $ do
                            numberWithUnit totalComments "idea-improvement-suggestions"
                                "Verbesserungsvorschlag" "Verbesserungsvorschläge"
                        when (CanComment `elem` caps) $
                            button_ [ value_ "create_comment"
                                    , class_ "btn-cta comments-header-button"
                                    , data_ "i18n" "new.comment"
                                    , onclick_ (U.commentOnIdea idea)]
                                "Neuer Verbesserungsvorschlag"
            div_ [class_ "comments-body grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    callToActionOnList'
                        (when (CanComment `elem` caps) .
                            a_ [href_ $ U.commentOnIdea idea, data_ "i18n" "idea-ask-first-suggestion"] $
                                "Gib den ersten Verbesserungsvorschlag ab!")
                        (\comment -> toHtml $ CommentWidget (ctx & capCtxComment .~ Just comment) caps comment)
                        (idea ^. ideaComments)


feasibilityIndicator :: Monad m => Idea -> HtmlT m ()
feasibilityIndicator idea = do
    case _ideaJuryResult idea of
        Nothing -> nil
        Just (IdeaJuryResult _ (Feasible _)) -> do
            li_ [class_ "icon-feasible"] $ span_ [data_ "i18n" "idea-is-possible"] "durchführbar"
        Just (IdeaJuryResult _ (NotFeasible _)) -> do
            li_ [class_ "icon-not-feasible"] $ span_ [data_ "i18n" "idea-not-possible"] "nicht durchführbar"

feasibilityVerdict :: Monad m => Idea -> HtmlT m ()
feasibilityVerdict idea =
    case _ideaJuryResult idea of
        Nothing                                        -> nil
        Just (IdeaJuryResult _ (Feasible Nothing))     -> nil
        Just (IdeaJuryResult _ (Feasible (Just expl))) -> explToHtml expl
        Just (IdeaJuryResult _ (NotFeasible expl))     -> explToHtml expl
  where
    explToHtml :: forall m. Monad m => Document -> HtmlT m ()
    explToHtml md = do
        div_ [class_ "info-text"] $ do
            "Begründung:"
            toHtml md

feasibilityButtons :: Monad m => Bool -> Idea -> [Capability] -> HtmlT m ()
feasibilityButtons renderJuryButtons idea caps =
    when (renderJuryButtons && CanJudge `elem` caps) $ do
            button_ [ class_ "button-group-item btn-cta m-valid"
                    , onclick_ $ U.judgeIdea idea IdeaFeasible
                    ] $ do
                i_ [class_ "icon-check"] nil
                span_ [data_ "i18n" "idea-is-possible"] "durchführbar"
            button_ [ class_ "button-group-item btn-cta m-invalid",
                      onclick_ $ U.judgeIdea idea IdeaNotFeasible
                    ] $ do
                i_ [class_ "icon-times"] nil
                span_ [data_ "i18n" "idea-is-possible"] "nicht durchführbar"


instance ToHtml ViewDeletedIdea where
    toHtmlRaw = toHtml
    toHtml p@(ViewDeletedIdea idea) = semanticDiv' [class_ "hero-unit narrow-container"] p $ do
        header_ [class_ "detail-header"] $ do
            linkToIdeaLocation idea
        div_ [class_ "container-not-found"] "Diese Idee wurde gelöscht."

validateIdeaTitle :: FormCS m r s
validateIdeaTitle = validate "Titel der Idee" titleV

instance FormPage CreateIdea where
    type FormPagePayload CreateIdea = ProtoIdea
    type FormPageResult CreateIdea = Idea

    formAction ci = U.createIdea $ ci ^. ciLoc
    redirectOf _ = U.viewIdea

    makeForm ci =
        ProtoIdea
        <$> ("title"         .: validateIdeaTitle (dftext Nothing))
        <*> ("idea-text"     .: validate "Idee" markdownV (dftext Nothing))
        <*> ("idea-category" .: makeFormSelectCategory Nothing)
        <*> pure (ci ^. ciLoc)

    formPage v form ci = createOrEditIdea (Left (ci ^. ciLoc)) v form ci

instance FormPage EditIdea where
    type FormPagePayload EditIdea = ProtoIdea

    formAction ei = U.editIdea $ ei ^. eiIdea
    redirectOf ei _ = U.viewIdea $ ei ^. eiIdea

    makeForm ei =
        ProtoIdea
        <$> ("title"         .: validateIdeaTitle (dftext . Just $ idea ^. ideaTitle))
        <*> ("idea-text"     .:
                validate "Idee" markdownV (dftext . Just . unMarkdown $ idea ^. ideaDesc))
        <*> ("idea-category" .: makeFormSelectCategory (idea ^. ideaCategory))
        <*> pure (idea ^. ideaLocation)
      where
        idea = ei ^. eiIdea

    formPage v form p@(EditIdea _ idea)
        | idea ^. ideaDeleted = toHtml $ ViewDeletedIdea idea
        | otherwise           = createOrEditIdea (Right idea) v form p

createOrEditIdea :: (Monad m, Typeable page, Page page) =>
    Either IdeaLocation Idea ->
    View (HtmlT m ()) -> (HtmlT m () -> HtmlT m ()) -> page -> HtmlT m ()
createOrEditIdea eLocIdea v form p =
    semanticDiv' [class_ "container-main container-narrow popup-page"] p $ do
        let cancelUrl = either id (view ideaLocation) eLocIdea
        h1_ [class_ "main-heading", data_ "i18n" "your-idea"] "Deine Idee"
        form $ do
            label_ $ do
                span_ [class_ "label-text", data_ "i18n" "idea-name"] "Wie soll deine Idee heißen?"
                inputText_ [class_ "m-small", data_ "i18n" "[placeholder]idea-name-example", placeholder_ "z.B. bessere Ausstattung im Computerraum"]
                    "title" v
            let editDomId    :: ST = DF.viewName v <> ".idea-text"
                previewDomId :: ST = editDomId <> "-preview"
            label_ $ do
                span_ [class_ "label-text", data_ "i18n" "idea-suggestion"] "Was möchtest du vorschlagen?"
                inputTextArea_ [placeholder_ "Hier kannst du deine Idee so ausführlich wie möglich beschreiben...", data_ "i18n" "[placeholder]idea-suggestion-description"]
                    Nothing Nothing "idea-text" v
                a_ [ class_ "btn m-input-action"
                   , data_ "i18n" "idea-show-preview"
                   , Lucid.onclick_ $ "showPreview('" <> editDomId <> "', '" <> previewDomId <> "')"
                   ]
                   "Vorschau einblenden"
            div_ [id_ previewDomId, class_ "markdown-preview m-closed"] nil
            formPageSelectCategory v
            footer_ [class_ "form-footer"] $ do
                input_ [type_ "submit", data_ "i18n" "[value]idea-publish", value_ "Idee veröffentlichen"]
                a_ [class_ "btn", data_ "i18n" "topiccreation-cancel", href_ $ U.listIdeas cancelUrl] $ do
                    -- FIXME: "are you sure?" dialog.
                    "abbrechen"
        case eLocIdea of
            Left _ -> nil
            Right idea ->
                footer_ [class_ "form-footer"] $ do
                    postButton_
                        [ class_ "btn-cta"
                        , data_ "i18n" "idea-confirm-delete"
                        , jsRedirectOnClickConfirm "Idee wirklich löschen?"
                            (absoluteUriPath . U.relPath $ U.listIdeas cancelUrl)
                        ]
                        (U.deleteIdea idea)
                        "Idee löschen"

instance FormPage MoveIdea where
    type FormPagePayload MoveIdea = Types.MoveIdea
    type FormPageResult  MoveIdea = Maybe Types.MoveIdea

    formAction mi    = U.moveIdea $ mi ^. miIdea
    redirectOf mi mt = U.viewIdea $ mi ^. miIdea & ideaLocation %~ maybe id moveIdeaToLocation mt

    makeForm (MoveIdea _ idea topics) =
        maybe MoveIdeaToWild MoveIdeaToTopic
        <$> ("topic-to-move" .: DF.choice topicList (Just currentTopic))
      where
        topicList = (Nothing, "Nach 'wilde Ideen'")
                  : map (Just . view _Id &&& view (topicTitle . html)) topics
        currentTopic = idea ^. ideaLocation ^? ideaLocationTopicId

    formPage v form mi = semanticDiv mi . form $ do
        h1_ [class_ "main-heading align-left"] "Idee verschieben"
        div_ [class_ "container-info"] . p_ $ do
            "Soll die Idee '" >> mi ^. miIdea . ideaTitle . html >> "'"
            " aus '" >> mi ^. miIdea ^. ideaLocation . uilabeledST . html >> "'"
            " verschoben werden?"
        DF.inputSelect "topic-to-move" v
        div_ [class_ "form-footer"] $ do
          DF.inputSubmit "Verschieben"
          cancelButton mi Nothing

commentIdeaNote :: Note Idea
commentIdeaNote = Note
    { noteHeaderText                = ("Verbesserungsvorschlag zu " <>) . view ideaTitle
    , noteExplanation               = Nothing
    , noteLabelText                 = "Was möchtest du sagen? Hast du Fragen?"
    , noteFieldNameInValiationError = "Verbesserungsvorschlag"
    , noteI18n                      = "idea-suggestion-to"
    , noteHeaderTextLeft            = const "Verbesserungsvorschlag zu "
    , noteHeaderTextMiddle          = view ideaTitle
    , noteHeaderTextRight           = const ""
    }

instance FormPage CommentOnIdea where
    type FormPagePayload CommentOnIdea = CommentContent
    type FormPageResult CommentOnIdea = Comment

    formAction = \case
        (CommentOnIdea _ idea Nothing)     -> U.commentOnIdea idea
        (CommentOnIdea _ _ (Just comment)) -> U.replyToComment comment
    redirectOf (CommentOnIdea _ idea _) = U.viewIdeaAtComment idea . view _Id

    makeForm CommentOnIdea{} =
        CommentContent <$> noteFormInput commentIdeaNote Nothing

    formPage v form coi = semanticDiv coi . (\h -> noteForm commentIdeaNote v form h) $ coi ^. coiIdea

instance FormPage EditComment where
    type FormPagePayload EditComment = Document

    formAction ec = U.editComment (ec ^. ecComment)

    redirectOf (EditComment _ idea comment) _ = U.viewIdeaAtComment idea (comment ^. _Id)

    makeForm ec = noteFormInput commentIdeaNote (Just (ec ^. ecComment . commentText))

    formPage v form ec = semanticDiv ec $ noteForm commentIdeaNote v form (ec ^. ecIdea)

judgeIdeaNote :: IdeaJuryResultType -> Note Idea
judgeIdeaNote juryType = Note
    { noteHeaderText                = (headerText <>) . view ideaTitle
    , noteExplanation               = Nothing
    , noteLabelText                 = labelText
    , noteFieldNameInValiationError = "Anmerkungen zur Durchführbarkeit"
    , noteI18n                      = "idea-possible-note"
    , noteHeaderTextLeft            = const headerText
    , noteHeaderTextMiddle          = view ideaTitle
    , noteHeaderTextRight           = const ""
    }
  where
    headerText = case juryType of
        IdeaFeasible    -> "[Angenommen zur Wahl] "
        IdeaNotFeasible -> "[Abgelehnt als nicht umsetzbar] "
    labelText = case juryType of
        IdeaFeasible    -> "Möchten Sie die Idee kommentieren?"
        IdeaNotFeasible -> "Idee ist nicht durchführbar. Begründung:"

instance FormPage JudgeIdea where
    type FormPagePayload JudgeIdea = IdeaJuryResultValue

    formAction (JudgeIdea _ juryType idea _topic) = U.judgeIdea idea juryType
    redirectOf ji _ = U.listIdeasInTopic (ji ^. jiTopic) ListIdeasInTopicTabAll Nothing
        -- FIXME: we would like to say `U.listIdeasInTopic topic </#> U.anchor (idea ^. _Id)` here,
        -- but that requires some refactoring around 'redirectOf'.

    makeForm (JudgeIdea _ IdeaFeasible idea _) =
        Feasible <$> noteFormOptionalInput (judgeIdeaNote IdeaFeasible) mFeasible
      where
        mFeasible :: Maybe Document = idea ^? ideaJuryResult . _Just . ideaJuryResultValue . _Feasible . _Just

    makeForm (JudgeIdea _ IdeaNotFeasible idea _) =
        NotFeasible <$> noteFormInput (judgeIdeaNote IdeaNotFeasible) mNotFeasible
      where
        mNotFeasible = idea ^? ideaJuryResult . _Just . ideaJuryResultValue . _NotFeasible

    formPage v form p@(JudgeIdea _ juryType idea _topic) =
        semanticDiv p $
            noteForm (judgeIdeaNote juryType) v form idea

creatorStatementNote :: Note Idea
creatorStatementNote = Note
    { noteHeaderText                = ("Ansage des Gewinners zur Idee" <>) . view ideaTitle
    , noteExplanation               = Nothing
    , noteLabelText                 = "Was möchtest du sagen?"
    , noteFieldNameInValiationError = "Statement des Autors"
    , noteI18n                      = "idea-winners-message"
    , noteHeaderTextLeft            = const "Ansage des Gewinners zur Idee"
    , noteHeaderTextMiddle          = view ideaTitle
    , noteHeaderTextRight           = const ""
    }

instance FormPage CreatorStatement where
    type FormPagePayload CreatorStatement = Document

    formAction      s   = U.creatorStatement $ s ^. csIdea
    redirectOf      s _ = U.viewIdea $ s ^. csIdea
    makeForm        s   = noteFormInput creatorStatementNote . creatorStatementOfIdea $ s ^. csIdea
    formPage v form s   = semanticDiv s . (\h -> noteForm creatorStatementNote v form h) $ s ^. csIdea

newtype ReportCommentContent = ReportCommentContent
    { unReportCommentContent :: Document }
  deriving (Eq, Show)

reportCommentNote :: Note ()
reportCommentNote = Note
    { noteHeaderText                = const "Verbesserungsvorschlag melden"
    , noteExplanation               = Just "Hier kannst du einen Verbesserungsvorschlag wegen eines verletzenden oder anstößigen Inhalts beim Moderationsteam melden. Das Team erhält eine Benachrichtigung und wird den Verbesserungsvorschlag schnellstmöglich überprüfen. Bitte gib unten einen Grund an, warum du den Inhalt für anstößig oder verletzend hältst."
    , noteLabelText                 = "Was möchtest du melden?"
    , noteFieldNameInValiationError = "Bemerkung"
    , noteI18n                      = "idea-comment-report"
    , noteHeaderTextLeft            = const "Verbesserungsvorschlag melden"
    , noteHeaderTextMiddle          = const ""
    , noteHeaderTextRight           = const ""
    }

instance FormPage ReportComment where
    type FormPagePayload ReportComment = ReportCommentContent

    formAction rc = U.reportComment $ rc ^. rcComment
    redirectOf rc _ = U.viewIdeaOfComment $ rc ^. rcComment

    makeForm _ =
        ReportCommentContent <$> noteFormInput reportCommentNote Nothing

    formPage v form p =
        semanticDiv p $ do
            noteForm reportCommentNote v form ()

reportIdeaNote :: Note Idea
reportIdeaNote = Note
    { noteHeaderText                = ("Die Idee " <>) . (<> " melden") . view ideaTitle
    , noteExplanation               = Just "Hier kannst du eine Idee wegen eines verletzenden oder anstößigen Inhalts beim Moderationsteam melden. Das Team erhält eine Benachrichtigung und wird die Idee schnellstmöglich überprüfen. Bitte gib unten einen Grund an, warum du den Inhalt für anstößig oder verletzend hältst."
    , noteLabelText                 = "Was möchtest du melden?"
    , noteFieldNameInValiationError = "Bemerkung"
    , noteI18n                      = "idea-report"
    , noteHeaderTextLeft            = const "Die Idee "
    , noteHeaderTextMiddle          = view ideaTitle
    , noteHeaderTextRight           = const " melden"
    }

instance FormPage ReportIdea where
    type FormPagePayload ReportIdea = Document

    formAction ri   = U.reportIdea $ ri ^. riIdea
    redirectOf ri _ = U.viewIdea   $ ri ^. riIdea

    makeForm _ = noteFormInput reportIdeaNote Nothing

    formPage v form ri = semanticDiv ri . (\h -> noteForm reportIdeaNote v form h) $ ri ^. riIdea


-- * handlers

-- | FIXME: 'viewIdea' and 'editIdea' do not take an 'IdeaSpace' or @'AUID' 'Topic'@ param from the
-- uri path, but use the idea location instead.  (this may potentially hide data inconsistencies.
-- on the bright side, it makes shorter uri paths possible.)
viewIdea :: (ActionPersist m, MonadError ActionExcept m, ActionUserHandler m, ActionCurrentTimestamp m, ActionLog m)
    => AUID Idea -> m ViewIdea
viewIdea ideaId = do
    now <- getCurrentTimestamp
    (ctx, idea) <- ideaCapCtx ideaId
    stats <- equery $ getIdeaStats idea -- FIXME ideaCapCtx' is giving the phase already
    pure $ ViewIdea now ctx stats

-- FIXME: ProtoIdea also holds an IdeaLocation, which can introduce inconsistency.
createIdea :: ActionM m => IdeaLocation -> FormPageHandler m CreateIdea
createIdea loc =
    formPageHandlerWithMsg
        (CreateIdea <$> locationCapCtx loc <*> pure loc)
        Action.createIdea
        "Die Idee wurde angelegt."

-- | FIXME: there is a race condition if several edits happen concurrently.  this can happen if
-- student and moderator edit an idea at the same time.  One solution would be to carry a
-- 'last-changed' timestamp in the edit form, and check for it before writing the edits.
editIdea :: ActionM m => AUID Idea -> FormPageHandler m EditIdea
editIdea ideaId =
    formPageHandlerWithMsg
        (uncurry EditIdea <$> ideaCapCtx ideaId)
        (Action.editIdea ideaId)
        "Die Änderungen wurden gespeichert."

moveIdea :: ActionM m => AUID Idea -> FormPageHandler m MoveIdea
moveIdea ideaId =
    formPageHandlerWithMsg
        (do (ctx, idea) <- ideaCapCtx ideaId
            topics <- equery $ findTopicsBySpace (idea ^. ideaLocation . ideaLocationSpace)
            pure $ MoveIdea ctx idea topics)
        (\mi -> Action.moveIdeaToTopic ideaId mi $> Just mi)
        "The Idee wurde verschoben."

reportIdea :: ActionM m => AUID Idea -> FormPageHandler m ReportIdea
reportIdea ideaId =
    formPageHandlerWithMsg
        (uncurry ReportIdea <$> ideaCapCtx ideaId)
        (Action.reportIdea ideaId)
        "Die Idee wurde der Moderation gemeldet."

-- | FIXME: make comments a sub-form and move that to "Frontend.Fragemnts.Comment".
commentOnIdea :: ActionM m => IdeaLocation -> AUID Idea -> FormPageHandler m CommentOnIdea
commentOnIdea loc ideaId =
    formPageHandlerWithMsg
        (do (ctx, idea) <- ideaCapCtx ideaId
            pure $ CommentOnIdea ctx idea Nothing)
        (\cc -> do
            comment <- addWithCurrentUser (AddCommentToIdea loc ideaId) cc
            eventLogUserCreatesComment comment
            return comment)
        "Der Verbesserungsvorschlag wurde gespeichert."

editComment :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> FormPageHandler m EditComment
editComment loc iid cid =
    formPageHandlerWithMsg
        (do (ctx, _, _, idea, comment) <- commentCapCtx' $ commentKey loc iid cid
            pure $ EditComment ctx idea comment)
        (\desc -> do
            let ck = commentKey loc iid cid
            update $ SetCommentDesc ck desc
            eventLogUserEditsComment =<< mquery (findComment ck))
        "Der Verbesserungsvorschlag wurde gespeichert."

replyToComment :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> FormPageHandler m CommentOnIdea
replyToComment loc ideaId commentId =
    formPageHandlerWithMsg
        (do (ctx, idea) <- ideaCapCtx ideaId
            pure . CommentOnIdea ctx idea $ idea ^. ideaComments . at commentId)
        (\cc -> do
            comment <- addWithCurrentUser (AddReply $ CommentKey loc ideaId [] commentId) cc
            eventLogUserCreatesComment comment
            return comment)
        "Der Verbesserungsvorschlag wurde gespeichert."

editReply :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment -> FormPageHandler m EditComment
editReply loc iid pcid cid =
    formPageHandlerWithMsg
        (do (ctx, _, _, idea, comment) <- commentCapCtx' $ replyKey loc iid pcid cid
            pure $ EditComment ctx idea comment)
        (\desc -> do
            let ck = replyKey loc iid pcid cid
            update $ SetCommentDesc ck desc
            eventLogUserEditsComment =<< mquery (findComment ck))
        "Der Verbesserungsvorschlag wurde gespeichert."

-- FIXME: Read the idea state from the db
judgeIdea :: ActionM m => AUID Idea -> IdeaJuryResultType -> FormPageHandler m JudgeIdea
judgeIdea ideaId juryType =
    formPageHandlerWithMsg
        (do (ctx, mtopic, _, idea) <- ideaCapCtx' ideaId
            topic <- mquery $ pure mtopic
            pure $ JudgeIdea ctx juryType idea topic)
        (Action.markIdeaInJuryPhase ideaId)
        ("Die Idee wurde als " <> showJuryResultTypeUI juryType <> " markiert")

creatorStatementOfIdea :: Idea -> Maybe Document
creatorStatementOfIdea idea = idea ^? ideaVoteResult . _Just . ideaVoteResultValue . _Winning . _Just

creatorStatement :: ActionM m => AUID Idea -> FormPageHandler m CreatorStatement
creatorStatement ideaId =
    formPageHandlerWithMsg
        (uncurry CreatorStatement <$> ideaCapCtx ideaId)
        (Action.setCreatorStatement ideaId)
        "Das Statement wurde gespeichert."

reportComment
    :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment
    -> FormPageHandler m ReportComment
reportComment loc iid cid =
    formPageHandlerWithMsg
        (uncurry ReportComment <$> commentCapCtx (commentKey loc iid cid))
        (Action.reportIdeaComment loc iid cid . unReportCommentContent)
        "Die Meldung wurde abgeschickt."

reportReply
    :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment
    -> FormPageHandler m ReportComment
reportReply loc iid pcid cid =
    formPageHandlerWithMsg
        (uncurry ReportComment <$> commentCapCtx (replyKey loc iid pcid cid))
        (Action.reportIdeaCommentReply loc iid pcid cid . unReportCommentContent)
        "Die Meldung wurde abgeschickt."
