{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.Comment (CommentWidget(..), cwComment)
where

import qualified Generics.SOP as SOP

import Access
import Frontend.Prelude

import qualified Frontend.Path as U


data CommentWidget = CommentWidget
    { _cwCapCtx   :: CapCtx
    , _cwIdeaCaps :: [Capability]
    , _cwComment  :: Comment
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic CommentWidget

makeLenses ''CommentWidget

instance ToHtml CommentWidget where
    toHtmlRaw = toHtml
    toHtml w = semanticDiv' [class_ "comment"] w $ do
        commentToHtml w
        div_ [class_ "comment-replies"] . for_ (w ^. cwComment . commentReplies) $ \reply ->
            div_ [class_ "comment-reply"] . commentToHtml $ w & cwComment .~ reply

commentToHtml :: Monad m => CommentWidget -> HtmlT m ()
commentToHtml w = div_ [id_ . U.anchor $ comment ^. _Id] $ do
    header_ [class_ "comment-header"] $ do
        let mi = comment ^. commentMeta

        span_ $ do
          div_ [class_ "author"] $ do
            a_ [href_ $ U.viewUserIdProfile (mi ^. metaCreatedBy)] $ do
                span_ [class_ "author-image"] $
                    userAvatarImg' avatarSizeSmall (mi ^. metaCreatedBy) (mi ^. metaCreatedByLogin)
                span_ [class_ "author-text"] $
                    mi ^. metaCreatedByLogin . unUserLogin . html

            span_ [class_ "author-timestamp"] $
              " am " <> (mi ^. metaCreatedAt . to simpleTimestampToHtmlDate . html)

        CommentVotesWidget (w ^. cwIdeaCaps) comment ^. html
    div_ [class_ "comments-body"] $ do
        if comment ^. commentDeleted
            then span_ [data_ "i18n" "deleted-content"] "[Inhalt gelöscht]"
            else comment ^. commentText . html
    unless (comment ^. commentDeleted) . footer_ [class_ "comment-footer"] $ do
        div_ [class_ "comment-footer-buttons"] $ do
            when (CanComment `elem` w ^. cwIdeaCaps && CanReplyComment `elem` comCaps) .
                button_ [class_ "btn comment-footer-button", onclick_ $ U.replyToComment comment] $ do
                    i_ [class_ "icon-reply"] nil
                    span_ [data_ "i18n" "answer"] "antworten"
            a_ [class_ "btn comment-footer-button", href_ (U.reportComment comment)] $ do
                i_ [class_ "icon-flag"] nil
                span_ [data_ "i18n" "report"] "melden"
            when (CanEditComment `elem` comCaps) $ do
                let edit = commentNestingElim U.editComment U.editReply $ commentNesting comment
                a_ [class_ "btn comment-footer-button", href_ (edit comment)] $ do
                    i_ [class_ "icon-pencil"] nil
                    span_ [data_ "i18n" "idea-suggestion-edit"] "bearbeiten"
            when (CanDeleteComment `elem` comCaps) $ do
                let msg = "Kommentar wirklich loeschen?"
                                          -- FIXME: umlauts.  `ö`, `\\u00F6`, or `&ouml;` won't do it.
                postButton_ [ class_ "btn comment-footer-button"
                            , jsReloadOnClickAnchorConfirm msg . U.anchor $ comment ^. _Id
                            ]
                            (U.deleteComment comment) $ do
                    i_ [class_ "icon-trash-o"] nil
                    span_ [data_ "i18n" "idea-suggestion-delete"] "löschen"
  where
    comment = w ^. cwComment
    comCaps = capabilities (w ^. cwCapCtx)


data CommentVotesWidget = CommentVotesWidget [Capability] Comment

instance ToHtml CommentVotesWidget where
    toHtmlRaw = toHtml
    toHtml p@(CommentVotesWidget caps comment) = semanticDiv' [class_ "comment-votes"] p .
        unless (comment ^. commentDeleted) $ do
            voteButton Up
            voteButton Down
      where
        votes = comment ^. commentVotes
        voteButton v = do
            span_ [class_ $ "comment-vote-" <> vs] $ do
                countCommentVotes v votes ^. showed . html
                let likeButton = if CanVoteComment `elem` caps
                        then postButton_ [ class_ "btn"
                                         , jsReloadOnClickAnchor . U.anchor $ comment ^. _Id
                                         ]
                                     (U.voteOnComment comment v)
                        else div_ [class_ "btn"]
                likeButton $
                    i_ [class_ $ "icon-thumbs-o-" <> vs] nil
          where vs = cs . lowerFirst $ show v
