{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.Note
where

import qualified Text.Digestive.Form as DF
-- import qualified Text.Digestive.Lucid.Html5 as DF

import Frontend.Prelude
import Frontend.Validation


data Note h = Note
    { noteHeaderText                :: h -> ST
    , noteHeaderTextLeft            :: h -> ST
    , noteHeaderTextMiddle          :: h -> ST
    , noteHeaderTextRight           :: h -> ST
    , noteExplanation               :: Maybe ST
    , noteLabelText                 :: ST
    , noteFieldNameInValiationError :: ST
    , noteI18n                      :: ST
    }

noteForm
    :: (Monad m)
    => Note h
    -> View (HtmlT m ())
    -> (HtmlT m () -> HtmlT m ())
    -> h
    -> HtmlT m ()
noteForm note v form header = do
    div_ [class_ "container-note"] $ do
        h1_ [class_ "main-heading"] $ do
            span_ [data_ "i18n" ((noteI18n note) <> "-header-left")] $ toHtml $ noteHeaderTextLeft note header
            span_ [] $ toHtml $ noteHeaderTextMiddle note header
            span_ [data_ "i18n" ((noteI18n note) <> "-header-right")] $ toHtml $  noteHeaderTextRight note header
        (div_ [class_ "container-info"] . p_ [data_ "i18n" ((noteI18n note) <> "-explanation")] . toHtml) `mapM_` noteExplanation note
        form $ do
            label_ $ do
                span_ [class_ "label-text", data_ "i18n" $ noteI18n note] . toHtml $ noteLabelText note
                inputTextArea_ [placeholder_ "..."] Nothing Nothing "note-text" v
                footer_ [class_ "form-footer"] $ do
                  input_ [type_ "submit", data_ "i18n" "[value]send-button", value_ "Abschicken"]

noteFormInput :: (Monad n, Monad m) => Note t -> Maybe Document -> DF.Form (HtmlT n ()) m Document
noteFormInput note mdoc =
    "note-text" .: validate (cs $ noteFieldNameInValiationError note)
                            markdownV
                            (dftext (unMarkdown <$> mdoc))

noteFormOptionalInput :: (Monad n, Monad m) => Note t -> Maybe Document -> DF.Form (HtmlT n ()) m (Maybe Document)
noteFormOptionalInput note mdoc =
    "note-text" .: validateOptional (cs $ noteFieldNameInValiationError note)
                                    markdownV
                                    (cs <$$> DF.optionalString (cs . unMarkdown <$> mdoc))
