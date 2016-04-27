{-# LANGUAGE LambdaCase #-}

-- {-# OPTIONS_GHC -Wall -Werror #-}

-- TODO: Make it as a structure.
module Frontend.Validation
    ( module TP
    , validation
    , (<??>)
    , FieldParser
    , satisfies
    )
where

import Text.Digestive as TD
import Text.Parsec as TP
import Text.Parsec.Error

import Frontend.Prelude

type FieldParser a = Parsec String () a

-- FIXME: Use (Error -> Html) instead of toHtml
fieldValidation :: String -> FieldParser a -> String -> Result (Html ()) a
fieldValidation name parser t =
    either (TD.Error . toHtml . errors) TD.Success $ parse parser name (cs t)
  where
    -- TODO: Remove '\n' from the error messages
    errors e = unwords [sourceName $ errorPos e, "-", errorMsgs $ errorMessages e]
    -- TODO: Translation :)
    -- errorMsgs = showErrorMessages "or" "unknown" "excepting" "unexpected" "end of input"
    errorMsgs = showErrorMessages "vagy" "ismeretlen" "ideillo" "nem vart" "sztring vege"

validation :: (Monad m) => String -> FieldParser a -> Form (Html ()) m String -> Form (Html ()) m a
validation n p = TD.validate (fieldValidation n p)


-- Missing from Parsec.
infix 0 <??>

-- Set the given message if the parser fails as an error message, pretend
-- no input is consumed.
(<??>) :: ParsecT s u m a -> String -> ParsecT s u m a
p <??> msg = TP.try p <?> msg

satisfies :: (a -> Bool) -> ParsecT s u m a -> ParsecT s u m a
satisfies pred parser = do
    x <- parser
    unless (pred x) $ fail ""
    return x
