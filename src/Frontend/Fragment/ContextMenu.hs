{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.ContextMenu
where

import Frontend.Prelude


contextMenu :: Monad m => [(Bool, ST, HtmlT m ())] -> HtmlT m ()
contextMenu (filter (view _1) -> entries) = do
    nav_ [class_ "pop-menu m-cta detail-header-menu"] $ do
        span_ [class_ "btn-cta", data_ "i18n" "options"] "Optionen"
        ul_ [class_ "pop-menu-list"] $ do
            if null entries
                then li_ [class_ "pop-menu-list-item", data_ "i18n" "no-further-options"] "(Keine weiteren Optionen)"
                else go `mapM_` entries
  where
    go :: Monad m => (Bool, ST, HtmlT m ()) -> HtmlT m ()
    go (_, icon, body) = li_ [class_ "pop-menu-list-item m-form"] .
        div_ [class_ "pop-menu-list-item-form-wrapper"] $ do
            i_ [class_ icon] nil
            body
