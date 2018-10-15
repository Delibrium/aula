{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.PhaseTime (displayPhaseWithTime)
where

import Control.Category ((.))
import Data.Time
import Prelude hiding ((.))

import Config (unsafeTimestampToLocalTime, aulaTimeLocale)
import Frontend.Prelude


displayPhaseWithTime :: Monad m => Timestamp -> Phase -> HtmlT m ()
displayPhaseWithTime now phase = do
    span_ [class_ "sub-heading"] $ do
        phase ^. uilabeledST . to (\x -> span_ [data_ "i18n" $ "idea-phase-" <> x] $ toHtml x)
        " "
        phase ^. displayPhaseTime now . to (mapM_ (\(i18n, s) -> if i18n /= "number" then if i18n == "time-stamp" then toHtml s else span_ [data_ "i18n" $ "idea-phase-time-" <> i18n] $ toHtml s else toHtml i18n))

displayPhaseTime :: Monoid r => Timestamp -> Getting r Phase [(ST, String)]
displayPhaseTime now = phaseStatus . to info
  where
    info t@(ActivePhase stamp) =
        [("no-holiday-Ends","(Endet ")] ++ displayTimespan t ++ [("time-stamp", showStamp stamp), ("close", ")")]
    info t@(FrozenPhase _) =
        [("holiday", "(Endet " <> displayTimespanFrozen t <> ")")]

    displayTimespan st = case stampToDays st of
        -- n | n < 0 -> assert False $ error "displayPhaseTime"  -- (this breaks the test suite)
        0 -> [("today", "heute")]
        1 -> [("tomorrow", "morgen")]
        n -> [("in", "in "), ("number", show n), ("days"," Tagen")]

    displayTimespanFrozen st = (cs . show . stampToDays $ st) <> " Tage nach den Ferien"
    stampToDays st = timespanDays (st ^. phaseLeftoverFrom now) + 1
    showStamp = formatTime aulaTimeLocale " am %d.%m.%Y um ca. %H Uhr" . unsafeTimestampToLocalTime
