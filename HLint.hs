import "hlint" HLint.Default
import "hlint" HLint.Dollar
import "hlint" HLint.Generalise
import "hlint" HLint.HLint

ignore "Avoid lambda" = Action.mkRunAction
ignore "Use camelCase" = Main.ViewIdea_PhaseNone
ignore "Use camelCase" = Main.ViewIdea_PhaseRefinement
ignore "Use camelCase" = Main.ViewIdea_PhaseJury
ignore "Use camelCase" = Main.ViewIdea_PhaseVoting
ignore "Use camelCase" = Main.ViewIdea_PhaseResult
ignore "Use camelCase" = Main.ViewIdea_PhaseFinished

ignore "Redundant do"
ignore "Use const"
ignore "Use fmap"
ignore "Use list literal"
ignore "Use record patterns"
ignore "Use =<<"

warn = listToMaybe (filter f xs) ==> find f xs
warn = isJust $ find f xs ==> any f xs
