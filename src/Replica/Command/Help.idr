module Replica.Command.Help

import Data.List
import Data.List1
import Data.String

import Replica.Command.Info
import Replica.Command.Run
import Replica.Command.Set
import Replica.Command.New
import Replica.Command.Version
import public Replica.Help
import Replica.Option.Types
import Replica.Other.Validation

export
help : Help
help = MkHelp
  { name = "help"
  , usage = Just "replica COMMAND [COMMAND_OPTIONS]"
  , description = "Integration testing for command line interfaces"
  , chapter = [ ("Commands", helpRun ::: [helpTest, helpInfo, helpSet, helpNew, helpVersion])
    ]
  , lastWords = Just "Run 'replica help COMMAND' for more information on a command."
  }

parseHelp' : Help -> List1 String -> ParseResult Help
parseHelp' help xs@(name:::ys) = maybe
  (InvalidOption xs)
  ( const $ case ys of
      [] => Done help
      (next::ys') => let
        subs = foldMap (forget . snd) help.chapter
        in foldl
             (\res, h => res <+> parseHelp' h (assert_smaller xs (next:::ys')))
             (InvalidOption $ pure "Cannot find help for '\{unwords ys}'")
             subs
  )
  $ guard $ name == help.name

export
parseHelp : List1 String -> ParseResult Help
parseHelp = parseHelp' help
