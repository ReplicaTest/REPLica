module Replica.Command.Help

import Data.List
import Data.List1

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
  "replica"
  (Just "replica COMMAND [COMMAND_OPTIONS]")
  "Integration testing for command line interfaces"
  [ ("Commands", helpRun ::: [helpInfo, helpSet, helpNew, helpVersion])
  ]
  (Just "Run 'replica help COMMAND' for more information on a command.")

export
parseHelp : List1 String -> ParseResult Help
parseHelp ("help" ::: []) = Done help
parseHelp ("help" ::: [command]) = maybe
  (InvalidMix "Help unavailable, \{show command} is not a valid command")
  Done $ do
    commands <- "Commands" `lookup` help.chapter
    lookup command $ map (\h => (h.name, h)) $ forget commands
parseHelp ("help" ::: xs) = InvalidMix $ "Too many arguments for help"
parseHelp xs = InvalidOption xs
