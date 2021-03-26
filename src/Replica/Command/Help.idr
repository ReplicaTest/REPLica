module Replica.Command.Help

import Data.List
import Data.List1

import Replica.Command.Info
import Replica.Command.Run
import public Replica.Help
import Replica.Option.Global
import Replica.Other.Validation

export
help : Help
help = MkHelp
  "replica"
  (Just "replica [GLOBAL_OPTIONS] COMMAND [COMMAND_OPTIONS]")
  "Integration testing for command line interfaces"
  [ ("Commands", helpRun globalOptionsHelp ::: [helpInfo globalOptionsHelp])
  , ("Options", globalOptionsHelp)
  ]
  (Just "Run 'replica help COMMAND' for more information on a command.")

export
parseHelp : List String -> Either String Help
parseHelp ["help"] = pure help
parseHelp ["help", command] = maybe
  (Left "Help unavailable, \{show command} is not a valid command")
  pure $ do
    commands <- "Commands" `lookup` help.chapter
    lookup command $ map (\h => (h.name, h)) $ forget commands
parseHelp ("help" :: xs) = Left $ "Too many arguments for help"
parseHelp xs = Left "Not a know command"
