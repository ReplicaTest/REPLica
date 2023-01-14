module Replica.App.Display

import Control.App
import Control.App.Console

import Replica.App.Format
import Replica.App.Replica
import Replica.Option.Global

-- display the name of a suite
export
displaySuite :
  Has [ State GlobalConfig Global
      , Console
      ] e => Maybe String -> App e ()
displaySuite suite = putStrLn $ !bold $ maybe "No suite given:" ("Suite: " <+>) suite
