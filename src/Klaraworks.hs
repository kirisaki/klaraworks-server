module Klaraworks
    ( runServer
    ) where

import Data.Text
import Klaraworks.Api

runServer :: Text -> IO ()
runServer path = putStrLn "Nyaan"
