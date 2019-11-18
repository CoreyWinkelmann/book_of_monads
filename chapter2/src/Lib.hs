module Lib where

import Control.Monad.State.Lazy

incrCounter :: State Integer Integer
incrCounter = do n <- get
                 p <- put (n + 1)
                 return (n + 1)

testingState = print $ evalState incrCounter 0
