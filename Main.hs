-- {-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TemplateHaskell #-}

import Zwerg.SDL
import Control.Monad.State.Strict (evalStateT)

main :: IO ()
main = evalStateT playZWERG initZwergSDLState
