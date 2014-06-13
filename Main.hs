module Main where
import Haste.DOM
import Builder
import Control.Monad.State
import Data.Monoid
import Prelude hiding (div)

main= do
  withElem "idelem" $   build $ do
    div $ do
         div $ do
               p "hello"
               p ! atr "style" "color:red" $   "world"

  return ()

