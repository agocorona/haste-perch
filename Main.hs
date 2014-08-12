module Main where
import Haste.DOM
import Haste.Perch
import Data.Monoid
import Prelude hiding (div)



main= do
  body <- getBody
  build' body $ do
    div $ do
         div $ do
               p "hello"
               p ! atr "style" "color:red" $   "world"

  return ()

  where
  build'= flip build
