module Main where
import Haste.DOM
import Haste.Perch
import Haste.Perch.Literate
import Data.Monoid
import Prelude hiding (div)



main= do
  lit "hello <b>world<b>"
  body <- getBody
  build' body $ do
    div $ do
         div $ do
               p "hello"
               p ! atr "style" "color:red" $   "world"

  return ()

  where
  build'= flip build
