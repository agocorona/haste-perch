module Main where
import Haste.DOM
import Builder
import Control.Monad.State
import Data.Monoid
import Prelude hiding (div)

main= do
  withElem "idelem" . build $ do
    div $ do
         div $ do
               p "hello"
               nelem "p" `attr` ("style","color:red")  `child`  "world"

  return ()

div cont=  nelem "div" `child`  cont

p cont = nelem "p"  `child`  cont
