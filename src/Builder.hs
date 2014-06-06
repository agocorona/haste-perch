-----------------------------------------------------------------------------
--
-- Module      :  Builder
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-#LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module Builder where
import Data.Typeable
import Haste.DOM
import Data.Monoid
import Unsafe.Coerce

newtype JSBuilderM a= JSBuilder{build :: Elem -> IO Elem} deriving Typeable

type JSBuilder = JSBuilderM ()

instance Monoid (JSBuilderM a) where
    mappend mx my= JSBuilder $ \e -> do
         x <- build mx e
         y <- build my e
         addChild x e
         addChild y e
         return e
    mempty  = JSBuilder return

instance Monad JSBuilderM where
   (>>) x y= mappend (unsafeCoerce x) y


   return  = mempty


child :: ToElem a => JSBuilder -> a -> JSBuilder
child me ch= JSBuilder $ \e' -> do
        e <- build me e'
        let t = toElem ch
        r <- build t e
        return e

class ToElem a where
  toElem :: a -> JSBuilder

instance ToElem String where toElem s= JSBuilder $ const $ newTextElem s

instance ToElem (JSBuilderM a) where toElem e = unsafeCoerce e

attr tag (n, v)=JSBuilder $ \e -> do
        tag' <- build tag e
        setAttr tag' n v
        return tag'


melem s= JSBuilder $ \e ->do
    e' <- newElem s
    addChild e' e
    return e'







