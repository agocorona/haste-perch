-----------------------------------------------------------------------------
--
-- Module      :  Haste.HPlay.Literate
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
{-# LANGUAGE OverloadedStrings #-}
module Haste.Perch.Literate (
lit, print, putStrLn, putStr
) where
import Haste
import Haste.Perch
import Prelude hiding (span,div,putStr,putStrLn,print)



lit text=  getBody >>= build (nelem "div" `setHtml` text)



print x= getBody >>=  build( span (show x) >> br)

putStrLn x = getBody >>=  build( span x >> br)

putStr x= getBody >>= build(span x)


