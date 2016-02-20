{-# LANGUAGE DataKinds, GADTs, OverloadedStrings, PolyKinds, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies                                                #-}
module Main where

import qualified Data.Text       as T
import           Language.OSA
import           Language.OSA.QQ

main :: IO ()
main = do
  as <- [osa|tell application "Finder"
               display dialog "Fourier"
             end tell|]
  print =<< compileOrError as
  print =<< execute as
  return ()

