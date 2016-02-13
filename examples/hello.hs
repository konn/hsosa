{-# LANGUAGE DataKinds, GADTs, OverloadedStrings, PolyKinds, TypeFamilies #-}
module Main where

import qualified Data.Text    as T
import           Language.OSA

main :: IO ()
main = do
  doAppleScript "tell application \"Finder\" to display dialog \"hello!\""
  return ()
