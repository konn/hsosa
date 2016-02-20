{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Language.OSA.QQ where
import Language.OSA

import qualified Data.List                 as L
import           Data.Maybe
import qualified Data.Text                 as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

osa :: QuasiQuoter
osa = QuasiQuoter { quoteExp  = quoteOSA
                  , quotePat  = fail "not implemented"
                  , quoteDec  = fail "not implemented"
                  , quoteType = fail "not implemented"
                  }

detectLinePos :: String -> Int -> (Int, Int)
detectLinePos txt at =
  let starts = init $ scanl (+) 0 $ map (succ . length) $ lines txt
      ln = pred $ fromMaybe (length starts) $ L.findIndex (>at) starts
  in (ln, at - (starts !! ln))

quoteOSA :: String -> ExpQ
quoteOSA src = do
  eith <- runIO $ compile =<< newAppleScript (T.pack src)
  case eith of
    Right () -> [| newAppleScript $(litE (stringL src)) |]
    Left err -> do
      Loc{loc_start = (glL, glP)} <- location
      let (loc, len) = asErrorRange err
          (l0, c0) = detectLinePos src loc
          (l1, c1) = detectLinePos src (loc+len)
          stL = lines src !! l0
          dispL = glL + l0
          dispC | loc == 0 = glP + c0
                | otherwise = c0
      fail $ unlines[ T.unpack (T.strip $ asErrorMessage err)
                    , "at LINE " ++ show dispL ++ ", COL " ++ show dispC ++ ": "
                    , "\t> " ++ stL
                    , "\t> " ++ replicate c0 ' ' ++ "^"
                    ]

