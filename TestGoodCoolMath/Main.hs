module Main where

import Test.Hspec ( hspec )
import qualified SimplificationTests ( spec )

main :: IO ()
main = hspec $ do
  SimplificationTests.spec
