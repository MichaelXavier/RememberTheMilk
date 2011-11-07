module Main (main) where

import Test.Hspec (hspecX)

import qualified Web.RememberTheMilk.Testing.Types as T (specs)

main :: IO ()
main = hspecX T.specs
