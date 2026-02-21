module Spreadsheet.CellExtraSpec (main, spec) where

import Control.Exception (AsyncException (StackOverflow))
import Spreadsheet.Cell
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Spreadsheet.Cell extra" $ do
        it "raises on cycle" $ do
            a <- run (cell $ pure (1 :: Int))
            set a (do aValue <- get a; pure (aValue + 1))
            run (get a) `shouldThrow` (== StackOverflow)

        it "handles set before evaluation" $ do
            a <- run (cell $ pure 1)
            set a (pure 2)
            run (get a) `shouldReturn` (2 :: Int)

        it "handles multiple readers" $ do
            a <- run (cell $ pure 1)
            b <- run (cell $ do aValue <- get a; pure (aValue + 1))
            c <- run (cell $ do aValue <- get a; pure (aValue + 2))
            set a (pure 2)
            run (get b) `shouldReturn` (3 :: Int)
            run (get c) `shouldReturn` (4 :: Int)

        it "handles chain dependencies" $ do
            a <- run (cell $ pure 1)
            b <- run (cell $ do aValue <- get a; pure (aValue + 1))
            c <- run (cell $ do bValue <- get b; pure (bValue + 1))
            set a (pure 2)
            run (get c) `shouldReturn` (4 :: Int)

        it "handles diamond dependency" $ do
            a <- run (cell $ pure 1)
            b <- run (cell $ do v <- get a; pure (v + 10))
            c <- run (cell $ do v <- get a; pure (v + 100))
            d <- run (cell $ do bv <- get b; cv <- get c; pure (bv + cv))
            run (get d) `shouldReturn` (112 :: Int)
            set a (pure 2)
            run (get d) `shouldReturn` (114 :: Int)

        it "handles dynamic dependency change" $ do
            flag <- run (cell $ pure True)
            x <- run (cell $ pure 10)
            y <- run (cell $ pure 20)
            c <- run (cell $ do
                f <- get flag
                if f then get x else get y)
            run (get c) `shouldReturn` (10 :: Int)
            set flag (pure False)
            run (get c) `shouldReturn` (20 :: Int)
            set y (pure 99)
            run (get c) `shouldReturn` (99 :: Int)

        it "handles reading same cell twice" $ do
            a <- run (cell $ pure 5)
            b <- run (cell $ do x <- get a; y <- get a; pure (x + y))
            run (get b) `shouldReturn` (10 :: Int)
            set a (pure 3)
            run (get b) `shouldReturn` (6 :: Int)

        it "handles multiple sets without gets" $ do
            a <- run (cell $ pure 1)
            b <- run (cell $ do v <- get a; pure (v * 10))
            _ <- run (get b)
            set a (pure 2)
            set a (pure 3)
            set a (pure 4)
            run (get b) `shouldReturn` (40 :: Int)
