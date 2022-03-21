module SpreadsheetSpec (main, spec) where

import Spreadsheet
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Spreadsheet" $ do
    it "handles dependency changes" $ do
      changeDependencies >>= (`shouldBe` (3, 102, 20))
    it "handles dependencies of many types" $ do
      differentTypesDependencies >>= (`shouldBe` (7, 5))

-- | Example of a graph of cells.
threeCells :: Exp (Cell Int, Cell Int, Cell Int)
threeCells = do
  a <- cell $ pure 1

  b <- cell $ pure 2

  -- c = a + b
  c <- cell $ do
    aValue <- get a
    bValue <- get b
    pure $ aValue + bValue

  pure (a, b, c)

-- | Example of propagating changes.
changeDependencies :: IO (Int, Int, Int)
changeDependencies = do
  (a, b, c) <- evalExp threeCells

  -- c = a + b = 1 + 2 = 3
  c3 <- evalExp $ get c

  -- a = 100
  -- So c = a + b = 100 + 2 = 102
  set a $ pure 100
  c102 <- evalExp $ get c

  -- a = b*b
  -- b = 4
  -- So c = a + b = 4*4 + 4 = 20
  set a $ do
    bValue <- get b
    pure $ bValue * bValue
  set b $ pure 4
  c20 <- evalExp $ get c

  pure (c3, c102, c20)

-- | Example of a graph of cells with different types.
differentTypesCells :: Exp (Cell String, Cell Int, Cell Int)
differentTypesCells = do
  a <- cell $ pure "hello"

  b <- cell $ pure 2

  -- c = a + b
  c <- cell $ do
    aValue <- get a
    bValue <- get b
    pure $ length aValue + bValue

  pure (a, b, c)

-- | Example of propagating changes for cells with different types.
differentTypesDependencies :: IO (Int, Int)
differentTypesDependencies = do
  (a, b, c) <- evalExp differentTypesCells

  -- c = length a + b = 5 + 2 = 7
  c7 <- evalExp $ get c

  -- b = 3
  set b $ pure 3

  -- a = "no"
  -- So c = length a + b = 2 + 3 = 5
  set a $ pure "no"
  c5 <- evalExp $ get c

  pure (c7, c5)
