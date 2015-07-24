module SpreadsheetSpec (main, spec) where

import Test.Hspec

import Spreadsheet

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Spreadsheet" $ do
    it "handles dependency changes" $ do
      changeDependencies >>= (`shouldBe` (3, 102, 20))

-- | Example of a graph of cells.
threeCells :: Exp (Cell Int, Cell Int, Cell Int)
threeCells = do
  a <- cell $ return 1

  b <- cell $ return 2

  -- c = a + b
  c <- cell $ do
    aValue <- get a
    bValue <- get b
    return $ aValue + bValue

  return (a, b, c)

-- | Example of propagating changes.
changeDependencies :: IO (Int, Int, Int)
changeDependencies = do
  (a, b, c) <- evalExp threeCells

  -- c = a + b = 1 + 2 = 3
  c3 <- evalExp $ get c

  -- a = 100
  -- So c = a + b = 100 + 2 = 102
  set a $ return 100
  c102 <- evalExp $ get c

  -- a = b*b
  -- b = 4
  -- So c = a + b = 4*4 + 4 = 20
  set a $ do
    bValue <- get b
    return $ bValue * bValue
  set b $ return 4
  c20 <- evalExp $ get c

  return (c3, c102, c20)
