{-# LANGUAGE GADTs #-}

-- | Haskell version of spreadsheet demo from <http://semantic-domain.blogspot.com/2015/07/how-to-implement-spreadsheet.html Neel Krishnaswami's blog post>.
module Spreadsheet (
                     -- * Types
                     Cell
                   , Exp

                     -- * Cell operations
                   , cell
                   , get
                   , set

                     -- * Expression elimination
                   , evalExp
                   ) where

import Control.Monad (ap, liftM)
import Data.IORef
import Data.Unique (Unique, newUnique)
import Data.List (union)

-- | Container for a value that can depend on other cells
-- through a code expression.
--
-- Has a unique identity, even across different cell types.
data Cell a =
  Cell { _code :: IORef (Exp a)      -- ^ expression to run
       , _value :: IORef (Maybe a)   -- ^ value memoized from running code
       , _reads :: IORef [ECell]     -- ^ cells that were read
       , _observers :: IORef [ECell] -- ^ cells that read this cell
       , _id :: Unique               -- ^ globally unique token
       }

-- | A computed result, along with the cells read during computation.
data Result a = Result a [ECell]

-- | An expression that is run to give a result.
newtype Exp a = Exp { runExp :: IO (Result a) }

-- | Existential type: "a cell of some type".
-- Used for a heterogeneous list of different types of cells.
data ECell where
  Pack :: Cell a -> ECell

-- | Compare cells using ID field.
instance Eq ECell where
  Pack x == Pack y = _id x == _id y

instance Monad Exp where
  return v = Exp $ return $ Result v []

  -- | Combine all the cells that were read.
  cmd >>= f = Exp $ do
    Result a cs <- runExp cmd
    Result b ds <- runExp (f a)
    return $ Result b (cs `union` ds)

-- | Boilerplate for monad.
instance Applicative Exp where
   (<*>) = ap
   pure = return

-- | Boilerplate for monad.
instance Functor Exp where
   fmap = liftM

-- | Construct a cell.
cell :: Exp a -> Exp (Cell a)
cell e = Exp $ do
  newCell <- Cell <$> newIORef e
                  <*> newIORef Nothing
                  <*> newIORef []
                  <*> newIORef []
                  <*> newUnique
  return $ Result newCell []

-- | Evaluate a cell to get its value.
get :: Cell a -> Exp a
get c = Exp $ do
  cValue <- readIORef (_value c)
  case cValue of
    Just v -> return $ Result v [Pack c]
    Nothing -> do
      Result v ds <- runExp =<< readIORef (_code c)
      writeIORef (_value c) (Just v)
      writeIORef (_reads c) ds
      mapM_ (\(Pack d) -> modifyIORef (_observers d) (Pack c :)) ds
      return $ Result v [Pack c]

-- | Remove a cell from another cell's observers.
removeObserver :: ECell -> ECell -> IO ()
removeObserver o (Pack c) =
  modifyIORef (_observers c) (filter (/= o))

-- | Recursively reset everything in the cell except for its code.
invalidate :: ECell -> IO ()
invalidate (Pack c) = do
  os <- readIORef (_observers c)
  rs <- readIORef (_reads c)
  writeIORef (_observers c) []
  writeIORef (_value c) Nothing
  writeIORef (_reads c) []
  mapM_ (removeObserver (Pack c)) rs
  mapM_ invalidate os

-- | Set a cell's code.
set :: Cell a -> Exp a -> IO ()
set c e = do
  writeIORef (_code c) e
  invalidate (Pack c)

-- | Evaluate an expression.
evalExp :: Exp a -> IO a
evalExp cmd = do
  Result a _ <- runExp cmd
  return a
