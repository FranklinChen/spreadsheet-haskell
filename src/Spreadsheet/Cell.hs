{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Reactive dataflow spreadsheet cells.
--
-- This module implements a reactive dataflow system where cells hold cached
-- computations that automatically track their dependencies. When a cell is
-- updated via 'set', all transitive dependents are recursively invalidated so
-- that subsequent reads recompute fresh values.
--
-- Build dependency graphs declaratively using the monadic interface
-- ('return', '>>=', 'cell', 'get', 'set', 'run').
--
-- Haskell translation of
-- <https://semantic-domain.blogspot.com/2015/07/how-to-implement-spreadsheet.html Neel Krishnaswami's spreadsheet>.
module Spreadsheet.Cell
    ( -- * Types
      Cell
    , Exp

      -- * Cell operations
    , cell
    , get
    , set

      -- * Expression elimination
    , run
    )
where

import Control.Monad (ap, liftM)
import Data.Foldable (traverse_)
import Data.IORef (
    IORef,
    modifyIORef,
    newIORef,
    readIORef,
    writeIORef,
 )
import Data.List (union)
import Data.Unique (Unique, newUnique)

-- | A cell is a mutable container that holds a cached value of type @a@,
-- together with its defining computation and dependency tracking information.
data Cell a = Cell
    { code :: IORef (Exp a)
    -- ^ expression to run
    , value :: IORef (Maybe a)
    -- ^ value memoized from running code
    , reads :: IORef [ECell]
    -- ^ cells that were read
    , observers :: IORef [ECell]
    -- ^ cells that read this cell
    , id :: Unique
    -- ^ globally unique token
    }

-- | A computed result, along with the cells read during computation.
data Result a = Result a [ECell]

-- | An expression is a computation that produces a value of type @a@ while
-- recording which cells were read during evaluation.
newtype Exp a = Exp (IO (Result a))

runExp :: Exp a -> IO (Result a)
runExp (Exp io) = io

{- | Existential type: "a cell of some type".
Used for a heterogeneous list of different types of cells.
-}
data ECell where
    Pack :: Cell a -> ECell

-- | Compare cells using ID field.
instance Eq ECell where
    Pack x == Pack y = x.id == y.id

instance Monad Exp where
    cmd >>= f = Exp $ do
        Result a cs <- runExp cmd
        Result b ds <- runExp (f a)
        pure $ Result b (cs `union` ds)

instance Applicative Exp where
    (<*>) = ap
    pure v = Exp $ pure $ Result v []

instance Functor Exp where
    fmap = liftM

-- | Create a new reactive cell whose value is defined by the given
-- expression. The cell is initially unevaluated; its computation runs
-- lazily the first time the cell is read via 'get'.
cell :: Exp a -> Exp (Cell a)
cell e = Exp $ do
    newCell <-
        Cell
            <$> newIORef e
            <*> newIORef Nothing
            <*> newIORef []
            <*> newIORef []
            <*> newUnique
    pure $ Result newCell []

-- | Read the current value of a cell. If the value is cached, it is
-- returned immediately. Otherwise the cell's computation is evaluated, the
-- result is cached, and the cell registers itself as an observer of every
-- cell that was read during evaluation.
get :: Cell a -> Exp a
get c = Exp $ do
    cValue <- readIORef c.value
    case cValue of
        Just v -> pure $ Result v [Pack c]
        Nothing -> do
            Result v ds <- runExp =<< readIORef c.code
            writeIORef c.value (Just v)
            writeIORef c.reads ds
            traverse_ (\(Pack d) -> modifyIORef d.observers (Pack c :)) ds
            pure $ Result v [Pack c]

-- | Remove a cell from another cell's observers.
removeObserver :: ECell -> ECell -> IO ()
removeObserver o (Pack c) =
    modifyIORef c.observers (filter (/= o))

-- | Recursively reset everything in the cell except for its code.
invalidate :: ECell -> IO ()
invalidate (Pack c) = do
    os <- readIORef c.observers
    rs <- readIORef c.reads
    writeIORef c.observers []
    writeIORef c.value Nothing
    writeIORef c.reads []
    traverse_ (removeObserver (Pack c)) rs
    traverse_ invalidate os

-- | Replace the computation of a cell and recursively invalidate all
-- transitive dependents so that the next 'get' will recompute.
set :: Cell a -> Exp a -> IO ()
set c e = do
    writeIORef c.code e
    invalidate (Pack c)

-- | Evaluate an expression and return its value, discarding the read set.
-- Use this at the top level to extract a result.
run :: Exp a -> IO a
run cmd = do
    Result a _ <- runExp cmd
    pure a
