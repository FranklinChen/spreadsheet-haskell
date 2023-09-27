{-# LANGUAGE GADTs #-}

-- | Haskell version of spreadsheet demo from <https://semantic-domain.blogspot.com/2015/07/how-to-implement-spreadsheet.html Neel Krishnaswami's blog post>.
module Spreadsheet (
    -- * Types
    Cell,
    Exp,

    -- * Cell operations
    cell,
    get,
    set,

    -- * Expression elimination
    evalExp,
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

{- | Container for a value that can depend on other cells
through a code expression.

Has a unique identity, even across different cell types.
-}
data Cell a = Cell
    { _code :: IORef (Exp a)
    -- ^ expression to run
    , _value :: IORef (Maybe a)
    -- ^ value memoized from running code
    , _reads :: IORef [ECell]
    -- ^ cells that were read
    , _observers :: IORef [ECell]
    -- ^ cells that read this cell
    , _id :: Unique
    -- ^ globally unique token
    }

-- | A computed result, along with the cells read during computation.
data Result a = Result a [ECell]

-- | An expression that is run to give a result.
newtype Exp a = Exp {runExp :: IO (Result a)}

{- | Existential type: "a cell of some type".
Used for a heterogeneous list of different types of cells.
-}
data ECell where
    Pack :: Cell a -> ECell

-- | Compare cells using ID field.
instance Eq ECell where
    Pack x == Pack y = _id x == _id y

instance Monad Exp where
    return = pure
    cmd >>= f = Exp $ do
        Result a cs <- runExp cmd
        Result b ds <- runExp (f a)
        pure $ Result b (cs `union` ds)

-- | Boilerplate for monad.
instance Applicative Exp where
    (<*>) = ap
    pure v = Exp $ pure $ Result v []

-- | Boilerplate for monad.
instance Functor Exp where
    fmap = liftM

-- | Construct a cell.
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

-- | Evaluate a cell to get its value.
get :: Cell a -> Exp a
get c = Exp $ do
    cValue <- readIORef (_value c)
    case cValue of
        Just v -> pure $ Result v [Pack c]
        Nothing -> do
            Result v ds <- runExp =<< readIORef (_code c)
            writeIORef (_value c) (Just v)
            writeIORef (_reads c) ds
            traverse_ (\(Pack d) -> modifyIORef (_observers d) (Pack c :)) ds
            pure $ Result v [Pack c]

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
    traverse_ (removeObserver (Pack c)) rs
    traverse_ invalidate os

-- | Set a cell's code.
set :: Cell a -> Exp a -> IO ()
set c e = do
    writeIORef (_code c) e
    invalidate (Pack c)

-- | Evaluate an expression.
evalExp :: Exp a -> IO a
evalExp cmd = do
    Result a _ <- runExp cmd
    pure a
