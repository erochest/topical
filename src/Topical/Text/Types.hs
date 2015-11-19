{-# LANGUAGE TemplateHaskell #-}


module Topical.Text.Types
    ( Tokenizer

    , Tree(..)
    , _EmptyTree
    , _Node
    , nodeData
    , nodeLeft
    , nodeRight

    , unfoldTree
    , nlr
    , lnr
    ) where


import           Control.Lens

import qualified Data.Text    as T


type Tokenizer = T.Text -> [T.Text]

data Tree a = EmptyTree
            | Node { _nodeData  :: a
                   , _nodeLeft  :: Tree a
                   , _nodeRight :: Tree a
                   }
            deriving (Show, Eq)
makePrisms ''Tree
makeLenses ''Tree

instance Functor Tree where
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)
    fmap _ EmptyTree    = EmptyTree

instance Applicative Tree where
    pure x = Node x EmptyTree EmptyTree

    (Node f lf rf) <*> (Node x lx rx) = Node (f x) (lf <*> lx) (rf <*> rx)
    EmptyTree      <*> _              = EmptyTree
    _              <*> EmptyTree      = EmptyTree

nlr :: Tree a -> [a]
nlr EmptyTree = []
nlr (Node n l r) = n : (nlr l) ++ (nlr r)

lnr :: Tree a -> [a]
lnr EmptyTree = []
lnr (Node n l r) = (nlr l) ++ n : (nlr r)

unfoldTree :: (a -> (b, (Maybe a, Maybe a))) -> a -> Tree b
unfoldTree f a = Node n l' r'
    where
        (n, (l, r)) = f a
        l' = maybe EmptyTree (unfoldTree f) l
        r' = maybe EmptyTree (unfoldTree f) r
