
{-# LANGUAGE RecordWildCards #-}

module Parsing.Lizante.Utils (
  Tree(..),
  (|>)
             ) where

-- OPs

infixl 3 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x


-- Trees
data Tree a
  = Node
    { value    :: a
    , children :: [Tree a]
    }
  | Leaf
    { value    :: a
    }

(>+) :: Tree a -> Tree a -> Tree a
(>+) Node{..} x = Node
  { children = x : children
  , ..
  }
(>+) Leaf{..} x = Node
  { children = return x
  , ..
  }
