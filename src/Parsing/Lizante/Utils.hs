
{-# LANGUAGE RecordWildCards #-}

module Parsing.Lizante.Utils (
  Tree(..), treeList,
  (|>), (>+)
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
  | TreeList [Tree a]
  deriving (Show)

treeList :: Tree a
treeList = TreeList []

(>+) :: Tree a -> Tree a -> Tree a
(>+) Node{..} (TreeList xs) = Node
  { children = children ++ xs
  , ..
  }
(>+) Leaf{..} (TreeList xs) = Node
  { children = xs
  , ..
  }
(>+) (TreeList xs) (TreeList zs) = TreeList $ xs ++ zs
(>+) Node{..} x = Node
  { children = children ++ [x]
  , ..
  }
(>+) Leaf{..} x = Node
  { children = return x
  , ..
  }
(>+) (TreeList xs) x = TreeList $ xs ++ [x]
