module TrieDelete where

import           TrieDef

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

trieDelete :: [Char] -> Trie a -> Trie a

trieDelete [] (TrieNode v children) = TrieNode Nothing children

trieDelete (x:xs) a@(TrieNode v children)
    -- if x exists and t is non empty, replace children[x] with t
    | Map.member x children && trieIsEmpty t == False = TrieNode v (Map.insert x (t) children)
    -- if x exists and has and empty trie, delete the key x and its trie
    | Map.member x children && trieIsEmpty t = TrieNode v (Map.delete x children)
    -- x does not exist, return original trie
    | otherwise = a
    -- compute t as recursively calling delete on xs and child node
    where t = trieDelete xs (children Map.! x)