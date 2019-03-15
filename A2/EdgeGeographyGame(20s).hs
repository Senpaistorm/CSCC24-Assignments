module EdgeGeographyGame where

   -- You may import useful modules here.
   import Data.IntMap (IntMap)
   import qualified Data.IntMap as IntMap
   import Data.Set (Set)
   import qualified Data.Set as Set
    {- The input is a list of adjacency lists, e.g.,
       [ (0, [1, 2]) , (1, [0]) , (2, [1]), (3, []) ]
       means 0->1, 0->2, 1->0, 2->1, 3 has no outgoing edges.
    
       goodFirstVertices takes this input and computes the choices for the first
       vertex so the first player is destined to win.
    -}
   goodFirstVertices :: [(Int, [Int])] -> [Int]
   goodFirstVertices inp = gfvIntList (IntMap.keys mp) mp
      where mp = listToIntmap inp

   -- convert list of (Int, [Int]) to IntMap(Set Int)
   listToIntmap :: [(Int, [Int])] -> IntMap (Set Int)
   listToIntmap (x:xs) = IntMap.map (\y -> Set.fromList y) (IntMap.fromList (x:xs))

   -- given a list of starting vertices and an IntMap, return the good vertices
   gfvIntList :: [Int] -> IntMap (Set Int) -> [Int]
   gfvIntList [] g = []
   gfvIntList (x:xs) g 
      | gfv x Set.empty g = x : gfvIntList xs g
      | otherwise = gfvIntList xs g

   gfvLazy :: [Int] -> Int -> Set(Int,Int) -> IntMap (Set Int) -> Bool
   gfvLazy [] _ _ _ = False
   gfvLazy adj@(x:xs) v edges g
      | Set.member (v,x) edges == False && gfv x (Set.insert (v,x) edges) g = True
      | otherwise = gfvLazy xs v edges g 

   -- given on vertex and an IntMap, return whether it is a good vertex
   gfv :: Int -> Set(Int,Int) -> IntMap (Set Int) -> Bool
   gfv x edges g 
    --  | adj == [] = True
      -- | otherwise = False
      -- | otherwise = Set.foldr (&&) True (Set.map (\v -> winnable v (IntMap.adjust (Set.delete v) x g)) adj)
      | winnableLazy adj x edges g = True
      | otherwise = False
      where 
         adj = Set.toList (g IntMap.! x)
   
   winnableLazy :: [Int] -> Int -> Set(Int,Int) -> IntMap(Set Int) -> Bool
   winnableLazy [] _ _ _ = True
   winnableLazy adj@(x:xs) v edges g
      | Set.member (v,x) edges == False && winnable x (Set.insert (v,x) edges) g == False = False
      | otherwise = winnableLazy xs v edges g 

   winnable :: Int -> Set(Int,Int) -> IntMap(Set Int) -> Bool
   winnable x edges g 
     -- | adj == [] = False
      -- | otherwise = Set.foldr (||) False (Set.map (\v -> gfv v (IntMap.adjust (Set.delete v) x g)) adj)
      | gfvLazy adj x edges g = True
      | otherwise = False
      where 
         adj = Set.toList (g IntMap.! x)
      