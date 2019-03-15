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
   goodFirstVertices inp = gfvIntMap (IntMap.keys mp) mp
      where mp = listToIntmap inp

   -- -- convert list of (Int, [Int]) to IntMap[Int]
   listToIntmap :: [(Int, [Int])] -> IntMap [Int]
   listToIntmap (x:xs) =  (IntMap.fromList (x:xs))

   -- given a list of starting vertices and an IntMap, return the good vertices
   gfvIntMap :: [Int] -> IntMap [Int] -> [Int]
   gfvIntMap [] g = []
   gfvIntMap (x:xs) g 
      | gfvWinAll (g IntMap.! x) x g = x: gfvIntMap xs g
      | otherwise = gfvIntMap xs g
   -- compute if I have a winning move for every path that opponent can take from v 
   gfvWinAll :: [Int] -> Int -> IntMap [Int] -> Bool
   gfvWinAll [] _ _ = True
   gfvWinAll adj@(x:xs) v g 
      | hasWinningMove x_adj g = gfvWinAll xs v g 
      | otherwise = gfvWinOne x_adj x gnew && gfvWinAll xs v g 
      where 
         gnew = (IntMap.adjust (filter (\y -> y/= x)) v g)
         x_adj = (g IntMap.! x)
   -- compute if I have at least one winning move for every path that I can take from v
   gfvWinOne :: [Int] -> Int -> IntMap[Int] -> Bool
   gfvWinOne [] _ _ = False
   gfvWinOne adj@(x:xs) v g
      | hasWinningMove x_adj g = gfvWinOne xs v g
      | otherwise = gfvWinAll x_adj x gnew || gfvWinOne xs v g
      where
         gnew = (IntMap.adjust (filter (\y -> y/= x)) v g)
         x_adj = (g IntMap.! x)
   -- given a list and a graph, check if there is at least one winning move 
   -- (empty list after making the move)
   hasWinningMove :: [Int] -> IntMap[Int] -> Bool
   hasWinningMove [] _ = False
   hasWinningMove adj@(x:xs) g = g IntMap.! x == [] || hasWinningMove xs g
      