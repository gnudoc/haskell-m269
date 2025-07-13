module Haskellcourse202503BT where

-- -- --
-- Let's define a binary tree type
-- -- --
data TreeBT a = EmptyBT
              | NodeBT {
                  getDataBT  :: a,
                  getLeftBT  :: TreeBT a,
                  getRightBT :: TreeBT a
              }
              deriving (Eq, Read, Show)

mkEmptyBT :: TreeBT a
mkEmptyBT = EmptyBT

mkNodeBT :: a -> TreeBT a -> TreeBT a -> TreeBT a
mkNodeBT x t1 t2 = NodeBT x t1 t2

isEmptyBT :: TreeBT a -> Bool
isEmptyBT EmptyBT                   = True
isEmptyBT (NodeBT x leftBT rightBT) = False

heightBT :: TreeBT a -> Int
heightBT EmptyBT = 0
heightBT (NodeBT nodeX leftBT rightBT)
                 = 1 + max (heightBT leftBT) (heightBT rightBT)

sizeBT :: TreeBT a -> Int
sizeBT EmptyBT                       = 0
sizeBT (NodeBT nodeX leftBT rightBT) = 1 + sizeBT leftBT + sizeBT rightBT

exposeBT :: TreeBT a -> (a, TreeBT a, TreeBT a)
exposeBT EmptyBT                   = error " exposeBT applied to EmptyBT"
exposeBT (NodeBT a leftBT rightBT) = (a, leftBT, rightBT)
-- -- --

-- -- --
egBSTree :: TreeBT Char
egBSTree = mkNodeBT 'H'
             (mkNodeBT 'D'
               (mkNodeBT 'B'
                 (mkNodeBT 'A' mkEmptyBT mkEmptyBT)
                 (mkNodeBT 'C' mkEmptyBT mkEmptyBT))
               (mkNodeBT 'F'
                 (mkNodeBT 'E' mkEmptyBT mkEmptyBT)
                 (mkNodeBT 'G' mkEmptyBT mkEmptyBT))
             )
             (mkNodeBT 'L'
               (mkNodeBT 'J'
                 (mkNodeBT 'I' mkEmptyBT mkEmptyBT)
                 (mkNodeBT 'K' mkEmptyBT mkEmptyBT))
               (mkNodeBT 'N'
                 (mkNodeBT 'M' mkEmptyBT mkEmptyBT)
                 (mkNodeBT 'O' mkEmptyBT mkEmptyBT))
             )
-- -- --

-- -- --
inOrderBT :: TreeBT a -> [a]
inOrderBT EmptyBT = []
inOrderBT (NodeBT nodeX leftBT rightBT)
  = inOrderBT leftBT ++ [nodeX] ++ inOrderBT rightBT

preOrderBT :: TreeBT a -> [a]
preOrderBT EmptyBT = []
preOrderBT (NodeBT nodeX leftBT rightBT)
  = [nodeX] ++ preOrderBT leftBT ++ preOrderBT rightBT


postOrderBT :: TreeBT a -> [a]
postOrderBT EmptyBT = []
postOrderBT (NodeBT nodeX leftBT rightBT)
  = postOrderBT leftBT ++ postOrderBT rightBT ++ [nodeX]
-- -- --

-- -- --
levelOrderBT :: TreeBT a -> [[a]]
levelOrderBT EmptyBT = []
levelOrderBT (NodeBT x leftBT rightBT)
  = [[x]] ++ longZipMerge (levelOrderBT leftBT) (levelOrderBT rightBT)

longZipMerge :: [[a]] -> [[a]] -> [[a]]
longZipMerge [] yss = yss
longZipMerge xss [] = xss
longZipMerge (xs:xss) (ys:yss)
  = [xs ++ ys] ++ longZipMerge xss yss

flattenLevels :: [[a]] -> [a]
flattenLevels levels = [elem | level <- levels, elem <- level]

breadthBT :: TreeBT a -> [a]
breadthBT t = (flattenLevels . levelOrderBT) t
-- -- --

labelsAtDepth :: Int -> TreeBT a -> [a]
labelsAtDepth d EmptyBT = []
labelsAtDepth d (NodeBT x leftBT rightBT)
  | d == 0    = [x]
  | otherwise = (labelsAtDepth (d-1) leftBT)
             ++ (labelsAtDepth (d-1) rightBT)

bfTravByLevel :: TreeBT a -> [a]
bfTravByLevel t = bfTbyL 0 t
bfTbyL :: Int -> TreeBT a -> [a]
bfTbyL d t
  | null xs    = []
  | otherwise  = xs ++ bfTbyL (d+1) t
      where xs = labelsAtDepth d t
--------
--------

isLeaf :: TreeBT a -> Bool
isLeaf EmptyBT    = False
isLeaf (NodeBT _ EmptyBT EmptyBT) = True
isLeaf (NodeBT _ _ _) = False
--

numLeaves :: TreeBT a -> Int
numLeaves EmptyBT = 0
numLeaves (NodeBT _ EmptyBT EmptyBT) = 1
numLeaves (NodeBT _ leftBT rightBT) = numLeaves leftBT + numLeaves rightBT
--

numLeavesAtLevel :: Int -> TreeBT a -> Int
numLeavesAtLevel target tree = count 0 tree
  where
    count :: Int -> TreeBT a -> Int
    count currentLevel EmptyBT = 0
    count currentLevel (NodeBT x leftBT rightBT)
      | currentLevel == target =
          if isLeaf (NodeBT x leftBT rightBT) then 1 else 0
      | currentLevel < target  =
          count (currentLevel+1) leftBT + count (currentLevel+1) rightBT
      | otherwise              = 0

numLeavesAtLevel' :: Int -> TreeBT a -> Int
numLeavesAtLevel' _ EmptyBT = 0
numLeavesAtLevel' d t@(NodeBT _ leftBT rightBT)
  | d == 0    = if isLeaf t then 1 else 0
  | d > 0     = numLeavesAtLevel' (d-1) leftBT + numLeavesAtLevel' (d-1) rightBT
  | otherwise = 0

isFull :: TreeBT a -> Bool
isFull EmptyBT                   = True
isFull (NodeBT _ leftBT rightBT) =
  case (isEmptyBT leftBT, isEmptyBT rightBT) of
    (True, True)                 -> True
    (False, False)               -> isFull leftBT && isFull rightBT
    _                            -> False

isFull' :: TreeBT a -> Bool
isFull' EmptyBT                       = True
isFull' (NodeBT _ EmptyBT EmptyBT)    = True
isFull' (NodeBT _ leftBT rightBT)
  | isFull' leftBT && isFull' rightBT = True
  | otherwise                         = False
