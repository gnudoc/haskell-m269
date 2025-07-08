module MyLib (
  TreeBT01(..),
  TreeBT(..)
  ) where


data TreeBT01 a = EmptyBT01
                | NodeBT01 a (TreeBT01 a) (TreeBT01 a)
                deriving (Eq, Read, Show)


data TreeBT a = EmptyBT
              | NodeBT {
                  getDataBT :: a,
                  getLeftBT :: (TreeBT a),
                  getRightBT :: (TreeBT a)
                  }
              deriving (Eq, Read, Show)

mkEmptyBT :: TreeBT a
mkEmptyBT = EmptyBT

mkNodeBT :: a -> TreeBT a -> TreeBT a -> TreeBT a
mkNodeBT x t1 t2 = NodeBT x t1 t2

isEmptyBT :: Eq a => TreeBT a -> Bool
isEmptyBT t = t == EmptyBT
