import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: (Ord k, Eq k, Eq v) => k -> m k v -> Maybe v
    insert :: (Ord k, Eq k, Eq v) => k -> v -> m k v -> m k v
    delete :: (Ord k, Eq k, Eq v) => k -> m k v -> m k v
    fromList :: (Ord k, Eq k, Eq v) => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)
    
instance MapLike ListMap where
    empty = ListMap []
    lookup key (ListMap []) = Nothing
    lookup key (ListMap (x:xs)) = if (key == fst x) then Just (snd x) else lookup key (ListMap xs)
    delete key (ListMap list)
                    | lookup key (ListMap list) == Nothing = ListMap $ list
                    | otherwise = helper 0 list
                                where
                                    helper n (x:xs) = if (fst x == key) then let (ys,zs) = splitAt n list in ListMap $ ys ++ xs else helper (n+1) xs
    insert key value (ListMap list)
                    | lookup key (ListMap list) == Nothing = ListMap $ (key,value) : list
                    | otherwise = helper 0 list
                                where
                                    helper n (x:xs) = if (fst x == key) then let (ys,zs) = splitAt n list in ListMap $ ys ++ ((key,value) : xs) else helper (n+1) xs
