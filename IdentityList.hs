module IdentityList where

import Data.Tuple.Extra (second)

data IL a b = IL a [(a, b)]

pushIL :: Num a => IL a b -> b -> IL a b
pushIL (IL next list) element = IL (next + 1) $ (next, element):list

removeIL :: Eq a => IL a b -> a -> IL a b
removeIL (IL next list) key = IL next $ filter ((/= key) . fst) list

instance Functor (IL a) where
  fmap func (IL next list) = IL next $ map (second func) list
