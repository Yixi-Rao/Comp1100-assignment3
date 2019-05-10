module ListUtils (counts, pick) where

-- | Count the number of times each item appears in a list.
counts :: Eq a => [a] -> [(a, Int)]
counts [] = []
counts (x:xs) = (x, 1 + count):counts rest where
  count = length (filter (== x) xs)
  rest = filter (/= x) xs

-- | Remove the first occurrence of an element from a list, if it exists.
-- Return @Nothing@ if the element is not present.
--
-- NOTE: In the recursive call to @go@, we cons each non-matching item
-- onto the accumulating parameter (the first argument to @go@). If
-- you work through this with pencil and paper, you will see that this
-- builds up the arguments to @go@ in reverse order:
--
-- @
-- pick 3 [1, 2, 3, 4, 5]
-- ==> go [] 3 [1, 2, 3, 4, 5]
-- ==> go [1] 3 [2, 3, 4, 5]
-- ==> go [2, 1] 3 [3, 4, 5]
-- @
--
-- Now the head of the list [3, 4, 5] is the 3 we are looking for, but
-- we want to return a list with all the items in the same order so we
-- reverse the accumulated list [2, 1]:
--
-- @
-- ==> Just (reverse [2, 1] ++ [4, 5])
-- ==> Just ([1, 2] ++ [4, 5])
-- ==> Just [1, 2, 4, 5]
-- @
--
-- Why do we build the list up backwards and then reverse it? It's
-- because cons - '(:)' is cheap, and appending to the end of a list
-- using '(++)' is expensive. If you were to write an alternate
-- version of 'pick' that accumulated using '(++)' at the end, you
-- would find that it takes many more steps to complete.
pick :: Eq a => a -> [a] -> Maybe [a]
pick = go [] where
  go rest x (y:ys)
    | x == y = Just (reverse rest ++ ys)
    | otherwise = go (y:rest) x ys
  go _ _ [] = Nothing
