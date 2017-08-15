import Data.List
data Prop a = Var a | Not (Prop a) | And (Prop a) (Prop a)
	| Or (Prop a)(Prop a) deriving Show

-- takes 4 real operators and go reads another argument
-- which is one of the Prop a constructors
foldProp :: (a->b) -> (b->b) -> (b->b->b) -> (b->b->b) -> Prop a 	-> b
foldProp fvar fnot fand for = go
	where
		go (Var name) = fvar name
		go (Not x) = fnot (go x)
		go (And x y) = fand (go x) (go y)
		go (Or x y) = for (go x) (go y)

-- (a->Bool) f.ex. (\x -> True)
evalProp :: (a->Bool) -> Prop a -> Bool
evalProp v = foldProp v not (&&) (||)

-- puts the name in a list, the right side of And is a list
-- with var names and right side as well
-- union -> unique names list
propVars :: Eq a => Prop a -> [a]
propVars = foldProp (\x->[x]) id union union

-- flip evaluates the function flipping the arguments first
-- elem returns true if the list contains an item with name of 
-- first parameter
-- any returns True if at least one item in the list fulfills
-- the condition
satProp :: Eq a => Prop a -> Bool
satProp p = any (\f -> evalProp f p) assg
	where assg = map (flip elem) (pow (propVars p))

-- create powerset of xs twice, one half add new element
pow :: [a] -> [[a]]
pow [] = [[]]
pow (x:xs) = (map (x:) (pow xs)) ++ (pow xs)




-- Assignment 4
data Tree t = Leaf | Node t (Tree t) (Tree t)

depthFirst :: Tree t -> [(Int,[t])]
depthFirst = go 0
	where
		go depth (Leaf) = [(depth,[])]
		go depth (Node x y z) = [((depth),[x])] ++ (go (depth+1) y) ++ (go (depth+1) z)

depthC :: Tree t -> Int -> [t]
depthC x c = [t | (depth,[t]) <- depthFirst (x), depth == c]

breathFirst :: Tree t -> [t]
breathFirst x = go 0 
	where 
		go currDepth
			| currDepth <= (depth x) = (depthC x currDepth)
								++ (go (currDepth+1))
			| otherwise = []
	

depth :: Tree t -> Int
depth x = maximum [t | (t,_) <- depthFirst x]

sortedTree :: Ord t => Tree t -> Bool
sortedTree = go
	where
		go (Leaf) = True 
		go (Node x (Leaf) (Leaf)) = True
		go (Node x l r) = (go l) && (go r) && (x>=parent l) 						&& (x<= parent r)	

parent :: Ord t => Tree t -> t
parent (Node x l r) = x












