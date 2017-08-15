data Prop a = Var a| Not (Prop a) | Or (Prop a) (Prop a) |
			And (Prop a) (Prop a) 
			deriving Show

pwr :: [Int] -> [[Int]]
pwr [] = [[]]
pwr (x:xs) = map (x:) (pwr xs) ++ (pwr xs)

data Tree a = Leaf | Node a (Tree a) (Tree a)
	deriving (Eq,Ord,Show)

sortt :: Ord t => Tree t -> Bool
sortt t = go t
 where 
	go (Leaf) = True
	go (Node a (Node b c d) (Node e f g)) = (sortt (Node b c d)) && (sortt (Node e f g)) &&  (b <= a) && (e >= a)
	go (Node a (Node b c d) (Leaf)) = (sortt (Node b c d)) && (a >= b)
	go (Node a (Leaf) (Node b c d)) = (sortt (Node b c d)) && (a<=b)
	go (Node a (Leaf) (Leaf)) = True


