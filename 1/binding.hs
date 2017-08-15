f y = x + 7 * y
x = 1
g x = x + (let y = x * 2; x = 5 in x + y)
h z 
  | z == 0    = g z
  | otherwise =
    let g v = v + 3
    in g z
  where g x = x + 2

{-
Binding remarks:

- The use of x in f's body refers to the global x *later* in the program
- In g's body, the x in "y = x * 2" refers to the x from "x = 5". Declarations
  in one let are mutually recursive, declarations in nested let's are not.
- The where in h's definition extends across all guards, so even in z == 0, 
  the g refers to the where definition, not the global one.
- let binds stronger than where, i.e., g z in the otherwise case refers to
  the let-bound g.
-}