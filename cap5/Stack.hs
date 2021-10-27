module Stack (Stack, -- exportar o tipo mas não o construtor
push, pop, top, -- exportar as operações
empty, isEmpty) where

data Stack a = Stk [a] deriving Show

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _)  = False
