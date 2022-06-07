--Sean
data Lista a = Vacia | Anadir a (Lista a)
  deriving Show
data Natural = Cero | Proximo Natural
  deriving Show
{-
La funcion conv recibe una cadena y la convierte en una lista de tipo Lista (Char,Int)
devolviendo la cantidad de cada una de las vocales que tiene la cadena, como se-}
{-ilustra en el ejemplo:
    conv::String ->Lista(Char,Int)
    conv "holaaa que tal" =>
    Anadir ('a',4)(Anadir('e',2)(Anadir('i',0)(Anadir('o',1)(Anadir('u',0) Vacia))))

Completar la definicion de conv, definiendo f y a
conv ys = f a ys
    where
      a ....
      f .....
-}

conv::String->Lista(Char,Int)
conv ys = foldr f a ys
  where
--  a = Vacia
    a = Anadir ('a',0)(Anadir('e',0)(Anadir('i',0)(Anadir('o',0)(Anadir('u',0) Vacia))))
    f::Char -> Lista (Char,Int)-> Lista (Char,Int)
    f x xs = if x == 'a' then mas 1 xs 
        else if x == 'e' then mas 2 xs
        else if x == 'i' then mas 3 xs
        else if x == 'o' then mas 4 xs
        else if x == 'u' then mas 5 xs else xs

--mas::Int ->Lista (Char,Int) -> Lista (Char,Int)
mas n xs = set n xs
--set::Int ->Lista (Char,Int) -> Lista (Char,Int)
set 1 (Anadir e xs) = Anadir (fst e, (snd e)+1) xs 
set n (Anadir e xs) = Anadir e (set (n-1) xs)
