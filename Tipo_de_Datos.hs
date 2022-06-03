
-- TIPOS COMPUESTOS Y RECURSIVOS
data Nota = Maestria Char | Cato Int | Umss Int
  deriving Show

--Ejercicio 1
--DEfinir una funcion que reciba una nota y devuelva verdad (True) si esta es de la umss
esNota (Umss n) = True
esNota (Cato n) = False

--Ejercicio 2
--Definir una fun que reciba una nota y devuelva su valor solo en caso que sea de la Cato
--o de Umss. En caso de ser de la Cato, que devuelva el promedio de las dos notas
esNota2 (Umss n) = n
esNota2 (Cato m) = (m + esNota2 (Umss 40)) `div` 2

--Ejercicio 3
--Definir una funcion que ingrese una lista de notas y devuelva las notas aprobadas
notasDeAprobacion::[Nota]->[Nota]
notasDeAprobacion []     = []
notasDeAprobacion (x:xs) = if (esAprobado x) then x:(notasDeAprobacion xs) else notasDeAprobacion xs
 where
   esAprobado (Maestria c)  = c /= 'D'
   esAprobado (Cato  n)   = n > 50

{-

[Int]                   Lista Enteros
____________________________________________________________
| []                | Vacia                                |
| 2:[] = [2]        | Añadir 2 Vacia                       |
| 5:[2] = [5,2]     | Añadir 5 (Añadir 2 Vacia)            |
| 3:[5,2] = [3,5,2] | Añadir 3 (Añadir 5 (Añadir 2 Vacia)) |
|__________________________________________________________|
-}

--data ListaEneteros = Vacia | Anadir Int ListaEnteros
data Lista a = Vacia | Anadir a (Lista a)
  deriving Show

cabeza::Lista a -> a
cabeza (Anadir x xs) = x

toLista::[a] -> Lista a
toLista[]      = Vacia
toLista (x:xs) = Anadir x (toLista xs)
{-
toLista [1,2,3]
Anadir 1 (toLista [2,3])
Anadir 1 (Anadir 2(toLista [3]))
Anadir 1 (Anadir 2 (Anadir 3 (toLista [])))
Anadir 1 (Anadir 2 (Anadir 3 (Vacia)))
-}
--existeElem:: Int->[Int]->Bool
--existeElem n [] = False
--existeElem n (x:xs) = if n==x then True else (existeElem n xs)

existeElem::Int->Lista Int -> Bool
existeElem n Vacia         = False
existeElem n (Anadir x xs) = if n==x then True else (existeElem n xs) 

borrarElem n Vacia         = Vacia
borrarElem n (Anadir x xs) = if n==x then xs else Anadir x (borrarElem n xs)

--sumarElem []     = 0
--sumarElem (x:xs) = x + sumarElem xs
sumarElem Vacia         = 0
sumarElem (Anadir x xs) =  x + sumarElem xs
{-
sumarElem [1,2,3]
1 + sumarElem [2,3]
1 + (2 + sumarElem [3])
1 + (2 + (3 + sumarElem []))
1 + (2 + (3 + 0))
6
-}

ordenarInsercion Vacia         = Vacia
ordenarInsercion (Anadir x xs) = insertar x (ordenarInsercion xs)

insertar e Vacia         = (Anadir e Vacia)
insertar e (Anadir x xs) = if e<=x then Anadir e (Anadir x xs) else Anadir x (insertar e xs)

--divisores :: Int -> [Int]
--divisores x = [y | y <- [1..x], x `mod` y == 0]

{-
divisores n = posDiv n n
  where
    posDiv n 0 = []
    posDiv n m = if (n `mod` m) == 0 then m:(posDiv n (m-1)) else posDiv n (m-1)
-}

divisores n = posDiv  n n
  where
    posDiv n 0 = Vacia
    posDiv n m = if (n `mod` m)==0 then Anadir m (posDiv n (m-1)) else posDiv n (m-1)

--primo :: Int -> Bool
--primo x = divisores x == [x,1]
primo x
 | x == 1 = True
 | x == 2 = False
 | miLength(divisores x) == 2 = True
 | otherwise = False
miLength Vacia = 0
miLength (Anadir x xs) = 1 + miLength xs

--soloPrimo []     = []
--soloPrimo (x:xs) = if (primo x)==True then x:(soloPrimo xs) else soloPrimo xs
soloPrimo Vacia         = Vacia
soloPrimo (Anadir x xs) = if (primo x)==True then Anadir x (soloPrimo xs) else soloPrimo xs 
