lista = [1,2,3,4,5,6,7,8,9]
lisLista = [[1,2,3],[4,5,6],[7,8],[9]]
prueba (x:xs) = [xs]
--Ejercicio 1
listaIgual::[Int] -> [Int] -> Bool
listaIgual [] [] = True
listaIgual [] ys = False
listaIgual xs [] = False
listaIgual (x:xs) (y:ys) = if x == y then listaIgual xs ys else False

--Ejercicio 2
fusionarListas::[Int] -> [Int] -> [Int]
fusionarListas [] [] = []
fusionarListas [] ys = ys
fusionarListas xs [] = xs
fusionarListas (x:xs) (y:ys) 
 |x<y = x:(fusionarListas xs (y:ys))
 |otherwise = y:(fusionarListas (x:xs) ys)

{-  ejercicio 3
    
    esMatriz [] = True
    0 X 0
    1 X 1 => [[...]] 1
    2 X 2 => [[...], [...]] 2
    3 X 3 => [[...], [...], [...]] 3
    esMatriz xss = fn (xss) (length xss)
        where
            fn [] tamMatriz = True
            fn (xs : xss) tamMatriz = if length(xs) == tamMatriz then fn (xss) (tamMatriz) else False
-}

esMatriz :: [[Integer]] -> Bool
esMatriz [] = True
esMatriz xss = fn (xss) (length xss)
        where
            fn [] tamMatriz = True
            fn (xs : xss) tamMatriz = if length(xs) == tamMatriz then fn (xss) (tamMatriz) else False
 
 --Ejercicio 5
soloPares::[Int]->[Int]
soloPares [] = []
soloPares (x:xs) = if ((x `mod` 2)==0) then x:(soloPares xs) else soloPares xs

--Ejercicio 6 listas de listas
longitudPar::[[Integer]]->[[Integer]]
longitudPar [[]] = [[]]
longitudPar (x:xs) = if ((length(x) `mod` 2)==0) then x:(longitudPar xs) else longitudPar xs

--Ejercicio 7
borrarElem::Integer->[Integer]->[Integer]
borrarElem _ [] = []
borrarElem n (x:xs) = if n==x then borrarElem n xs else x:borrarElem n xs 





-- 8. Definir una función que reciba una lista de listas y devuelva una lista formada
-- por los penúltimos elementos de las listas
{-
    Entrada: [[1,2,3], [4, 5, 6], [7, 8, 9]]
    Salida: [2, 5, 8]

    -- 1. Definir una funcion que saque penultimos elementos de una lista
    -- 2. Recorrer la matriz y aplicar la funcion penultimo "penultimo"
    -- 3. Todos los resultados se deben agregar a una lista
-}

penultimo :: [a] -> a
penultimo xs = xs !! (length(xs) - 2)

ej8 :: [[Integer]] -> [Integer]
ej8 [] = []
ej8 (xs : xss) = penultimo xs : ej8 xss




--Ejercicio 9 posibles divisores
--Lista Comprension
divisores :: Int -> [Int]
divisores x = 
    [y | y <- [1..a], x `mod` y == 0]
    where a = x `div` 2

miLength::[Int]->Int
miLength [] = 0
miLength (x:xs) = 1 + miLength xs

--Recursivo
divisor n = posDiv n n
  where
    posDiv n 0 = []
    posDiv n m = if (n `mod` m)==0 then m:(posDiv n (m-1)) else posDiv n (m-1)

--posDivisores::Int -> [Int]


--ejercicio10
buscarElem n xs = elem n xs 0
  where
    elem n [] m  = (-1)
    elem n (x:xs) m = if (n == x) then m else (elem n xs (m+1))

--ejercicio13
{-
 ordenarInsercion [2,3,1]
insertar 2 (ordenarInsercion [3,1])
insertar 2 (insertar 3(ordenarInsercion [1]))
insertar 2 (insertar 3 (insertar 1(ordenarInsercion [])))
insertar 2 (insertar 3 (insertar 1([])
insertar 2 (insertar 3 ([1]))
insertar 2 (1:(insertar 3 []))
insertar 2 (1:([3]))
insertar 2 [1,3]
1:(insertar 2 [3])
1:(2:(3:[]))
[1,2,3]
-}
ordenarInsercion [] = []
ordenarInsercion (x:xs) = insertar x (ordenarInsercion xs)

insertar e [] = [e]
insertar e (x:xs) = if e<=x then e:(x:xs) else x:insertar e xs


{-
insertar 3 [1,2,5,3,1]
1:(insertar 3 [2,5,3,1])
1:(2:(insertar 3 [5,3,1]))
1:(2:(3:(5:[3,1])))

-}
 
concatenar::[[a]]->[a]
concatenar xss = [x | xs <- xss, x <- xs]

longitud xs = sum [1 | x <- xs]

factores :: Int -> [Int]
factores x = [y | y <- [1..x], x `mod` y == 0]

primo :: Int -> Bool
primo x = factores x == [1,x]

primos :: Int -> [Int]
primos x = [y | y <- [2..x], primo y]

