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

--Ejercicio 9 posibles divisores
divisores :: Int -> [Int]
divisores x = 
    [y | y <- [1..a], x `mod` y == 0]
    where a = x `div` 2
    
concatenar::[[a]]->[a]
concatenar xss = [x | xs <- xss, x <- xs]

longitud xs = sum [1 | x <- xs]

longitud1::[a]->Integer
longitud1 [] = 0
longitud1 (x:xs) = 1 + (longitud1 xs)

factores :: Int -> [Int]
factores x = [y | y <- [1..x], x `mod` y == 0]

primo :: Int -> Bool
primo x = factores x == [1,x]

primos :: Int -> [Int]
primos x = [y | y <- [2..x], primo y]

