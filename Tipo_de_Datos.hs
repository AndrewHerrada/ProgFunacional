{-# LANGUAGE OverloadedStrings #-}
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


