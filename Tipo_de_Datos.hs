
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
notasDeAprobacion (x:xs) = if esAprobado x then x:notasDeAprobacion xs else notasDeAprobacion xs
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
existeElem n (Anadir x xs) = if n==x then True else existeElem n xs

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

insertar e Vacia         = Anadir e Vacia
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

{-
     Grafico                   Tipo Arbol
__________________________________________________________
|                   |                                    | 
|       10          | Hoja 10                            |
|___________________|____________________________________|
|       /  \        |                                    |
|      10  20       |Rama (Hoja 10) (Hoja 20)            |
|___________________|____________________________________|
|      / \          |                                    |
|     /   5         |                                    |
|    /\             |Rama(Rama(Hoja 10)(Hoja 20))(Hoja 5)|
|  10 20            |                                    |
|___________________|____________________________________|
-}
data Arbol a = Hoja a | Rama (Arbol a) (Arbol a)
  deriving Show

arb = Rama (Hoja 5) (Rama (Rama(Hoja 2)(Hoja 10))(Hoja 30))
arb1 = Rama (Hoja 5) (Rama (Rama(Hoja 2)(Hoja 11))(Hoja 30))

totalHojas::Arbol Integer -> Integer
totalHojas (Hoja x)    = 1
totalHojas (Rama ai ad) = (totalHojas ai) + (totalHojas ad)

sumatoriaHojas::Arbol Integer -> Integer
sumatoriaHojas (Hoja x)     = x
sumatoriaHojas (Rama ai ad) = (sumatoriaHojas ai) + (sumatoriaHojas ad)

{-
sumatoriaHojas Rama(Rama(Hoja 10)(Hoja 20))(Hoja 5)
ai = (Rama(Hoja 10)(Hoja 20))
ad = Hoja 5
sumatoriaHojas (Rama(Hoja 10)(Hoja 20)) + sumatoriaHojas (Hoja 5)
sumatoriaHojas (Rama(Hoja 10)(Hoja 20)) + 5
ai = Hoja 10
ad = Hoja 20
(sumatoriaHojas (Hoja 10) + sumatoriaHojas (Hoja 20)) + 5
                10        +           20                5
= 35
-}

--comparar [] []        = True
--comparar [] ys        = False
--comparar xs []        = False
--comparar (x:xs)(y:ys) = if x==y then comparar xs ys else False

compararArboles (Hoja a) (Hoja b)                     = if a == b then True else False 
compararArboles (Rama (ai1) (ad1)) (Rama (ai2) (ad2)) = compararArboles ai1 ai2 && compararArboles ad1 ad2
compararArboles _ _                                   = False


preorden::Arbol a -> [a]
preorden (Hoja x)     = [x]
preorden (Rama ai ad) = (preorden ai ++ preorden ad)
{-
preorden Rama(Rama(Hoja 10)(Hoja 20))(Hoja 5)
ai = Rama(Rama(Hoja 10)(Hoja 20))
ad = Hoja 5
preorden (Rama(Hoja 10)(Hoja 20)) ++ preorden Hoja 5
preorden (Rama(Hoja 10)(Hoja 20)) ++ [5]
ai = Hoja 10
ad = Hoja 20
preorden ((Hoja 10) ++ (Hoja 20)) ++ [5]
preorden ([10] ++ [20]) ++ [5]
preorden [10,20,5]
-}

{-Arbol de busqueda
-------------------------------------------\
    .    Hoja                              |
-------------------------------------------|
   20                                      |
  /  \   Rama 20 Hoja Hoja                 |
 .    .                                    |
-------------------------------------------|
   10                                      |
  /  \   Rama 10 Hoja (Rama 20 Hoja Hoja)  |
 .    20                                   |
     /  \                                  |
    .    .                                 |
-------------------------------------------/
-}



data ArbolBus a = Hoja1 | Rama1 a (ArbolBus a) (ArbolBus a)
  deriving Show

arbus = Rama1 20 (Rama1 10 Hoja1 Hoja1)(Rama1 40 (Rama1 30(Rama1 25 Hoja1 Hoja1)(Rama1 36 Hoja1 Hoja1))(Rama1 50 Hoja1 Hoja1))
arbus1 = Rama1 10 Hoja1 (Rama1 20 Hoja1 Hoja1)
arbus2 = Rama1 ("ale",10) Hoja1 (Rama1 ("leo",20) Hoja1 Hoja1)
arbus3 = Rama1 ("ruth",20) (Rama1 ("jose",10) Hoja1 Hoja1)(Rama1 ("leo",20) (Rama1 ("ernan",30)(Rama1 ("ale",25) Hoja1 Hoja1)(Rama1 ("zule",36) Hoja1 Hoja1))(Rama1 ("santi",50) Hoja1 Hoja1))

                                       
buscar::Integer->ArbolBus Integer->Bool
buscar x Hoja1 = False
buscar x (Rama1 r ai ad)
  |x==r = True
  |x<r  = buscar x ai
  |x>r  = buscar x ad

insertarArbol e (Hoja1) = Rama1 e Hoja1 Hoja1
insertarArbol e (Rama1 r ai ad)
  |e>r  = Rama1 r ai (insertarArbol e ad)
  |e<r  = Rama1 r (insertarArbol e ai) ad
  |e==r = Rama1 r ai ad

listArbol (Hoja1) = []
listArbol (Rama1 r ai ad) = r:((listArbol ai) ++ (listArbol ad))

crearLista []     = Hoja1
crearLista (x:xs) =  Rama1 x (crearLista (filter(<= x) xs))(crearLista (filter (>x) xs))

--------------------------------------------------------------------------------------------
type Materia = String
type Alumno = String
type Calificacion = Int
type Notas = ArbolBus(Alumno,Calificacion)
type Plantilla = (Materia,Notas)

--notaMayor::Planilla->Calificacion
notaMayor (cadena,arbol) = nota (arbol)
nota Hoja1 = 0
nota (Rama1 (nom,not) ai ad) = if not>nota(ad) then not else nota ad  
mayor(x:xs) = if x > mayor(xs) then x else mayor xs


inserta dupla (cadena,arbol) = meter dupla arbol
meter dupla (Hoja1) = Rama1 dupla Hoja1 Hoja1
meter (nuevoNom,laNota) (Rama1 (nom,not) ai ad)
  |laNota > not = Rama1 (nom,not) ai (meter (nuevoNom,laNota) ad)
  |laNota < not = Rama1 (nom,not) (meter (nuevoNom,laNota) ai) ad
  |laNota == not = Rama1 (nom,not) ai ad


meter1 dupla (cad,Hoja1) = (cad,Rama1 dupla Hoja1 Hoja1)
meter1 (nuevoNom,laNota) (cad,(Rama1 (nom,not) ai ad))
  |laNota > not = (cad,Rama1 (nom,not) ai (meter (nuevoNom,laNota) ad))
  |laNota < not = (cad,Rama1 (nom,not) (meter (nuevoNom,laNota) ai) ad)
  |laNota == not = (cad,Rama1 (nom,not) ai ad)
--data Lista a = Vacia | Anadir a (Lista a)
--  deriving Show
--data Natural = Cero | Sgte Natural
--  deriving Show

