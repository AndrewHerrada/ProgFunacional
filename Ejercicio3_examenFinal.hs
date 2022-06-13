data Lista a = Empty | Put a (Lista a)
  deriving Show
data Natural = Cero | Sucesor Natural
  deriving (Show,Eq)

--Escribir una definicion para f de modo que obedezca al tipo de dato.
f::Lista (Lista (Int,Char))->(Natural->Bool)->(Char,Natural)
f (Put x(Put e xs)) n = if ((n Cero)==True) then ('1',Cero) else f xs n
 where
   n m = if (m==Cero) then False else True
---------------------------------------------------------------------------------------------------------------------   
--definir la funcion uqe recibe un caracter y una cadena y devuelve una lista
--con las posiciones en uqe el caracter oucrren en la cadena
--          buscar 'a' "baacbda" => [1,2,6]

type Cadena = Lista Char
type Posiciones = Lista Natural
type Caracter = Char

--buscar::Caracter->Cadena->Posiciones
buscar letra xs  = looking letra xs 0
  where
    looking letra Empty n = Empty
    looking letra (Put x xs) n = if letra==x then Put (toint n) (looking letra xs (n+1)) else looking letra xs (n+1) 

toint 0 = Cero
toint n = Sucesor (toint (n-1))
{-
bus::Char->String->[Int]
bus letra xs = lookingFor letra xs 0
  where
    lookingFor letra [] n = []
    lookingFor letra (x:xs) n = if letra == x then n:lookingFor letra xs (n+1) else lookingFor letra xs (n+1)

lookingFor 'a' "lal" 0
lookingFor 'a' "al" 1
1:(lookingFor 'a' "l" 2)
1:(lookingFor 'a' "" 3)
1:[]
[1]
-}
----------------------------------------------------------------------------------------------------------------------
{-
Definir una funcion que reciba una lista de numeros  naturales y devuelva una
lista de cadenas de asteriscos de tamaño igual a cada natural de la lista como se ilustra en el ejemplo:
          convierte::[Natural]->[String]

Input:
[Proximo(Proximo(Proximo(Proximo Nada))),(Proximo(Proximo Nada)),(Proximo(Proximo(Proximo Nada)))

OutPut:
["****","**","***"]
-}

data Numero = Nada | Proximo Numero
  deriving Show

convierte xs = map toAsterisco xs
  where
    toAsterisco Nada        = ""
    toAsterisco (Proximo n) = "*" ++ (toAsterisco n)
-----------------------------------------------------------------------------------------------------------------------
{-
Usando LISTA POR COMPRENSION definir una funcion que reciba una lista de de
alumnos y devuelva una lista de parse (Nombre,GraficaDeFaltas) de modod que
GraficaDeFaltas sea una lista con cantFaltas de asteriscos
Por Ejemplo:
    [('juan',5,80),('pepe',0,90),('susi',8,52)]
devolveria:
    [('juan',"$$$$$"),('pepe',[]),('susi',8,"$$$$$$$$")]
-}

type Carnet          = Int
type Promedio        = Int
type Nombre          = String
type CantidadDeFaltas = Int
type Alumno          = (Nombre,CantidadDeFaltas,Promedio)
type GraficaDeFaltas = [Char]
type AlumnoResp      = (Nombre,GraficaDeFaltas)


fun::[Alumno]->[AlumnoResp]
fun xs = [(h x, g(i x)) | x <- xs]
  where
    g 0 = ""
    g n = "$" ++ g (n-1)
    h (nom,c,p) = nom
    i (nom,c,p) = c
-------------------------------------------------------------------------------------------------------------------------
--Definir una funcion procesaFila::Lista Char->Lista Char->Lista Char
-- S U B L N N C O S U O
-- Z C B L A N C O T K U A B
------------------------------
-- * * B L * N C O * * * * *

cad = "sublnncosuo"
cad1= "zcblancotkuab"

procesaFila::Lista Char->Lista Char->Lista Char
procesaFila xs Empty  = tolista(ast (miLength xs)) 
procesaFila Empty ys  = tolista(ast (miLength ys))
procesaFila (Put x xs) (Put y ys) = if x==y then Put x (procesaFila xs ys) else Put '*' (procesaFila xs ys) 

miLength Empty = 0
miLength (Put x xs) = 1 + miLength xs

tolista [] = Empty
tolista (x:xs) = Put x (tolista xs)



procesa::[Char]->[Char]->[Char]
procesa xs []  = ast ((length xs)) 
procesa [] ys  = ast ((length ys))
procesa (x:xs) (y:ys)  = if x==y then x:(procesa xs ys) else '*':(procesa xs ys)
ast 0 = ""
ast n = "*" ++ ast (n-1)



{-
length(min xs ys) = 11
ast (resta (length xs) (length ys)) = "**"
procesa "sublinncosuo" ""zcblancotkuab" 1
*:(procesa "ublinncosuo" ""cblancotkuab" 2)
*:(*:(procesa "blinncosuo" ""blancotkuab" 3))
*:(*:(b:(procesa "linncosuo" ""lancotkuab" 4)))
*:(*:(b:(l:(procesa "inncosuo" ""ancotkuab" 5))))
*:(*:(b:(l:(*:(procesa "nncosuo" ""ncotkuab" 6)))))
*:(*:(b:(l:(*:(n:(procesa "ncosuo" ""cotkuab" 7))))))
-}
