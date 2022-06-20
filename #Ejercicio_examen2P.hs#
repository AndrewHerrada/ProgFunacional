--Ejercicios para examen
--Sea
--subCadena "abcdefg" 2 5 => "cdef"
data Lista a = Sinelementos | Aumentar a (Lista a)
  deriving Show
data Natural = Nada | Unomas Natural
  deriving (Show,Eq)
type Cadena = Lista Char
type PosicionInicial = Natural
type PosicionFinal = Natural

natu = (Unomas Nada)
natu2 = toint 3
lista2 = tolista [1,2,3,4,5]
lista3 = tolista ['a','d','s','i','v']
toint 0 = Nada
toint n = Unomas (toint (n-1))
tolista [] = Sinelementos
tolista (x:xs) = Aumentar x (tolista xs)


at Nada (Aumentar x xs) = x
at (Unomas n) (Aumentar x xs) = at n xs

concatenar n lista = Aumentar n lista

subCadena::Cadena->PosicionInicial->PosicionFinal->Cadena
subCadena xs posIni posFin  =
  if posIni == posFin then Aumentar(at posFin xs) Sinelementos
  else concatenar(at posIni xs) (subCadena xs (Unomas posIni) posFin)     


--subCadena xs ini fin = if ini == fin then [xs !! fin]  else [(xs !! ini)] ++ subCadena xs (ini + 1) fin
{-
subCadena [5,2,6,3,6,7] ini fin
subCadena [5,2,6,3,6,7] 2    4 
6++(subCadena [5,2,6,3,6,7]) 3 4
6++(3++(subCadena [5,2,6,3,6,7] 4 4
6++(3++(6))
[6,1,6]

-}

{-
subCadena2 xs de hasta = if de Main./= hasta then Aumentar ((get) xs de) subCadena2 xs (Unomas de) hasta else Aumentar ((get) xs hasta) Sinelementos 

get (Aumentar x _) Nada = x
get (Aumentar x xs) (Unomas n) = get xs n

instance Eq Natural where
(/=) Nada Nada = True
(/=) (Unomas n1) (Unomas n2) = n1 == n2
(/=) _ _ = False
-}
