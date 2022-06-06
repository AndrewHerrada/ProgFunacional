
m1 = [5,4,1,9,2,3,6,8,7,4]
listOrdenada = [1,2,3,4,5]
listaOrdenada = [1,2,3,4,5]
m2 = [[5,14,3,6,7],
    [31,9,22,5,13],
    [2,3,8,6,5],
    [6,8,2,21,14],
    [17,4,12,41,2]]

m3 = [[1,2,3,4],
    [5,6,7,8],
    [9,19,18,17],
    [15,12,14,10,11]]
--1
eje1 :: [c] -> c
eje1 = (last.(take 3))
m1 :: [Integer]
--2
eje2 :: [c] -> c
eje2 = (last.(take 2))
--4
eje4 = (last.(last.(take 3)))
--5
eje5 = (last.(take 3.(last)))
--6
lista_ordenada::Ord a=>[a]->Bool
lista_ordenada [] = True
lista_ordenada [_] = True
lista_ordenada (x:y:xs) = (x<=y) && lista_ordenada (y:xs)
--7
eje7 list1 list2 = if (list1 == list2) then True else False
--8
eje8::Ord a=>[a]->Bool
eje8 [] = True
eje8 [_] = True
eje8 (x:y:xs) = (x == y) && eje8 (y:xs)
--9
eje9 :: Int -> [a] -> a
eje9 n xs = last(take num xs)
    where
        num = n
--10
miLength xs = sum (map elem xs)
    where
        elem  x = 1
miZip xs ys =  map g [0..n-1]
    where
       n = min (length xs) (length ys)
       g i = (xs!!i, ys!!i) 
miFilter f xs = concat(map g xs)
    where
        g x = if f x then [x] else []
--11
transpuesta mss = map (obtenerColumna mss) [0..ultimaCol]
    where
        ultimaCol = (length(head mss))  - 1
        obtenerColumna mss c = map (!!c) mss
----------------FOLDR---------------
ej1 xs = foldr f a xs
    where
        a = 1
        f x rs = x * rs
miMap fun xs = foldr f a xs
    where
        a=[]
        f x rs = (fun x):rs
myFilter fun xs = foldr f a xs
    where
        a=[]
        f x rs = if fun rs then x:rs else [x]
