
-- 1. f1 x y z = if x then y+10 else z

f1 x y z = if x then y+10 else z
-- 2. f2 x y z = if z==’s’ then 2*x else y
f2::Int->Int->Char->Int
f2 x y z = if z=='s' then 2*x else y
-- 3. f3 x y z = ( x 2) && (y ‘a’) && (z True)
--f3 :: Num t => (t -> Bool) -> (Char -> Bool) -> (Bool -> Bool) -> Bool
--f3 x y z = ( x 2) && (y 'a') && (z True)
-- 4. f4=(\x -> \y -> \z -> \w -> if x 2 then z else w+10 )
f4 :: (Integer -> Bool) -> p -> Integer -> Integer -> Integer
f4=(\x -> \y -> \z -> \w -> if x 2 then z else w+10 )
-- 5. f5=(\(x,y) -> \(z ,w) -> (x y )+ (z w))
f5 :: (t1 -> Integer, t1) -> (t2 -> Integer, t2) -> Integer
f5=(\(x,y) -> \(z ,w) -> (x y )+ (z w))
-- 6. f6 x = x
f6::Int->Int
f6 x = x
-- 7. f7 x y = x y
f7::(Int->Int)->Int->Int
f7 x y = x y
-- 8. f8 x y z = x (y z)
f8 :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
f8 x y z = x (y z)
-- 9. f9 x y z = (x y) z
f9 :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
f9 x y z = (x y) z
-- 10. f10 x y z w= x (y (z w))
f10 :: (t1 -> t2) -> (t3 -> t1) -> (t4 -> t3) -> t4 -> t2
f10 x y z w= x (y (z w))
-- 11. f11 x y z w= ((x y) z) w
f11 :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
f11 x y z w= ((x y) z) w
-- 12. f10 x y z w= ((x y) (z w)
f12 :: (t1 -> t2 -> t3) -> t1 -> (t4 -> t2) -> t4 -> t3
f12 x y z w= ((x y) (z w))
------------------------------------
eje2 g b1 n h
 |b1 = 20*n + (h True)
 |g fun1 fun2 = 100
 where
  fun1 n1 n2 b3 = if b3 then n1+n2 else 0
  fun2 b4 = if b4 then 10 else 30
{-
ej3::(Int->Int)->(Int->Bool)->Int->Bool->Int
ej3 f1 f2 n1 b1
 |b1 = n1 + 3
 |True = f1 4
 |100 = f2 x
 where
  x bol = if bol then True else False
 
-}