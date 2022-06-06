{-# LANGUAGE BlockArguments #-}
import System.Win32 (LOCALESIGNATURE(LOCALESIGNATURE), dACL_SECURITY_INFORMATION, mAXIMUM_ALLOWED, aCCESS_SYSTEM_SECURITY, _open_osfhandle)
--1
areaCuadrado l = l * l 
--2 y 3
areaRectangulo (b, a) = (b * a, 2 * a + 2 * b)
--4
fun x y = x > y
--5
fun1 (x) = if (x `mod` 2 == 0) then True else False
--6 
fun2 (x) = if (x `mod` 2 == 0 && x `mod` 3 == 0) then True else False
--7
potenciaTres x = x ^ 3
--8
potencia4 x = x ^ 4
potencia8 x = x ^ 8
potencia10 x = x ^ 10
potencia32 x = x ^ 32
--9
ejer n1 n2 f = f n1 n2

--1
mayor x y = if x>y then x else y
--2
mayorDeTres x y z = mayor x (mayor y z)
--3
mayorDeCuatro w x y z = mayor w (mayor x (mayor y z))
--4
examen p1 p2 f ins = 
     if (p1>=51 && p2>=51 && f>=51 && ins>=51)
                then "Aprobado" else "Reprobado"
--5
fecha x y = if x>y then x else y

--1
--2
examenC x 
 |x >=51 = "Aprobado"
 |x < 51 = "Reprobado"
--3
examenD x 
 |x >= 90 = "Excelente"
 |x >= 70 = "Bien"
 |x >= 51 = "Regular"
 |otherwise = "Mal"
--4
examenE primerP segundoP  final instancia
 |(primerP + segundoP)/2 >= 51 = "Aprobado"
 |final > 50 = "Aprobado"
 |instancia > 50 = "Aprobado"
 |otherwise = "Reprobado"
--5
mayorList (x:xs)
 | x > mayorList(xs) = x
 | otherwise = mayorList(xs)
--6
quebrado (n,d)
 |(n/d) > 1 = True
 |otherwise = False
--7
fechaMayor (d,m,a) (d2,m2,a2)
 |a > a2 = (d,m,a)
 |m > m2 = (d,m,a)
 |d > d2 = (d,m,a)
 |otherwise = (d2,m2,a2)
--8
aniosTranscurridos (d,m,a) (d2,m2,a2)
 |m < m2 = a2 - a
 |m > m2 = a2 - a - 1
 |m == m2 && d <= d2 = a2 - a
 |otherwise = a2 - a - 1
--9
mesesTranscurridos (d,m,a) (d2,m2,a2)
 |m <= m2 && a <= a2 = ((a2 - a + 1) * 12) - m - (12 - m2)
 |otherwise = -1
 --10
contarAnios (x,y) = (x - y) * 365
diasTranscurridos (d,m,a) (d2,m2,a2)
 |d <= d2 && m <= m2 && a <= a2 = contarAnios (a2,a) + (((m2 - m + 1) * 31) - d - (31 - d2))
 |otherwise = - 1
--11
fechasTranscurridos (d,m,a) (d2,m2,a2) = 
     (diasTranscurridos (d,m,a) (d2,m2,a2),mesesTranscurridos (d,m,a) (d2,m2,a2),aniosTranscurridos (d,m,a) (d2,m2,a2)) 

--1
sgtVocal v = 
     case v of
          'a' -> 'e'
          'e' -> 'i'
          'i' -> 'o'
          'o' -> 'u'
          'u' -> 'a'
          _ -> '?'
--3
miAnd (v1, v2) = 
     case (v1,v2) of
          (True, True) -> True
          _ -> False
--4
miOr (v1, v2, v3) = 
     case (v1,v2, v3) of
          (False, False, False) -> False
          (_, _, _) -> True
--5
miXor (v1, v2, v3) = 
     case (v1,v2, v3) of
          (True, True, True) -> False
          (False, False, False) -> False
          (_, _, _) -> True
--6
-- miOperador operador (v1, v2, v3) =
--      case (operador == "And") of
--           True -> miAnd (v1, v2, v3)
--           case (operador == "Or") of
--                True -> miOr (v1, v2, v3)
--                False -> miXor (v1, v2, v3)
--9
dosNum n1 n2 =
     case (n1 > n2) of
          True -> n2
          _ -> n1
--10
-- fun4_10 n1 n2 n3 n4 n5 n6 = 
--      case (fun4_9 n1 n2 >= fun4_9 n3 n4) of
--           True -> case (fun4_9 n3 n4 >= fun4_9 n5 n6) of
--                True -> fun4_9 n5 n6
--                False -> case (fun4_9 n1 n2 >= fun4_9 n5 n6) of
--                     True -> fun4_9 n5 n6
--                     False -> fun4_9 n1 n2
--11
tresNum n1 n2 n3 = 
     case (n1 + n2 + n3 < 20 && n1 + n2 + n3 >= 10) of
          True -> "Sumatoria mayor"
          False -> case (n1 + n2 + n3 < 10) of
               True -> "Sumatoria menor"
               _ -> "Vacio"        
--12
evaluacion n1 n2 n3 =
     case ((n1 + n2 + n3)/3 >= 100) of
          True -> "Excelente"
          False -> case ((n1 + n2 + n3)/3 >= 70 && (n1 + n2 + n3)/3 < 90) of
               True -> "Bien"
               False -> case ((n1 + n2 + n3)/3 >= 51 && (n1 + n2 + n3)/3 < 70) of
                    True -> "Regular"
                    _ -> "Mal"

--DEFINICIONES LOCALES
-- mayorDeSeis a b c d e f = mayor a (mayor b (mayor c (mayor d(mayor e f))))
--1
--2
eje2 n1 n2 n3 = 
 let
     sumatoria = n1 + n2 + n3
     sumEn  li ls = sumatoria>=li && sumatoria < ls
 in
     if sumEn 10 20 then "Sumatoria mayor" else
          if sumEn 1 10 then "Sumatoria menor" else 
               "Vacio"
--3
eje3 n1 n2 n3 n4
 | promEn 90 100 ="Excelente"
 | promEn 80 89 ="Muy Bien"
 | promEn 70 79 ="Bien"
 | promEn 51 69 ="Regular"
 | promEn 0 50 ="Mal"
 | otherwise ="notas invalidas"
 where
    prom= div (n1+n2+n3+n4) 4
    promEn li ls = prom>=li && prom <=ls
--4 Inventar ejercicios que muestren la utilidad de las definiciones locales

--1
reco1 (d,_,_) = d
--2
reco2 (_,m,_) = m
--3
reco3 (_,_,a) = a
--4
reco4 (quebrado1,quebrado2) = mayor quebrado1 quebrado2 
--5
reco5 (numerador, denominador) = numerador
--6
reco6 (numerador, denominador) 
 |numerador/denominador > 0 = '+'
 |otherwise = '-'   
--7
reco7 ((d,m,a),(d1,m1,a1),(d2,m2,a2)) 
 |promA ((d,m,a),(d1,m1,a1),(d2,m2,a2)) = (d,m,a)
 |promA ((d1,m1,a1),(d,m,a),(d2,m2,a2)) = (d1,m1,a2)
 |promA ((d2,m2,a2),(d,m,a),(d1,m1,a1)) = (d2,m2,a2)
 where
     promA ((d,m,a),(d1,m1,a1),(d2,m2,a2)) = 
          (a < a1 && a < a2) || (m < m1 && m < m2) || (d < d1 && d < d2)
--8
reco8 :: (Ord a, Ord b, Ord c) => ((a, b, c), (a, b, c)) -> (a, b, c)
reco8 ((h,m,s),(h1,m1,s1)) =
 let
     promH ((h,m,s),(h1,m1,s1)) = 
          h < h1 || m < m1 || s < s1
 in
     if promH ((h,m,s),(h1,m1,s1)) then (h,m,s) else (h1,m1,s1)
--8_v2
reco8_v2 ((h,m,s),(h1,m1,s1)) 
 |promH ((h,m,s),(h1,m1,s1)) = (h,m,s)
 |promH ((h1,m1,s1),(h,m,s)) = (h1,m1,s1)
 where
      promH ((h,m,s),(h1,m1,s1)) = 
           h < h1 || m < m1 || s < s1
--10
reco10 natural = natural + 1