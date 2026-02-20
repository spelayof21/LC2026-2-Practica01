module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float | --representa un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Square l) = l * l
area (Rectangle b a) = b * a
area (Triangle l) = (l * ((l * (sqrt 3.0)) / 2)) / 2
area (Trapeze mayor menor h) = ((mayor + menor) * h) / 2

--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Circle r) = 2 * r * pi
perimeter (Square l) = 4 * l
perimeter (Rectangle b a) = 2 * (a + b)
perimeter (Triangle l) = 3 * l
perimeter (Trapeze mayor menor h) = mayor + menor + 2 * sqrt(((mayor - menor) / 2)^2 + h^2)


--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float, Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 (Point x y) = sqrt(x^2 + y^2)


--Ejercicio 3
data Haskellium = Haskellium {
  name :: String,
  lastName1 :: String,
  lastName2 :: String,
  location :: Point,
  houseShape :: Shape
} deriving (Show)

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son papa mama nombreHijo =
  Haskellium
  nombreHijo
  (lastName1 papa)
  (lastName2 mama)
  (location papa)
  (houseShape papa)

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost h =
  let shape = houseShape h
      paredes = perimeter shape * 2.5
      techo = area shape
  in paredes + techo

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork t =
  let distancia = from0 (location h)
  in if dist < 300
        then dist / 30
        else dist / 70

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo xs = xs == reverse xs

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) =
  let ps = conjuntoPotencia xs
  in ps ++ map (x:) ps

--ARBOLES

--Implementacion

data OneTwoTree a =
  Empty
  | Hoja a
  | Nodo a (OneTwoTree a) (OneTwoThree a)
  deriving (Show)

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma Empty = 0
suma (Hoja x) = x
suma (Nodo x izq der) = x + suma izq + suma der
