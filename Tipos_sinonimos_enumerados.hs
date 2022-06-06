{-Parte 1: Definir los siguientes tipos de datos
    - ZonaGeografica que permita representar las 3 zonas geograficas de Bolivia
      (valles, llanos y altiplano)
    - Departamento que permita representar los 9 departamentos de Bolivia
-}
{-
type Valles         = String
type Llanos         = String
type Altiplano      = String
type ZonaGeografica = (Valles,Llanos,Altiplano)
-}
data ZonaGeografica = Valle | Llano | Altiplano
data Departamento = Pando | Beni | SantaCruz | LaPaz | Cocha | Oruro | Potosi | Sucre | Tarija

--1. Una funcion que reciba una zonsa y devuelva un mensaje indicando sus caracteristica
caracteristica::ZonaGeografica ->String
caracteristica Valle    = "Paisaje bonito, clima templado"
caracteristica Llano    = "Paisaje seco, clima frio"
caracteristica Altiplano = "Paisaje verde, clima humedo"

--2. Una funcion que reciba un departamento y devuelva True si pertenece a la
--zona de los valles, falso en otro caso.
