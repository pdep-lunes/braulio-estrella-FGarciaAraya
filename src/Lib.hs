module Lib () where

import Text.Show.Functions ()

type Poder = Personaje->Personaje

data Personaje = UnPersonaje {
    nombre       ::String,
    poder_basico ::Poder,
    poder_super  ::Poder,
    super_activa ::Bool,
    cantidad_vida::Int
} deriving Show

-- PODERES
bolaEspinosa:: Personaje -> Personaje
bolaEspinosa personajeEnemigo = personajeEnemigo {cantidad_vida = max (cantidad_vida personajeEnemigo - 1000 ) 0}

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas tipo otroPersonaje
    | tipo == "sanador" = otroPersonaje {cantidad_vida = cantidad_vida otroPersonaje + 800}
    | tipo == "dañino"  = otroPersonaje {cantidad_vida = div (cantidad_vida otroPersonaje) 2}
    | otherwise = otroPersonaje


granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio personajeEnemigo 
    |radio > 3 && cantidad_vida personajeEnemigo < 800 = personajeEnemigo {nombre = nombre personajeEnemigo ++ " Espina estuvo aqui", super_activa = False, cantidad_vida = 0}
    |radio > 3 = personajeEnemigo {nombre = nombre personajeEnemigo++ " Espina estuvo aqui"}
    |otherwise = bolaEspinosa personajeEnemigo

torretaCurativa :: Personaje -> Personaje
torretaCurativa personajeAliado = personajeAliado {super_activa = True, cantidad_vida = cantidad_vida personajeAliado * 2}

-- REPORTES
ataquePoderEspecial::Personaje->Personaje->Personaje
ataquePoderEspecial miPersonaje suPersonaje
    | super_activa miPersonaje =  (poder_basico miPersonaje).(poder_super miPersonaje) $ suPersonaje
    | otherwise     = suPersonaje
estaEnLasUltimas::[Personaje]->[String]
estaEnLasUltimas todosPersonajes = (map nombre).(filter tienePocaVida) $ todosPersonajes 

tienePocaVida :: Personaje -> Bool
tienePocaVida personaje = cantidad_vida personaje < 800

{-
atacar con el poder especial: si el personaje tiene el súper poder activo, entonces va a atacar a su contrincante con el súper y con el básico. Si no, no hará nada.
saber quiénes están en las últimas: es decir, el nombre de aquellos brawlers que tienen menos de 800 puntos de vida.
-}

-- PERSONAJES
espina :: Personaje
espina = UnPersonaje {nombre = "Espina", poder_basico = bolaEspinosa, poder_super= granadaDeEspinas 5, super_activa = True, cantidad_vida = 4800}

pamela :: Personaje
pamela = UnPersonaje {nombre = "Pamela", poder_basico = lluviaDeTuercas "sanador", poder_super = torretaCurativa, super_activa = False, cantidad_vida = 9600}


{-

bolaEspinosa: le quita 1000 puntos de vida a quien sea su contrincante (¡no debe quedar un número negativo!).
lluviaDeTuercas: pueden ser sanadoras o dañinas. Las primeras le suman 800 puntos de vida a su colega y las segundas le disminuyen a la mitad la vida de quien sea su contrincante. En cualquier otro caso, no le pasa nada al personaje.
granadaDeEspinas: el daño va a depender del radio de explosión de la misma. Si es mayor a 3, le agregara a su nombre “Espina estuvo aquí”. Si además su contrincante tiene menos de 800 vida, desactiva su súper y lo deja con 0 de vida. En otro caso, se usa una bola de espinas.
torretaCurativa: le activa el súper a su aliado y lo deja con el doble de su salud inicial.

-}
