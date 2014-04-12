--Cristian Davila, practica complementaria de Haskell.

module Estrategias where

import Tablero
import System.Random


-- Dado una semilla y dos enteros que indican el principio y el fin, devuelve un numero random en ese itnervalo
genera :: RandomGen s => s -> Int -> Int -> Int
genera s lo hi = x
 where (x,s1) = randomR (lo,hi) s


-- Dado una semilla y una lista de posiciones, devuelve un elemento aleatorio de la lista
puntRandom :: RandomGen g => g -> [Pos] -> Pos
puntRandom g (ls) = y
    where
        y = ls !! (genera g 0 ((length ls)-1))


-- Dado dos listas, devuelve en una lista las posiciones de la primera lista que no pertenecen a la segunda
posicionesLibres :: [Pos] -> [Pos] -> [Pos]
posicionesLibres [] xs = []
posicionesLibres (l:ls) xs = if( not (isOnList xs l) ) then l:(posicionesLibres ls xs)
                            else (posicionesLibres ls xs)


-- Dado una semilla y un tablero, devuelve una posicion aleatoria vacia en el tablero
estrategiaAleatoria :: RandomGen g => g -> Tablero -> Pos
estrategiaAleatoria g (Panel xs os) = punt
            where
                ls = posicionesLibres [1..9] (xs++os)
                punt = puntRandom g ls


-- Dado una semilla y un tablero, devuelve una posicion libre en el tablero, que o bien esta al lado de otra
-- de sus fichas, o bien es aleatoria
estrategiaSemiInteligente :: RandomGen g => g -> Tablero -> Pos
estrategiaSemiInteligente g (Panel xs os) = punt
                where
                    ls = posicionesLibres [1..9] (xs++os)
                    punt =
                            if(isOnList ls 5) then 5         --Posicion central tiene ventaja
                            else if(puntoProximo os ls /= (-1)) then puntoProximo os ls
                            else puntRandom g ls


-- Dado una semilla y un tablero, devuelve una posicion libre en el tablero, escogida con un criterio mas avanzado
estrategiaInteligente :: RandomGen g => g -> Tablero -> Pos
estrategiaInteligente g (Panel xs os) = punt
                where
                    ls = posicionesLibres [1..9] (xs++os)
                    punt = posSigLibreAvanzado g (Panel xs os) ls


-- Dado una semilla, un tablero y una lista de puntos libres en el tablero, devuelve una posicion libre en el
-- tablero, que o bien esta al lado de otra de sus fichas, o gana la partida, o evita que el rival la gane,
-- o es una posicion aleatoria.
posSigLibreAvanzado :: RandomGen g => g -> Tablero -> [Pos] -> Pos
posSigLibreAvanzado g (Panel xs os) ls = punt
                where
                    punt =
                            if      ( length os >= 2 && ganaO /= (-1)) then ganaO
                            else if ( length xs >= 2 && ganaX /= (-1)) then ganaX
                            else if ( isOnList ls 5 ) then 5
                            else if ( length os == 0 ) then puntRandom g ls
                            else if(puntoProximo os ls /= (-1)) then puntoProximo os ls
                            else puntRandom g ls
                    ganaO = ganar os ls
                    ganaX = ganar xs ls


-- Dado una lista de las posiciones de sus fichas, y una lista de posiciones libres, devuelve la posicion que hace
-- que gane la partida, o -1 si no existe esa posicion
ganar :: [Pos] -> [Pos] -> Pos
ganar os [] = (-1)
ganar os (l:ls) = punt
                where
                    punt =
                            if(tresEnRaya (l:os)) then l
                            else ganar os ls


-- Dado una lista de las posiciones de sus fichas, y una lista de posiciones libres, devuelve una posicion que esta
-- al lado de una de sus fichas
puntoProximo :: [Pos] -> [Pos] -> Pos
puntoProximo _ [] = (-1)
puntoProximo os (l:ls) = punt
                    where
                        punt =
                                if(point /= (-1)) then point
                                else puntoProximo os ls
                        point = (nearPoint l os)


-- Dado una posicion y una lista de posiciones libres, devuelve una posicion libre que este al lado de la posicion dada,
-- o -1 si no existe esa posicion
nearPoint :: Pos -> [Pos] -> Pos
nearPoint p1 [] = (-1)
nearPoint p1 (o1:os) = punt
                    where
                        punt =  if (near p1 o1) then p1
                                else nearPoint p1 os


-- Dado dos posiciones, indica si estan al lado
near :: Pos -> Pos -> Bool
near p1 o1 = cerca
            where
                cerca =
                        if( (p1+1) == o1 || (p1-1) == o1 || (p1+3) == o1 || (p1-3) == o1) then True
                        else if( (p1==5) && ( (p1+2) == o1 || (p1-2) == o1 || (p1+4) == o1 || (p1-4) == o1) ) then True
                        else False


-- Dado un tablero, devuelve la posicion de la ficha que no influye en ganar la partida
buscarPosicionSobrante ::  Tablero -> Pos -> Pos
buscarPosicionSobrante (Panel xs (o1:o2:o3:os)) movimiento = punt
            where
                punt =
                        if      (tresEnRaya (movimiento:o1:o2:[]) ) then o3
                        else if (tresEnRaya (movimiento:o1:o3:[]) ) then o2
                        else if (tresEnRaya (movimiento:o2:o3:[]) ) then o1
                        else o1 --sino sempre eliminem un aleatoriament, y sera o1
