--Cristian Davila, practica complementaria de Haskell.

module PlayervsCPU where

import Tablero
import Estrategias
import System.Random
import System.IO


mainPlayervsCPU =
            do
                print ("Modalidad escogida: Player vs CPU")
                putStrLn " "
                print ("Introduzca el numero de rondas, o 0 si quiere rondas infinitas:")
                input <- getLine
                pedirEstrategia tableroPartida (read input)


-- Pide una estrategia para la CPU
pedirEstrategia :: Tablero -> Int -> IO ()
pedirEstrategia tableroPartida numRond =
            do
                putStrLn " "
                print ("Estrategia:")
                print ("Introduzca un 1 para la estrategia aleatoria, 2 para la semi inteligente o 3 para la inteligente:")
                input <- getLine                
                empezarPartida tableroPartida numRond (read input)


-- Comprueba si la estrategia dada es valida, si lo es empieza la partida, si no lo es, la pide de nuevo
empezarPartida :: Tablero -> Int -> Int -> IO ()
empezarPartida tableroPartida numRond estrategia =
            if(estrategia < 0 || estrategia > 4) then
                do
                    print ("Esa estrategia no existe, repita de nuevo.")
                    putStrLn " "
                    pedirEstrategia tableroPartida numRond
            else
                do
                    putStrLn " "
                    print ("Tablero de inicio:")
                    putStrLn " "
                    escribirTablero tableroPartida
                    player tableroPartida numRond2 estrategia
                    where
                        numRond2 =
                                    if(numRond==0) then (-1)
                                    else numRond


-- Pide un movimiento al jugador
player :: Tablero -> Int -> Int -> IO ()
player (Panel xs os) numRond estrategia =
            do
                putStrLn " "
                putStrLn " "
                print ("Introduzca la posicion donde desea mover")
                input <- getLine
                procesarMovimiento (Panel xs os) (read input) numRond estrategia


-- Decide si es un movimiento de añadir una ficha, o de intercambiar
procesarMovimiento :: Tablero -> Pos -> Int -> Int -> IO ()
procesarMovimiento (Panel xs os) movimiento numRond estrategia =
            do
                if(length xs < 3) then procesarMovimientoSimple (Panel xs os) movimiento numRond estrategia
                else pedirFichaCambio (Panel xs os) movimiento numRond estrategia


-- Pide una ficha para intercambiar
pedirFichaCambio :: Tablero -> Pos -> Int -> Int -> IO ()
pedirFichaCambio (Panel xs os) movimiento numRond estrategia =
            do
                print ("Introduzca la posicion de la ficha que desea substituir")
                input <- getLine
                procesarFichaCambio (Panel xs os) movimiento (read input) numRond estrategia

-- Comprueba si la ficha para cambiar pertenece al jugador, si lo es continua la partida, si no lo es, pide una nueva
procesarFichaCambio :: Tablero -> Pos -> Int -> Int -> Int -> IO ()
procesarFichaCambio (Panel xs os) movimiento oldPos numRond estrategia =
            do
                if((isOnList xs oldPos)) then procesarMovimientoCambio (Panel xs os) movimiento oldPos numRond estrategia
                else pedirFichaCambio (Panel xs os) movimiento numRond estrategia


-- Comprueba si el movimiento dado es valido, si lo es continua la partida, si no lo es, pide uno de nuevo
procesarMovimientoCambio :: Tablero -> Pos -> Int -> Int -> Int -> IO ()
procesarMovimientoCambio (Panel xs os) movimiento oldPos numRond estrategia =
            do
                if(movimiento > 0 && movimiento < 10 && (isOnList ls movimiento)) then prepararEstrategias tableroPartida' numRond2 estrategia
                else repetirMov (Panel xs os) numRond estrategia

                where
                    numRond2 = numRond-1
                    ls = posicionesLibres [1..9] (xs++os)
                    tableroPartida' = cambiarFichaX (Panel xs os) movimiento oldPos


-- Comprueba si el movimiento dado es valido, si lo es continua la partida, si no lo es, pide uno de nuevo
procesarMovimientoSimple :: Tablero -> Pos -> Int -> Int -> IO ()
procesarMovimientoSimple (Panel xs os) movimiento numRond estrategia =
            do
                if(movimiento > 0 && movimiento < 10 && (isOnList ls movimiento)) then prepararEstrategias tableroPartida' numRond2 estrategia
                else repetirMov (Panel xs os) numRond estrategia
                where
                    numRond2 = numRond-1
                    ls = posicionesLibres [1..9] (xs++os)
                    tableroPartida' = colocarFichaX (Panel xs os) movimiento


-- Pide un movimiento de nuevo
repetirMov :: Tablero -> Int -> Int -> IO ()
repetirMov (Panel xs os) numRond estrategia =
            do
                putStrLn " "
                print ("Movimiento invalido, introduzcalo de nuevo")
                player (Panel xs os) numRond estrategia


-- Crea una semilla y se la pasa a la maquina
prepararEstrategias :: Tablero -> Int -> Int -> IO ()
prepararEstrategias (Panel xs os) numRond estrategia =
            do
                g <- newStdGen
                maquina (Panel xs os) numRond estrategia g


-- Comprueba si ha ganado el jugador, y si no ha ganado, utilizando la estrategia elegida por el jugador,
-- escoge el mejor movimiento para ganar la partida
maquina :: RandomGen g => Tablero -> Int -> Int -> g -> IO ()
maquina (Panel xs os) numRond estrategia g =
            do
                putStrLn " "
                escribirTablero (Panel xs os)
                putStrLn " "                
                if (tresEnRaya xs) then print ("Felicidades! Has ganado!")
                else
                    do
                    print ("Movimiento de la maquina:")
                    print (movimiento)
                    putStrLn " "
                    checkComputerWin tableroPartida' numRond estrategia
                where                    
                    movimiento =                        
                            if(estrategia==1) then estrategiaAleatoria g (Panel xs os)
                            else if (estrategia==2) then estrategiaSemiInteligente g (Panel xs os)
                            else estrategiaInteligente g (Panel xs os)
                    tableroPartida' =
                                if(length os < 3) then colocarFichaO (Panel xs os) movimiento
                                else cambiarFichaO (Panel xs os) movimiento oldPos
                    oldPos = --solo lo utilizaremos si tenemos 3 fichas colocadas, sino, el valor que tenga da igual
                        if(length os == 3) then buscarPosicionSobrante (Panel xs os) movimiento
                        else (-1)


-- Comprueba si ha ganado la maquina
checkComputerWin :: Tablero -> Int -> Int -> IO ()
checkComputerWin (Panel xs os) numRond estrategia=
            do
                escribirTablero (Panel xs os)
                if (tresEnRaya os) then print ("Game over!")
                else if(numRond==0) then print ("Empate!")
                else player (Panel xs os) numRond estrategia
