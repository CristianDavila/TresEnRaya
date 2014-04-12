--Cristian Davila, practica complementaria de Haskell.

module CPUvsCPU where

import Tablero
import Estrategias
import System.Random
import System.IO


mainCPUvsCPU =
            do
                print ("Modalidad escogida: CPU vs CPU")
                putStrLn " "
                print ("Introduzca el numero de rondas, o 0 si quiere rondas infinitas")
                input <- getLine
                pedirEstrategiaX tableroPartida (read input)


-- Pide una estrategia para la CPU X
pedirEstrategiaX :: Tablero -> Int -> IO ()
pedirEstrategiaX tableroPartida numRond =
            do
                putStrLn " "
                print ("Estrategia en la maquina X:")
                print ("Introduzca un 1 para la estrategia aleatoria, 2 para la semi inteligente o 3 para la inteligente")
                input <- getLine
                pedirEstrategiaO tableroPartida numRond (read input)


-- Comprueba si la estrategiaX es valida, y si lo es pide una estrategia para la CPU O, si no lo es, la pide de nuevo
pedirEstrategiaO :: Tablero -> Int -> Int -> IO ()
pedirEstrategiaO tableroPartida numRond estrategiaX =
            if(estrategiaX < 0 || estrategiaX > 4) then
                do
                    print ("Esa estrategia no existe, repita de nuevo.")
                    putStrLn " "
                    pedirEstrategiaX tableroPartida numRond
            else
                do
                    print ("Estrategia en la maquina O:")
                    print ("Introduzca un 1 para la estrategia aleatoria, 2 para la semi inteligente o 3 para la inteligente")
                    input <- getLine
                    empezarPartida tableroPartida numRond estrategiaX (read input)


-- Comprueba si la estrategiaO dada es valida, si lo es empieza la partida, si no lo es, la pide de nuevo
empezarPartida :: Tablero -> Int -> Int -> Int -> IO ()
empezarPartida tableroPartida numRond estrategiaX estrategiaO =
                if(estrategiaO < 0 || estrategiaO > 4) then
                    do
                        print ("Esa estrategia no existe, repita de nuevo.")
                        putStrLn " "
                        pedirEstrategiaO tableroPartida numRond estrategiaX
                else
                do
                    print ("Tablero de inicio:")
                    escribirTablero tableroPartida
                    prepararEstrategiasX tableroPartida numRond2 estrategiaX estrategiaO
                    where
                        numRond2 =
                                    if(numRond==0) then (-1)
                                    else numRond


-- Crea una semilla y se la pasa a la maquinaX
prepararEstrategiasX :: Tablero -> Int -> Int -> Int -> IO ()
prepararEstrategiasX tableroPartida numRond2 estrategiaX estrategiaO =
            do
                g <- newStdGen
                maquinaX tableroPartida numRond2 estrategiaX estrategiaO g


-- Comprueba si ha ganado la maquina O, y si no ha ganado, utilizando la estrategia elegida por el jugador,
-- escoge el mejor movimiento para ganar la partida
maquinaX :: RandomGen g => Tablero -> Int -> Int -> Int -> g -> IO ()
maquinaX (Panel xs os) numRond estrategiaX estrategiaO g =
            do

                putStrLn " "
                if (tresEnRaya os) then print ("Gana la maquina O")
                else
                    do
                    print ("Movimiento de la maquina X:")
                    print (movimiento)
                    checkComputerWinX tableroPartida' numRond estrategiaX estrategiaO
                where

                    movimiento =
                        if(estrategiaX==1) then estrategiaAleatoria g (Panel os xs)
                        else if (estrategiaX==2) then estrategiaSemiInteligente g (Panel os xs)
                        else estrategiaInteligente g (Panel os xs)
                    tableroPartida' =
                                if(length os < 3) then colocarFichaX (Panel xs os ) movimiento
                                else cambiarFichaX (Panel xs os) movimiento oldPos
                    oldPos = --solo lo utilizaremos si tenemos 3 fichas colocadas, sino, el valor que tenga da igual
                        if(length os == 3) then buscarPosicionSobrante (Panel  os xs) movimiento
                        else (-1)


-- Comprueba si ha ganado la maquina X
checkComputerWinX :: Tablero -> Int -> Int -> Int -> IO ()
checkComputerWinX (Panel xs os) numRond estrategiaX estrategiaO =
            do
                escribirTablero (Panel xs os)
                if (tresEnRaya xs) then print ("Gana la maquina X!")
                else if(numRond==0) then print ("Empate!")
                else prepararEstrategiasO (Panel xs os) numRond2 estrategiaX estrategiaO
                where
                    numRond2 = numRond-1


-- Crea una semilla y se la pasa a la maquinaO
prepararEstrategiasO :: Tablero -> Int -> Int -> Int -> IO ()
prepararEstrategiasO tableroPartida numRond2 estrategiaX estrategiaO =
            do
                g <- newStdGen
                maquinaO tableroPartida numRond2 estrategiaX estrategiaO g


-- Comprueba si ha ganado la maquina X, y si no ha ganado, utilizando la estrategia elegida por el jugador,
-- escoge el mejor movimiento para ganar la partida
maquinaO :: RandomGen g => Tablero -> Int -> Int -> Int -> g -> IO ()
maquinaO (Panel xs os) numRond estrategiaX estrategiaO g =
            do

                putStrLn " "
                if (tresEnRaya xs) then print ("Gana la maquina X")
                else
                    do
                    print ("Movimiento de la maquina O:")
                    print (movimiento)
                    checkComputerWinO tableroPartida' numRond estrategiaX estrategiaO
                where

                    movimiento =
                        if(estrategiaO==1) then estrategiaAleatoria g (Panel xs os)
                        else if (estrategiaO==2) then estrategiaSemiInteligente g (Panel xs os)
                        else estrategiaInteligente g (Panel xs os)
                    tableroPartida' =
                                if(length os < 3) then colocarFichaO (Panel xs os) movimiento
                                else cambiarFichaO (Panel xs os) movimiento oldPos
                    oldPos = --solo lo utilizaremos si tenemos 3 fichas colocadas, sino, el valor que tenga da igual
                        if(length os == 3) then buscarPosicionSobrante (Panel xs os) movimiento
                        else (-1)


-- Comprueba si ha ganado la maquina O
checkComputerWinO :: Tablero -> Int -> Int -> Int -> IO ()
checkComputerWinO (Panel xs os) numRond estrategiaX estrategiaO =
            do
                escribirTablero (Panel xs os)
                if (tresEnRaya os) then print ("Gana la maquina O!")
                else if(numRond==0) then print ("Empate!")
                else prepararEstrategiasX (Panel xs os) numRond estrategiaX estrategiaO
