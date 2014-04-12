--Cristian Davila, practica complementaria de Haskell.



-- El tablero esta compuesto por dos listas de posiciones, donde están las posiciones de las X y en el otro el de las O
-- La posiciones en el tablero siguen el siguiente orden:
--
--"      Tablero     "
--   1      2      3
--   4      5      6
--   7      8      9
--
-- El jugador indicara con un numero entre el 1 y el 9 la posición donde desea mover, el sistema comprueba si la posición
-- es valida (si esta libre)
-- En caso de necesitar una ficha de intercambio, el sistema la pedirá y también comprobara si es valida.


import Tablero
import Estrategias
import CPUvsCPU
import PlayervsCPU
import System.IO


main =
            do
                print ("Bienvenido!")
                putStrLn " "
                print ("Tiene dos modalidades de juego:")
                print (" 1 - Player vs CPU")
                print (" 2 - CPU vs CPU")
                putStrLn " "
                print ("Introduzca el identificador de la modalidad que desea:")
                putStrLn " "
                input <- getLine
                comprobarModalidad (read input)


comprobarModalidad modalidad =
            do
                if(modalidad == 1) then mainPlayervsCPU
                else if(modalidad == 2) then mainCPUvsCPU
                else lanzarExcepcion


lanzarExcepcion =
            do
                putStrLn " "
                print ("La modalidad seleccionada no existe, introduzca una correcta:")
                putStrLn " "
                input <- getLine
                comprobarModalidad (read input)
