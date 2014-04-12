--Cristian Davila, practica complementaria de Haskell.



module Tablero where


type Pos = Int

type Posiciones = [Pos]

data Tablero = Panel Posiciones Posiciones
             | TableroVacio



-- Indica si un elemento esta en una lista
isOnList :: [Int] -> Int -> Bool
isOnList [] p = False
isOnList (x:xs) p = if(p==x) then True
                    else isOnList xs p


-- Dado un tablero y una posicion, dice el valor de la posicion
muestraPosicion :: Tablero -> Int -> String
muestraPosicion (Panel xs os) p = val
                where
                    val =
                        if(isOnList xs p) then "   X   "
                        else if (isOnList os p) then "   O   "
                        else "   -   "


-- Dado un tablero y una fila, muestra los elementos de dicha fila
muestraFila :: Tablero -> [Int] -> String
muestraFila (Panel xs os) [] = ""
muestraFila (Panel xs os) (p:ps) = (muestraPosicion (Panel xs os) p) ++  (muestraFila (Panel xs os) (ps))


-- Dado un tablero, muestra el tablero
muestraTablero :: Tablero -> String
muestraTablero (Panel xs os) = show "      Tablero     " ++ "\n" ++ fila1 ++ "\n" ++ fila2 ++ "\n" ++ fila3 ++ "\n"
                        where
                            fila1 = muestraFila (Panel xs os) [1,2,3]
                            fila2 = muestraFila (Panel xs os) [4,5,6]
                            fila3 = muestraFila (Panel xs os) [7,8,9]



instance Show Tablero where show exemple = muestraTablero exemple



tableroPartida::Tablero
tableroPartida = Panel [] []



-- Dado un tablero, escribe el tablero
escribirTablero :: Tablero -> IO ()
escribirTablero (Panel xs os) = putStr(show (Panel xs os))



-- Dado un tablero y una posicion, el punto pasa a formar parte de las fichas de X
colocarFichaX::Tablero -> Pos -> Tablero
colocarFichaX (Panel xs os) p = (Panel (p:xs) os)



-- Dado un tablero y una posicion, el punto pasa a formar parte de las fichas de O
colocarFichaO::Tablero -> Pos -> Tablero
colocarFichaO (Panel xs os) p = (Panel xs (p:os))



-- Dado un tablero, una posicion nueva, y una antigua, el punto nuevo pasa a formar parte de las fichas de X,
-- y se elimina el antiguo
cambiarFichaX :: Tablero -> Pos -> Pos -> Tablero
cambiarFichaX (Panel xs os) p oldP = (Panel (p:newLista) os)
                                    where
                                        newLista = filter (oldP /=) xs



-- Dado un tablero, una posicion nueva, y una antigua, el punto nuevo pasa a formar parte de las fichas de O,
-- y se elimina el antiguo
cambiarFichaO :: Tablero -> Pos -> Pos -> Tablero
cambiarFichaO (Panel xs os) p oldP = (Panel xs (p:newLista))
                                    where
                                        newLista = filter (oldP /=) os



-- Dado dos listas de posiciones, indica si la primera fila es subconjunto de la segunda
subLista :: [Pos] -> [Pos] -> Bool
subLista (x1:x2:x3:_) (xs) =    if((isOnList xs x1) && (isOnList xs x2) && (isOnList xs x3)) then True
                            else False



-- Dado una lista de posiciones, indica si hay 3 fichas en fila
tresEnRaya :: [Pos] -> Bool
tresEnRaya xs = enRaya1 || enRaya2 || enRaya3 || enRaya4 || enRaya5|| enRaya6 || enRaya7 || enRaya8
                            where
                                enRaya1 = subLista [1,2,3] xs
                                enRaya2 = subLista [4,5,6] xs
                                enRaya3 = subLista [7,8,9] xs
                                enRaya4 = subLista [1,4,7] xs
                                enRaya5 = subLista [2,5,8] xs
                                enRaya6 = subLista [3,6,9] xs
                                enRaya7 = subLista [1,5,9] xs
                                enRaya8 = subLista [3,5,7] xs



-- Dado un tablero, indica si alguno de los dos ha ganado
tresEnRayaXO :: Tablero -> Bool
tresEnRayaXO (Panel xs os) = tresEnRaya xs || tresEnRaya os
