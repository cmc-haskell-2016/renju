module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix

data State = Black | Red
                     deriving (Show, Eq)
data W a = W a | Tie | None

data Diagonal = L | R

type Win = W State
               
type Cell = Maybe State 

type Field = Matrix Cell
type PointI = (Int,Int)

data World = World
          { field :: Field        -- матрица значений
          , state :: State        -- чей ход
          , win :: Win            -- флаг конца игры
          , pic :: [Picture]      -- загруженные изображения
          , back :: Maybe World   -- отмена хода (прошлый мир)
          , timer :: PointI       -- таймер для обоих игроков 
          }

--размер ячейки
sizeCell :: Float
sizeCell = 40.0

--размерность матрицы (размерность сетки + 1) 
sizeField :: Int
sizeField = 10

--положение центра игровой доски
offsetX :: Float
offsetX = 0

offsetY :: Float
offsetY = (-50)

--заполнение матрицы разерности n
matrixFiling :: Int -> Matrix Cell
matrixFiling n = matrix n n $ \ _ -> Nothing

--возвращает список - строку матрицы 
getRow :: Int -> Matrix Cell -> [Cell] 
getRow n m = [m ! (i,j) | i <- [n]
                          , j <- [1 .. (ncols m)]]


main :: IO ()
main    
 = do
   rejnzu     <- loadBMP "img/rejnzu.bmp"
   red_win    <- loadBMP "img/red_win.bmp"
   black_win  <- loadBMP "img/black_win.bmp"
   tie        <- loadBMP "img/tie.bmp"
   play_game  <- loadBMP "img/play_game.bmp"
   texture    <- loadBMP "img/texture.bmp"
   timer_b    <- loadBMP "img/b.bmp"
   timer_r    <- loadBMP "img/r.bmp"
   go (World (matrixFiling sizeField) Black None [rejnzu,red_win,black_win,tie,play_game,texture, timer_b,timer_r] Nothing (20,20))

--запуск игры
go :: World -> IO ()
go world = play (InWindow "Game Rejnzu" (500,500) (0,0)) 
               white 
               1
               world 
               convert 
               handle
               update

--изменяет таймер
update ::Float -> World -> World
update _ (World a state c d e (x,y)) | (x == 0) || (y == 0) = (World a state c d e (x,y))
                                     | (x == 1) = (World a state (W Red) d e (0,y))
                                     | (y == 1) = (World a state (W Black) d e (x,0))
                                     | otherwise            = (World a state c d e $ f state)
                                     where
                                     f Red     = (x, y-1)
                                     f Black   = (x-1, y)

--переводит внутреннее представление мира в картинку
convert :: World -> Picture
convert (World m _ w p _ t)  = Pictures $
                           drawPic w p ++ 
                           time t p    ++ 
                           mainDrawField m

time :: PointI -> [Picture] -> [Picture]
time (x,y) p = zipWith (\ z dx -> translate (offsetX + dx) (offsetY + (fromIntegral sizeField) / 2 * sizeCell) $ Scale 0.3 0.3 $ Text $ show z)
             [x,y] [c2,c1]
             ++
             zipWith (\ dx i -> translate (offsetX + dx) (offsetY + (fromIntegral sizeField) / 2 * sizeCell + 50) $ p !! i)
             [c4,c3] [6,7]
             where 
             c1         = (fromIntegral sizeField) / 2 * sizeCell - 35
             c2         = - c1 - 50
             c3         = c1 + 30 
             c4         = c2 + 30

--отрисовка img
drawPic :: Win -> [Picture] -> [Picture]
drawPic x p = case x of
              None ->         zipWith(\dy i -> translate offsetX (offsetY + dy) $ p !! i)
                                      [1,220,270] 
                                      [5,4,0]
                                
              _     ->        zipWith(\dy i -> translate offsetX (offsetY + dy) $ p !! i)
                                     [1,270,220]
                                     [5,0,msg x]
              where
                  msg (W Black) = 2
                  msg (W Red)   = 1
                  msg (Tie)     = 3 

--отрисовка одной ячейки (позиции - положение квадратика относительно центра) и левой верхней фишечки, если она есть
drawCell :: Float -> Float -> Cell -> [Picture]
drawCell pos_x pos_y s = case s of
                             Nothing -> [reckWire] 
                             (Just x) -> [reckWire
                                         ,Translate 
                                         (pos_x - (sizeCell / 2))
                                         (pos_y + (sizeCell / 2)) $
                                         Color (col x) $ circleSolid (sizeCell / 2)]
                         where
                         reckWire =  translate 
                                     pos_x 
                                     pos_y $
                                     rectangleWire sizeCell sizeCell
                         col Black = black
                         col Red   = red

--отрисовка строки
--pos_x pos_y положение самого первого квадратика строки относительно центра

drawRow :: Float -> Float -> [Cell] -> [Picture]
drawRow pos_x pos_y [(Just x)] = [Translate 
                                                   (pos_x - (sizeCell / 2)) 
                                                   (pos_y + (sizeCell / 2)) $
                                                   Color (col x) $ circleSolid (sizeCell / 2)]
                                 where 
                                 col Black = black
                                 col Red   = red
drawRow _     _     [Nothing]         =     [Blank]

drawRow pos_x pos_y l                 =     (drawCell pos_x pos_y $ head l) 
                                                ++ (drawRow (pos_x + sizeCell) pos_y $ tail l)

--отрисовка всей сетки
mainDrawField :: Field -> [Picture]
mainDrawField m  =                            drawField 
                                                        (offsetX - sizeCell * fromIntegral (((nrows m) - 1) `div` 2)) 
                                                        (offsetY + sizeCell * fromIntegral (((nrows m) - 1) `div` 2)) 
                                                        ((nrows m) - 1) 
                                                        m

--n количество оставшихся на отрисовку строк 

drawField :: Float -> Float -> Int -> Matrix Cell -> [Picture]
drawField pos_x pos_y 0 m          =     drawLastRow 
                                                        (pos_x - (sizeCell / 2)) 
                                                        (pos_y + (sizeCell / 2)) 
                                                        $ Main.getRow 1 m
drawField pos_x pos_y n m          =     (drawRow pos_x pos_y $ Main.getRow 1 m) 
                                                ++ (drawField 
                                                        pos_x (pos_y - sizeCell) 
                                                        (n - 1) 
                                                        (submatrix 2 (nrows m) 1 (ncols m) m)) 

--отрисовка последних фишичек
drawLastRow :: Float -> Float -> [Cell] -> [Picture]
drawLastRow _ _ []                 =      [Blank]
drawLastRow pos_x pos_y l          =      (drawLastCell 
                                                         pos_x 
                                                         pos_y $ 
                                                         head l) 
                                                     ++ (drawLastRow (pos_x + sizeCell)   pos_y  $ tail l)

--отрисовка фишки последней строки (не рисуе квадратик)
drawLastCell :: Float -> Float -> Cell -> [Picture]
drawLastCell _ _ Nothing                  =       [Blank]
drawLastCell pos_x pos_y (Just x)         =       [Translate 
                                                         pos_x 
                                                         pos_y $ 
                                                         Color (col x) $ circleSolid (sizeCell / 2)]
                                          where
                                          col Black = black
                                          col Red = red

--обработка внешних событий
handle :: Event -> World -> World
handle (EventKey (SpecialKey KeySpace) Down _ _) w =  getback w
handle       _                  (World m s (W Red) p b t) = World m s (W Red) p b t
handle       _                  (World m s (W Black) p b t) = World m s (W Black) p b t
handle (EventKey (MouseButton LeftButton) _ _ (x,y)) w = checkWorld (mainNumberRow (x,y),mainNumberCol (x,y)) w
handle _ w = w

--
getback :: World -> World
getback (World m s w p Nothing t) = (World m s w p Nothing t)
getback (World _ _ _ _ (Just b) t) = b 

--обрабочик мира
checkWorld :: PointI -> World -> World
checkWorld (0,_) m = m
checkWorld (_,0) m = m
checkWorld coord (World m s l p b t) | m ! coord == Nothing  =  World
                                                           (putIn coord s m)
                                                           (inverseState s)
                                                            (gameRules coord s (putIn coord s m))
                                                            p
                                                            (Just (World m s l p b t))
                                                            t
                                 | otherwise             =  World m s l p b t

--получение номера столбца
mainNumberCol :: Point -> Int
mainNumberCol x                          = numberCol
                                           x
                                           (offsetX - sizeCell - fromIntegral((sizeField - 1) `div` 2) * sizeCell) 
                         

numberCol :: Point -> Float -> Int
numberCol (x,_) n | x < n || x > (n + fromIntegral(sizeField) * sizeCell) = 0
                  | otherwise                                             = div sizeField 2 + 1 + div (round (x - offsetX)) (round sizeCell)

--получение номера строки
mainNumberRow :: Point -> Int
mainNumberRow x = numberRow
                          x
                         (offsetY + sizeCell + fromIntegral((sizeField - 1) `div` 2) * sizeCell) 
                         

numberRow :: Point -> Float -> Int
numberRow (_,y) n | y > n || y < (n - fromIntegral(sizeField) * sizeCell) = 0
                  | otherwise                                             = div sizeField 2 - div (round ( y - offsetY)) (round sizeCell)

--заполнение ячейки матрицы
putIn :: PointI -> State -> Matrix Cell -> Matrix Cell
putIn (a,b) Black m = setElem (Just Black) (a,b) m
putIn (a,b) Red m   = setElem (Just Red) (a,b) m 

--инвертирование состояния
inverseState :: State -> State
inverseState Black = Red
inverseState Red = Black

--получение окрестности 5 для строки
getNeighRow :: PointI -> Matrix Cell -> [Cell]
getNeighRow (a,b) m = [m ! (i,j) | i <- [a]
                          , j <- [(b - 4) .. (b + 4)]
                          , j >= 1 && j <= (ncols m)]

--получение окрестности 5 для столбца
getNeighCol :: PointI -> Matrix Cell -> [Cell]
getNeighCol (a,b) m = [m ! (i,j) | i <- [(a - 4) .. (a + 4)]
                          , j <- [b]
                          , i >= 1 && i <= (nrows m)]

--получение окрестности 5 по диагонали влево

getNeighDiag :: PointI -> Matrix Cell -> Diagonal -> [Cell]
getNeighDiag (a,b) m d = diag (a,b) m (a + 4) (b + 4) d


diag :: PointI -> Matrix Cell -> Int -> Int -> Diagonal -> [Cell]
diag (a,b) m i j d
                     | i == (a - 5) = []
                     | i >= 1 && i <= (nrows m) && j>=1 && j <= (ncols m) = m ! (i,j) : (diag (a,b) m (i - 1) (lr d) d)
                     | otherwise = diag (a,b) m (i - 1) (lr d) d
                     where
                     lr L = j - 1
                     lr R = j + 1

--по [Cell] определяет, есть ли выигрыш

winner :: [Cell] -> Bool
winner l = winFunc l 0 

winFunc :: [Cell] -> Int -> Bool
winFunc _ 4 = True
winFunc (x : (y : xs)) ac | x == y && (x /= Nothing) = winFunc (y : xs) (ac + 1) 
                          | otherwise = winFunc (y:xs) 0 
winFunc _ _ = False

--Определяет, есть ли выигрыш в игре
--None - никто, Tie - полностью заполнена, иначе победитель (W Black |W Red)

gameRules :: PointI -> State -> Matrix Cell ->Win
gameRules (x,y) s m
                  | winner ( getNeighDiag (x,y) m L) || 
                      winner ( getNeighDiag (x,y) m R) || 
                      winner ( getNeighRow (x,y) m ) ||
                      winner ( getNeighCol (x,y) m ) 
                    = (W s)
                  | fullBoard (toList m) = Tie
                  | otherwise = None

--заполнена ли доска

fullBoard :: [Cell] -> Bool
fullBoard [] = True
fullBoard (Nothing : _) = False
fullBoard l = fullBoard (tail l)



