module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix
import System.IO.Unsafe

data State = Black | Red
                     deriving (Show, Eq)
data W a = W a | Tie | None

data Diagonal = L | R

data Hard = Easy | Hard
data Mode = Hum_Comp | Hum_Hum
data Time = Limit | No_limit
type Pause = Bool


data MouseEvent = Click | Move

data Menu = Main {anum :: Int} | Opt | Empty

type Win = W State
               
type Cell = Maybe State 

type Field = Matrix Cell
type PointI = (Int,Int)

data World = World
          { field:: Field                 -- матрица значений
          , state:: State                 -- чей ход
          , win  :: Win                   -- флаг конца игры
          , pic  :: [Picture]             -- загруженные изображения
          , back :: Maybe World           -- отмена хода (прошлый мир)
          , timer:: PointI                -- таймер для обоих игроков 
          , menu :: Menu                  -- объекты меню
          , mode :: (Time,Hard,Mode,Pause)--состояния игры
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
   rejnzu     <- loadBMP "img/rejnzu.bmp"  --0
   red_win    <- loadBMP "img/red_win.bmp" --1
   black_win  <- loadBMP "img/black_win.bmp"   --2
   tie        <- loadBMP "img/tie.bmp"         --3
   play_game  <- loadBMP "img/play_game.bmp"   --4
   texture    <- loadBMP "img/texture.bmp"     --5
   timer_b    <- loadBMP "img/b.bmp"           --6
   timer_r    <- loadBMP "img/r.bmp"           --7
   back       <- loadBMP "img/bmp/back.bmp"    --8
   button     <- loadBMP "img/bmp/button.bmp"  --9
   fon        <- loadBMP "img/bmp/fon.bmp"    --10
   hard       <- loadBMP "img/bmp/hard.bmp"   --11
   hardness   <- loadBMP "img/bmp/hardness.bmp"--12
   load       <- loadBMP "img/bmp/load.bmp"   --13
   menu       <- loadBMP "img/bmp/menu.bmp"   --14
   mode       <- loadBMP "img/bmp/mode.bmp"   --15
   ok         <- loadBMP "img/bmp/ok.bmp"     --16
   options    <- loadBMP "img/bmp/options.bmp"--17
   options_1  <- loadBMP "img/bmp/options_1.bmp"--18
   save       <- loadBMP "img/bmp/save.bmp"     --19
   time       <- loadBMP "img/bmp/time.bmp"     --20
   time_2     <- loadBMP "img/bmp/time_2.bmp"   --21
   time_text  <- loadBMP "img/bmp/time_text.bmp"--22
   x          <- loadBMP "img/bmp/x.bmp"        --23
   easy       <- loadBMP "img/bmp/easy.bmp"     --24
   h_h        <- loadBMP "img/bmp/h.bmp"        --25
   h_c        <- loadBMP "img/bmp/h_c.bmp"      --26
   
             
   go (World (matrixFiling sizeField) Black None [rejnzu,red_win,black_win,tie,play_game,texture,timer_b,timer_r,back,button,fon,hard,hardness,load,menu,mode,ok,options,options_1,save,time,time_2,time_text,x, easy,h_h,h_c] Nothing (20,20) Empty (No_limit,Easy,Hum_Hum,False))

drawMenu :: [Picture] -> Menu -> [Picture]
drawMenu p (Main 0) = drawMain p
drawMenu p (Main n) = (zipWith (\dy i -> translate offsetX (offsetY + dy) $ p !! i)
                              [0,c]
                              [10,9]) ++ (tail $ drawMain p)
                      where
                      c = (fromIntegral sizeField) / 2 * sizeCell - 140 - 60 * fromIntegral(n - 1)
drawMenu p Opt = drawOpt p
drawMenu p _ = [Blank]


drawMain :: [Picture] -> [Picture]
drawMain p =  [translate offsetX offsetY $ p !! 10,
              translate offsetX (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 60 ) $ p !! 14,
              translate offsetX (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 140 ) $ p !! 13,
              translate offsetX (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 200 ) $ p !! 19,
              translate offsetX (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 260 ) $ p !! 17,
              translate (offsetX + 170) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 50) $ p !! 23]

drawOpt :: [Picture] -> [Picture]
drawOpt   p = [translate offsetX offsetY $ p !! 10,
              translate (offsetX + 170) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 50 ) $ p !! 23,
              translate offsetX (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 60 ) $ p !! 18,
              translate (offsetX - 120 ) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 120 ) $ p !! 22,
              translate (offsetX + 35) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 120 ) $ p !! 20,
              translate (offsetX + 130) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 120 ) $ p !! 21,
              translate (offsetX - 100 ) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 200 ) $ p !! 12,
              translate (offsetX + 35 ) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 200 ) $ p !! 11,
              translate (offsetX + 130 ) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 200 ) $ p !! 24,
              translate (offsetX + 35 ) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 200 ) $ p !! 16,
              translate (offsetX - 110 ) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 280 ) $ p !! 15,
              translate (offsetX + 35 ) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 280 ) $ p !! 25,
              translate (offsetX + 130 ) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 280 ) $ p !! 26,
              translate (offsetX - 150 ) (offsetY + (fromIntegral sizeField) / 2 * sizeCell - 330 ) $ p !! 8]

--запуск игры
go :: World -> IO ()
go world = play (InWindow "Game Rejnzu" (500,500) (0,0)) 
               white 
               1
               world 
               convert 
               handle
               update


--загрузка из файла
loadFile :: IO (Matrix Cell,(State,(String,String)))
loadFile = do
           file1 <- readFile "save/save_w.txt"
           file2 <- readFile "save/save_st.txt"
           fail "error to open file"
           return (rec_matrix 1 1 file1 $ matrixFiling sizeField, rec_state file2)

--загрузка матрицы
rec_matrix :: Int -> Int -> String -> Matrix Cell -> Matrix Cell
rec_matrix _ _ [] m = m
rec_matrix i j (x:xs) m             | x == '\n' = rec_matrix (i + 1) 1 xs m
                                    | x == ' '  = rec_matrix i j xs m 
                                    | otherwise = rec_matrix i (j + 1) xs (setElem (f x) (i,j) m)
                                    where
                                    f '0' = Nothing
                                    f '1' = (Just Black)
                                    f '2' = (Just Red)
--загрузка того, кто ходит и времени
rec_state :: String -> (State,(String,String))
rec_state (x:xs)   | x == 'B' = (Black, rec_time (tail xs) [] ("",""))
                   | x == 'R' = (Red, rec_time (tail xs) [] ("",""))

rec_time :: String -> String -> (String,String) -> (String,String)
rec_time [] buf s = s
rec_time (x:xs) buf (a,b)   | x == '\n' = (a,reverse buf)
                            | x == ' '  = rec_time xs [] (reverse buf,"")
                            | otherwise = rec_time xs (x:buf) (a,b)



loadGame :: (Matrix Cell,(State,(String,String)))
loadGame = (unsafePerformIO loadFile)

--изменяет таймер
update ::Float -> World -> World
update _ (World a state c d e (x,y) m (x1,y1,z1,p))  | (p == True) = (World a state c d e (x,y) m (x1,y1,z1,p))
                                                     | (x == 0) || (y == 0) = (World a state c d e (x,y) m (x1,y1,z1,p))
                                                     | (x == 1) = (World a state (W Red) d e (0,y) m (x1,y1,z1,p))
                                                     | (y == 1) = (World a state (W Black) d e (x,0) m (x1,y1,z1,p))
                                                     | otherwise            = (World a state c d e (f state) m (x1,y1,z1,p))
                                                     where
                                                     f Red     = (x, y-1)
                                                     f Black   = (x-1, y)

--переводит внутреннее представление мира в картинку
convert :: World -> Picture
convert (World m _ w p _ t menu (ti,ha,mo,pa)) =
                           Pictures $
                           drawPic w p ++ 
                           time t p    ++ 
                           mainDrawField m ++
                           drawMenu p menu 

time :: PointI -> [Picture] -> [Picture]
time (x,y) p = zipWith (\ z dx -> translate (offsetX + dx) (offsetY + (fromIntegral sizeField) / 2 * sizeCell) $ Scale 0.3 0.3 $ Text $ show z)
             [x,y] [c2,c1]
             ++
             zipWith (\ dx i -> translate (offsetX + dx) (offsetY + (fromIntegral sizeField) / 2 * sizeCell + 50) $ p !! i)
             [c4,c3] [6,7]
             where 
             c1         = (fromIntegral sizeField) / 2 * sizeCell - 35
             c2         = - c1 - 50
             c3         = c1 + 20
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
handle (EventKey (Char 'z') Down _ _) (World a b c d e f Empty (x,y,z,p)) = World a b c d e f (Main 0) (x,y,z,True)
handle (EventMotion (x,y)) (World a b c d e f (Main g) (x1,y1,z1,p)) = handle_menu Move (x,y) (World a b c d e f (Main g) (x1,y1,z1,True)) 
handle (EventKey (MouseButton LeftButton) Down _ (x,y)) (World a b c d e f (Main g) (x1,y1,z1,p)) = handle_menu Click (x,y) (World a b c d e f (Main g) (x1,y1,z1,True))
handle (EventKey (MouseButton LeftButton) Down _ (x,y)) (World a b c d e f Opt (x1,y1,z1,p)) = handle_menu Click (x,y) (World a b c d e f Opt (x1,y1,z1,True))

handle _ (World a b c d e f g (x,y,z,True)) = (World a b c d e f g (x,y,z,True))
handle (EventKey (SpecialKey KeySpace) Down _ _) w =  getback w
handle       _                  (World m s (W Red) p b t menu (x,y,z,v)) = World m s (W Red) p b t menu (x,y,z,v)
handle       _                  (World m s (W Black) p b t menu (x,y,z,v)) = World m s (W Black) p b t menu (x,y,z,v)
handle (EventKey (MouseButton LeftButton) Down _ (x,y)) w = checkWorld (mainNumberRow (x,y),mainNumberCol (x,y)) w
handle _ w = w

handle_menu :: MouseEvent -> Point -> World -> World
handle_menu event (x,y) (World a b c d e f (Main g) (x1,y1,z1,p)) | (x >= (offsetX - 120) && x <= (offsetX + 120)) && (y >= (-10) && y <= 30) 
                                                       = case event of
                                                         Move -> (World a b c d e f (Main 1) (x1,y1,z1,p))
                                                         Click-> (World a b c d e f (Main 1) (x1,y1,z1,p))
                                                     | (x >= (offsetX - 120) && x <= (offsetX + 120)) && (y >= (-70) && y <= (-30)) 
                                                       = case event of
                                                         Move -> (World a b c d e f (Main 2) (x1,y1,z1,p))
                                                         Click-> (World a b c d e f (Main 2) (x1,y1,z1,p))
                                                     | (x >= (offsetX - 120) && x <= (offsetX + 120)) && (y >= (-130) && y <= (-90)) 
                                                       = case event of
                                                         Move -> (World a b c d e f (Main 3) (x1,y1,z1,p))
                                                         Click-> (World a b c d e f Opt (x1,y1,z1,p))
                                                     |(x >= (offsetX + 150) && x<= (offsetX + 190) && y>=90 && y<=120)
                                                       = case event of
                                                         Click->  World a b c d e f Empty (x1,y1,z1,False)
                                                         Move ->  World a b c d e f (Main g) (x1,y1,z1,p)
                                                     | otherwise = World a b c d e f (Main g) (x1,y1,z1,p)






--
getback :: World -> World
getback (World m s w p Nothing t men x) = (World m s w p Nothing t men x)
getback (World _ _ _ _ (Just b) t _ _) = b 

--обрабочик мира
checkWorld :: PointI -> World -> World
checkWorld (0,_) m = m
checkWorld (_,0) m = m
checkWorld coord (World m s l p b t menu (x,y,z,ps)) | m ! coord == Nothing  =  World
                                                           (putIn coord s m)
                                                           (inverseState s)
                                                            (gameRules coord s (putIn coord s m))
                                                            p
                                                            (Just (World m s l p b t menu (x,y,z,ps)))
                                                            t
                                                            menu
                                                            (x,y,z,ps)
                                     | otherwise             =  World m s l p b t menu (x,y,z,ps)

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



