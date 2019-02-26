import System.Random
import Data.List

pop_size = 100   :: Int     -- размер популяции
pc       = 0.3   :: Double  -- вероятность кроссинговера
pm       = 0.2   :: Double  -- вероятность мутаций
a        = 0.05  :: Double  -- параметр для функции оценки
m        = 1000  :: Double  -- параметр мутации

data Point = Point Double Double Double deriving Show

instance Num Point where
    (+) (Point x1 x2 x3) (Point y1 y2 y3) = Point (x1 + y1) (x2 + y2) (x3 + y3)
    (-) (Point x1 x2 x3) (Point y1 y2 y3) = Point (x1 - y1) (x2 - y2) (x3 - y3)
    (*) (Point x1 x2 x3) (Point y1 y2 y3) = Point (x1 * y1) (x2 * y2) (x3 * y3)
    abs (Point x1 x2 x3) = Point (abs x1) (abs x2) (abs x3)
    signum (Point x1 x2 x3) = let x = signum x1 + x2 + x3 
                              in Point x x x
    fromInteger a = let x = fromInteger a 
                    in Point x x x

instance Fractional Point where
    (/) (Point x1 x2 x3) (Point y1 y2 y3) = Point (x1/y1) (x2/y2) (x3/y3) 
    fromRational a = let x = fromRational a 
                     in Point x x x

-- Целевая функция
target_fun :: Point -> Double
target_fun (Point x y z) = sqrt x + sqrt y + sqrt z

-- Вводимые ограничения
isCorrect_point :: Point -> Bool
isCorrect_point (Point x y z)
     | r1 && r2 && r3 = True
     | otherwise = False
     where r1 = x > 0 && y > 0 && z >  0 
           r2 = x < 1 && y < 1 && z <  1
           r3 = x^2 + 2*y^2 + 3*z^2 <= 1

-- Функция ранжирования
eval :: Int -> Double
eval n = a*(1-a)^(n-1)

-- Инициализатор особи
create_point :: IO Point
create_point = do
    x <- randomRIO (0, 1)
    y <- randomRIO (0, 1)
    z <- randomRIO (0, 1)
    let p = Point x y z
    if isCorrect_point p 
        then return p
        else create_point

-- Инициализатор популяции
create_pop :: IO [Point]
create_pop = 
    let create_pop' 0 = [] 
        create_pop' n = create_point : create_pop' (n-1)
    in  sequence $ create_pop' pop_size

-- Ранжирование популяции от лучшей к худшей
sort_pop :: [Point] -> [Point]
sort_pop p = reverse $ sortOn target_fun p

-- НАЧАЛО. СЕЛЕКЦИЯ 
-- Выбирается pop_size лучших особей.
selection :: [Point] -> IO [Point]
selection p = selection' pop_size p where 
     selection' 0 _ = return []
     selection' n p = do
            let q = map cumul_poss [1..pop_size]
            a <- randomRIO (0, last q)
            let i = select_num a q
                x = p !! (i-1)
            (return [x]) <> (selection' (n-1) p) 
            where 
                cumul_poss n = sum $ map eval [1..n]
                select_num r (x:xs) = 1 + (if r > x then select_num r xs else 0)
-- КОНЕЦ. СЕЛЕКЦИЯ

-- НАЧАЛО. КРОССИНГОВЕР 
-- Отбор родительских хромосом. На первом месте в паре -- будущие родители, на втором -- все оставшиеся
select_parents :: [Point] -> IO ([Point], [Point])
select_parents p = select_parents' 0 p where
    select_parents' n p
         | n == pop_size = return ([],[])
         | otherwise = do
            a <- randomRIO (0, 1 :: Double)
            if a < pc 
                then return ([p !! n], []) <> select_parents' (n+1) p 
                else return ([], [p !! n]) <> select_parents' (n+1) p

-- Образовываем родительские пары. Кому-то может не достаться особи. Такая особь отправляется во вторую группу.
make_pair :: ([Point], [Point]) -> ([(Point, Point)], [Point])
make_pair ([], a) = ([], a)
make_pair (y:[], a) = ([], y:a)
make_pair (x:y:ss, a) = ([(x,y)], []) <> make_pair (ss, a)

-- Операция, обратная make_pair
rev_make_pair :: ([(Point, Point)], [Point]) -> ([Point], [Point])
rev_make_pair ([]  , y) = ([], y)
rev_make_pair ((x:xs), y) = ([fst x], []) <> ([snd x], []) <> rev_make_pair (xs, y)

-- Объединением пары из потомков и особей, не ставших родителями 
union_pair :: ([Point], [Point]) -> [Point]
union_pair (x, y) = x <> y

-- Кроссинговер между парой особей
crossingover' :: (Point, Point) -> IO (Point, Point)
crossingover' (x, y) = do
     a <- randomRIO (0, 1 :: Double)
     let a' = Point a a a
         x' = a'*x + (1-a')*y
         y' = (1-a')*x + a'*y
     if isCorrect_point x' && isCorrect_point y' 
        then return (x', y')
        else crossingover' (x, y)

-- Кроссинговер между особями на множестве особей
crossingover :: [Point] -> IO [Point]
crossingover p = do
     p'     <- select_parents p
     let ps =  make_pair p'
     pc'    <- sequence $ map crossingover' (fst ps)
     pc''   <- return   $ snd ps
     return (union_pair.rev_make_pair $ (,) pc' pc'')
-- КОНЕЦ. КРОССИНГОВЕР 

-- НАЧАЛО. СЛУЧАЙНЫЕ МУТАЦИИ
mutation :: Point -> IO Point
mutation p = do
    a <- randomRIO (0, 1 :: Double)
    d <- create_point
    if a > pm then  return p else  mutation' p d 0
        where mutation' p d 100 = return p
              mutation' p d n   = do
                m'     <- randomRIO (0, m)
                let p' =  p + d * (Point m' m' m')
                if isCorrect_point p' then return p' else mutation' p d (n+1)
-- КОНЕЦ. СЛУЧАЙНЫЕ МУТАЦИИ

main :: IO ()
main = do
    let first_population    = create_pop
        step_of_evolution p = p >>= crossingover >>= sequence . map mutation >>= return . sort_pop >>= selection
        print_pop         p = p >>= (mapM_ putStrLn).(map show)
    p' <- (iterate step_of_evolution first_population) !! 200
    let m' = head $ sort_pop p'
    putStrLn ("Лучшая особь\t"       ++ (show m'))
    putStrLn ("Целевое значение\t\t" ++ (show $ target_fun m'))
