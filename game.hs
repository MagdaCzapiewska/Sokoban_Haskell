{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.Text (pack)
type Program = IO () -- program wykonuje IO i nie daje wartości 
type Maze = Coord -> Tile
type DrawFun = State -> Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun

data SSState world = StartScreen | Running world
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D deriving Eq
data Coord = C {coordx, coordy :: Int}
data WithUndo a = WithUndo a [a]
data State = S {
  stPlayer :: Coord,
  stDir    :: Direction,
  stBoxes  :: [Coord],
  stLevel  :: Integer,
  stMove   :: Integer,
  stXdim   :: [Integer],
  stYdim   :: [Integer]
} deriving Eq
data Activity world = Activity {
  actState  :: world,
  actHandle :: (Event -> world -> world),
  actDraw   ::(world -> Screen)
}

data Event = KeyPress String
type Screen = String

instance Eq Coord where
  C x y == C x' y' = x == x' && y == y'

main :: Program
main = program       -- dorośli używają main jako głównej funkcji

program :: Program
program = etap5

startScreen :: Screen
startScreen = "Sokoban!\n" ++ etap4

winScreen :: Integer -> Screen
winScreen mov = "Poziom ukończony, liczba ruchów: " ++ (show mov)

greenColor :: String
greenColor = "\x1b[32m"

redColor :: String
redColor = "\x1b[31m"

defaultColor :: String
defaultColor = "\x1b[0m"

yellowColor :: String
yellowColor = "\x1b[33m"

lightGreenColor :: String
lightGreenColor = "\x1b[32;1m"

whiteColor :: String
whiteColor = "\x1b[37;1m"

-- Wall 	#
-- Player 	@
-- Player on goal square 	+
-- Box 	$
-- Box on goal square 	*
-- Goal square 	.
-- Floor 	(Space) 

wallChar :: Char
wallChar = '#'

playerChar :: Char
playerChar = '@'

playerOnGoalSquareChar :: Char
playerOnGoalSquareChar = '+'

boxChar :: Char
boxChar = '$'

boxOnGoalSquareChar :: Char
boxOnGoalSquareChar = '*'

goalSquareChar :: Char
goalSquareChar = '.'

floorChar :: Char
floorChar = ' '

blankChar :: Char
blankChar = '-'

drawTile :: Tile -> Char
drawTile Wall    = wallChar
drawTile Ground  = floorChar
drawTile Storage = goalSquareChar
drawTile Box     = boxChar
drawTile Blank   = blankChar

coloredChar :: Char -> String
coloredChar '#' = greenColor ++ "#" ++ defaultColor
coloredChar '-' = whiteColor ++ "-" ++ defaultColor
coloredChar '$' = redColor ++ "$" ++ defaultColor
coloredChar '*' = lightGreenColor ++ "*" ++ defaultColor
coloredChar '@' = yellowColor ++ "@" ++ defaultColor
coloredChar '+' = yellowColor ++ "+" ++ defaultColor
coloredChar c = [c]

-- good mazes and starting positions (naming: maze + G + number)

mazeG01 :: Int -> Int -> Tile
mazeG01 x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

initPosG01 :: Coord
initPosG01 = C (-3) (-3)

initDirG01 :: Direction
initDirG01 = U

mazeG02 :: Int -> Int -> Tile
mazeG02 x y
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x == 0 && y <= 1             = Wall
  | x > 0 && y == -3             = Storage
  | x < 0  && x > -3 && y == -1  = Box
  | x == 0 && y == 2             = Box
  | otherwise                    = Ground

initPosG02 :: Coord
initPosG02 = C (-3) (-3)

initDirG02 :: Direction
initDirG02 = U

mazeG03 :: Int -> Int -> Tile
mazeG03 x y
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x <= 1 && y == 2             = Wall
  | x >= -1 && y == 0            = Wall
  | x <= 1 && y == -2            = Wall
  | x == -3 && y == 3            = Storage
  | x == -3 && y == -3           = Storage
  | x == 2  && y == 3            = Box
  | x == 2 && y == -3            = Box
  | otherwise                    = Ground

initPosG03 :: Coord
initPosG03 = C (-3) 0

initDirG03 :: Direction
initDirG03 = U

mazeG04 :: Int -> Int -> Tile
mazeG04 x y
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x <= 0 && y == (-x)          = Wall
  | x <= 1 && y == -(x-1)        = Storage
  | x >= 0 && x <= 2  && y == -2 = Box
  | otherwise                    = Ground

initPosG04 :: Coord
initPosG04 = C (-3) 0

initDirG04 :: Direction
initDirG04 = U


-- bad mazes and starting positions (naming: maze + B + number)

mazeB01 :: Int -> Int -> Tile
mazeB01 x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x /= 3 && y == -x        = Wall
  | x <= -2 && y == -3       = Storage
  | x >= 2 && y == 1         = Box
  | otherwise                = Ground

initPosB01 :: Coord
initPosB01 = C 3 3

initDirB01 :: Direction
initDirB01 = D

mazeB02 :: Int -> Int -> Tile
mazeB02 x y
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | abs x == 2 && abs y <= 2  = Wall
  | abs x <= 2 && abs y == 2  = Wall
  | x == -3 && y == -3        = Storage
  | x == 0 && y == 0          = Box
  | otherwise                 = Ground

initPosB02 :: Coord
initPosB02 = C 3 3

initDirB02 :: Direction
initDirB02 = D

mazeB03 :: Int -> Int -> Tile
mazeB03 x y
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | abs x == 2 && abs y <= 2  = Wall
  | abs x <= 2 && abs y == 2  = Wall
  | x == 0 && y == -1         = Storage
  | x == 0 && y == 0          = Box
  | otherwise                 = Ground

initPosB03 :: Coord
initPosB03 = C 3 3

initDirB03 :: Direction
initDirB03 = D

mazeB04 :: Int -> Int -> Tile
mazeB04 x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 0 && y <= 1         = Wall
  | x > 0 && y == -3         = Storage
  | x < 0 && y == -1         = Box
  | otherwise                = Ground

initPosB04 :: Coord
initPosB04 = C (-3) (-3)

initDirB04 :: Direction
initDirB04 = U

mazeB05 :: Int -> Int -> Tile
mazeB05 x y
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x <= 1 && y == 2             = Wall
  | x >= -1 && y == 0            = Wall
  | x <= 1 && y == -2            = Wall
  | x == -3 && y == 3            = Storage
  | x == -3 && y == -3           = Storage
  | x == 3  && y == 3            = Box
  | x == 3 && y == -3            = Box
  | otherwise                    = Ground

initPosB05 :: Coord
initPosB05 = C (-3) 0

initDirB05 :: Direction
initDirB05 = U

mazeB06 :: Int -> Int -> Tile
mazeB06 x y
  | abs x > 4  || abs y > 4  = Blank
  | x == -3 && y == 4        = Ground
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

initPosB06 :: Coord
initPosB06 = C (-3) (-3)

initDirB06 :: Direction
initDirB06 = U

mazeB07 :: Int -> Int -> Tile
mazeB07 x y
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | abs x == 2 && abs y <= 2  = Wall
  | abs x <= 2 && abs y == 2  = Wall
  | x == -3 && y == -3        = Storage
  | x == -3 && y == 3         = Storage
  | x == -2 && y == 3         = Box
  | x == 0 && y == 0          = Box
  | otherwise                 = Ground

initPosB07 :: Coord
initPosB07 = C 3 3

initDirB07 :: Direction
initDirB07 = D

mazeB08 :: Int -> Int -> Tile
mazeB08 x y
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x <= 0 && y == (-x)          = Wall
  | x <= -1 && y == -(x-1)       = Storage
  | x >= 0 && x <= 2  && y == -2 = Box
  | otherwise                    = Ground

initPosB08 :: Coord
initPosB08 = C (-3) 0

initDirB08 :: Direction
initDirB08 = U

mazeB09 :: Int -> Int -> Tile
mazeB09 x y
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x <= 1 && y == 2             = Wall
  | x >= -1 && y == 0            = Wall
  | x <= 1 && y == -2            = Wall
  | x == -3 && y == 3            = Storage
  | x == -3 && y == -3           = Storage
  | x == 2  && y == 3            = Box
  | x == 2 && y == -3            = Box
  | otherwise                    = Ground

initPosB09 :: Coord
initPosB09 = C 0 0

initDirB09 :: Direction
initDirB09 = U

mazeB10 :: Int -> Int -> Tile
mazeB10 x y
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x <= 1 && y == 2             = Wall
  | x >= -1 && y == 0            = Wall
  | x <= 1 && y == -2            = Wall
  | x == -3 && y == 3            = Storage
  | x == -3 && y == -3           = Box
  | x == 2  && y == 3            = Box
  | x == 2 && y == -3            = Wall
  | otherwise                    = Ground

initPosB10 :: Coord
initPosB10 = C 0 0

initDirB10 :: Direction
initDirB10 = U

-- Argument: function Int -> Int -> Tile, returns: function Coord -> Tile
mazeToMaze :: (Int -> Int -> Tile) -> Maze
mazeToMaze m = (\c -> (m (coordx c) (coordy c)))

-- Argument: function Coord -> Tile, returns: function Int -> Int -> Tile
bigMazeTomaze :: (Maze) -> (Int -> Int -> Tile)
bigMazeTomaze m = (\x y -> m (C x y))

-- There is type Maze (Coord -> Tile) already, so I'm naming new data NewMaze
data NewMaze = NewMaze {pos :: Coord, maz :: Coord -> Tile}
mazes :: [NewMaze]
mazes = [NewMaze initPosG01 (mazeToMaze mazeG01), NewMaze initPosG02 (mazeToMaze mazeG02), NewMaze initPosG03 (mazeToMaze mazeG03), NewMaze initPosG04 (mazeToMaze mazeG04)]
badMazes :: [NewMaze]
badMazes = [NewMaze initPosB01 (mazeToMaze mazeB01), NewMaze initPosB02 (mazeToMaze mazeB02), NewMaze initPosB03 (mazeToMaze mazeB03), NewMaze initPosB04 (mazeToMaze mazeB04), NewMaze initPosB05 (mazeToMaze mazeB05), NewMaze initPosB06 (mazeToMaze mazeB06), NewMaze initPosB07 (mazeToMaze mazeB07), NewMaze initPosB08 (mazeToMaze mazeB08), NewMaze initPosB09 (mazeToMaze mazeB09), NewMaze initPosB10 (mazeToMaze mazeB10)]

allMazes :: [NewMaze]
allMazes = appendList mazes badMazes

-- New version: (Coord -> Tile) -> (Coord -> Tile)
removeBoxes :: Maze -> Maze
removeBoxes m = f . m where f = (\t -> if t == Box then Ground else t)


addBoxes :: [Coord] -> Maze -> Maze
addBoxes c m = (\coordinates -> if (elemList coordinates c) then Box else (m coordinates))


adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)

remove :: [Coord] -> [Coord] -> Coord -> [Coord]
remove coords1 coords2 c
  | length(coords2) == 0 = coords1
  | (head coords2) == c = coords1 ++ (tail coords2)
  | otherwise = remove (coords1 ++ ([head coords2])) (tail coords2) c

removeAndAdd :: [Coord] -> Coord -> Coord -> [Coord]
removeAndAdd coords c1 c2 = (remove [] coords c1) ++ [c2]

-- moveBox returns old state if there is a Wall or a Box on the fields where the Box could be possibly moved
-- otherwise it returns new state
moveBox :: State -> Coord -> Coord -> Direction -> Direction -> [Coord] -> State
moveBox s c1 c2 d1 d2 coords
  | tileAt nextPos == Wall || elem nextPos coords = s { stPlayer = c1, stDir = d1, stBoxes = coords }
  | otherwise = s { stPlayer = c2, stDir = d2, stBoxes = removeAndAdd coords c2 nextPos,
                     stMove = stMove s + 1 }
  where 
    nextPos = adjacentCoord d2 c2 
    tileAt (C x y) = getMaze s x y
    getMaze = bigMazeTomaze . maz . nth allMazes . stLevel

-- checkAndMove returns old state if there is a Wall or a not movable Box on the field with new coordinates
-- otherwise it returns new state
checkAndMove :: State -> Tile -> Coord -> Coord -> Direction -> Direction -> [Coord] -> State
checkAndMove s t c1 c2 d1 d2 coords
  | t == Wall = S { stPlayer = oldPosition, stDir = oldDirection, stBoxes = coords, stLevel = (stLevel s), stMove = noMove, stXdim = (stXdim s), stYdim = (stYdim s) }
  | t == Box = moveBox s c1 c2 d1 d2 coords
  | otherwise = S {stPlayer = newPosition, stDir = newDirection, stBoxes = coords, stLevel = (stLevel s), stMove = addMove, stXdim = (stXdim s), stYdim = (stYdim s) }
  where
    oldPosition = c1
    oldDirection = d1
    newPosition = c2
    newDirection = d2
    noMove = stMove s
    addMove = (stMove s) + 1

-- movePlayer returns state of the game after the attempt to make a move
movePlayer :: Direction -> State -> State
movePlayer d s = let newPos@(C x y) = adjacentCoord d (stPlayer s) in  checkAndMove s (addBoxes (stBoxes s) (removeBoxes (maz (nth (appendList mazes badMazes) (stLevel s)))) newPos) (stPlayer s) newPos (stDir s) d (stBoxes s)


initialBoxes :: Integer -> [Coord]
initialBoxes lv = [C x y | x <- [-10..10], y <- [-10..10], (tileAt lv (C x y)) == Box, reachable (C x y) (pos (fromLevel lv)) (neighbourFunction (maz (fromLevel lv))) ]
  where
    tileAt l (C x y) = getMaze l x y
    getMaze = bigMazeTomaze . maz . fromLevel
    fromLevel = nth allMazes

initialState :: State
initialState = S { stPlayer = pos (nth (appendList mazes badMazes) 0), stDir = U, stBoxes =  (initialBoxes 0), stLevel = 0,  stMove = 0, stXdim = [-10..10], stYdim = [-10..10]}

gameState :: State
gameState = initialState

nextLevel :: State -> State
nextLevel s = let n = ((stLevel s) + 1) in S {stPlayer = pos (nth (appendList mazes badMazes) n), stDir = U, stBoxes =  (initialBoxes n), stLevel = n,  stMove = 0, stXdim = [-10..10], stYdim = [-10..10]}

handleEvent :: Event -> State -> State 
handleEvent (KeyPress key) s
    | key == "n" && isWinning s && ((stLevel s) < (listLength (appendList mazes badMazes)) - 1) = nextLevel s
    | isWinning s = s
    | key == "d" = movePlayer R s
    | key == "w"    = movePlayer U s
    | key == "a"  = movePlayer L s
    | key == "s"  = movePlayer D s
    | key == "n" && ((stLevel s) < (listLength (appendList mazes badMazes) - 1)) = nextLevel s
handleEvent _ s      = s


walkActivity :: Activity State
walkActivity = Activity { actState = gameState, actHandle = handleEvent, actDraw = draw }


walk5 :: IO ()
walk5 = runActivity ((resettable . withStartScreen . withUndo) walkActivity)


-- tileIgnoringBoxes tells what tile would be in a given place if there are no boxes
tileIgnoringBoxes :: DrawFun
tileIgnoringBoxes s x y = drawTile (tileAt (C (fromIntegral x) (fromIntegral y)))
  where
    tileAt (C x y) = getMaze s (C x y)
    getMaze = removeBoxes . maz . nth allMazes . stLevel
    
-- tileNoticingBoxes tells what tile is in a given place (and if box is on storage tile or not)
tileNoticingBoxes :: Picture
tileNoticingBoxes fun = \s x y ->
  let tileAt (C x' y') = addBoxes (stBoxes s) (getMaze s) (C x' y')
      getMaze = removeBoxes . maz . nth allMazes . stLevel
      reactionTile =
        if tileAt (C (fromIntegral x) (fromIntegral y)) == Box
          then if fun s x y == goalSquareChar then boxOnGoalSquareChar else drawTile Box
          else fun s x y
  in reactionTile
  
--tileNoticingPlayer tells what tile is in a given place but notices also a player
tileNoticingPlayer :: Picture
tileNoticingPlayer fun = \s x y ->
  let reactionChar =
        if (fun s x y) == goalSquareChar
          then playerOnGoalSquareChar
          else playerChar
  in if (stPlayer s) == (C (fromIntegral x) (fromIntegral y))
      then reactionChar
      else fun s x y


stateScreen :: State -> Screen
-- for this specific mazes [... | y <- [10,9..(-10)], x <- [-10..11]] would be enough, but this is more generic
stateScreen s = [function s x y | y <- [11,10..(-11)], x <- [-37..37]]
  where function s' x' y' = if (x' == 37) then '\n' else tileNoticingPlayer (tileNoticingBoxes tileIgnoringBoxes) s' x' y'
  
coloredScreen :: Screen -> Screen
coloredScreen [] = ""
coloredScreen (x:xs) = (coloredChar x) ++ (coloredScreen xs)


draw :: State -> Screen
draw s
  | isWinning s = winScreen (stMove s)
  | otherwise = "Poziom " ++ (show ((stLevel s) + 1)) ++ "\n" ++ coloredScreen (stateScreen s)
    
resettable :: Activity s -> Activity s
resettable (Activity state0 handle drawArg)
  = Activity state0 handle' drawArg
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s


withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s
    
runActivity :: Activity s -> IO ()
runActivity s' = do
  hSetBuffering stdin NoBuffering
  putStr $ actDraw s' (actState s')
  let go p = do
        putStr "\n"
        input <- getChar
        let q = (actHandle p) (KeyPress [input]) (actState p)
        putStr "\ESCc"
        putStr $ actDraw p q
        go Activity {actState = q, actHandle = actHandle p, actDraw = actDraw p}
  go s'

-- returns True iff all the Boxes are on Storage fields. It uses only reachable boxes, because function initialBoxes finds only them.
isWinning :: State -> Bool
isWinning s = onlyTrue (map (\t -> if t == Storage then True else False) (map (maz (nth (appendList mazes badMazes) (stLevel s))) (stBoxes s)))

-- returns True iff all values in the list are True
onlyTrue :: [Bool] -> Bool
onlyTrue [] = True
onlyTrue l = if (head l) == False then False else onlyTrue (tail l)



withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle drawArg) = Activity state0' handle' drawArg' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    drawArg' (WithUndo s _) = drawArg s
    
    
-- functions for lists

elemList :: Eq a => a -> [a] -> Bool
elemList a [] = False
elemList a (x:xs) = a == x || elemList a xs

appendList :: [a] -> [a] -> [a]
appendList [] l2 = l2
appendList (l1h:l1t) l2 = l1h : appendList l1t l2

listLength :: [a] -> Integer
listLength [] = 0
listLength (h:t) = 1 + listLength t

filterList :: (a -> Bool) -> [a] -> [a]
filterList f [] = []
filterList f (h:t)
  | f h = h : (filterList f t)
  | otherwise = (filterList f t)

nth :: [a] -> Integer -> a
nth (h:t) 0 = h
nth (h:t) n = nth t (n-1)

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (h:t) = (f h) : (mapList f t)

andList :: [Bool] -> Bool
andList [] = True
andList (h:t) = h && andList t

allList :: (a-> Bool) -> [a] -> Bool
allList _ [] = True
allList f (h:t) = f h && allList f t

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ acc [] = acc
foldList f acc (x:xs) = f x (foldList f acc xs)

-- functions for graphs

-- tells if all vertices reachable from a given vertice are ok
isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = allList isOk (findAllReachable initial neighbours)

-- finds all vertices reachable from a given vertice
findAllReachable :: Eq a => a -> (a -> [a]) -> [a]
findAllReachable initial neighbours = appendList [initial] (findAllReachableFromAList [initial] neighbours [initial])    
        
-- finds all unvisited neighbours of a given vertice and returns only them (without initial vertice)
unvisitedNeighbours :: Eq a => a -> (a -> [a]) -> [a] -> [a]
unvisitedNeighbours initial neighbours visited = filterList (\x -> not (elemList x visited)) (neighbours initial)

-- finds all unvisited neighbours of vertices in a given list and returns only them (without initial list)
unvisitedNeighboursOfAList :: Eq a => [a] -> (a -> [a]) -> [a] -> [a]
unvisitedNeighboursOfAList [] _ _ = []
unvisitedNeighboursOfAList (h:t) neighbours visited = 
    let unvisitedH = unvisitedNeighbours h neighbours visited
        updatedVisited = appendList visited unvisitedH
    in appendList unvisitedH (unvisitedNeighboursOfAList t neighbours updatedVisited)
 
-- finds all vertices reachable from a given list of vertices but without this initial list
findAllReachableFromAList :: Eq a => [a] -> (a -> [a]) -> [a] -> [a]
findAllReachableFromAList [] _ _ = []
findAllReachableFromAList vertices neighbours visited =
    let unvisited = unvisitedNeighboursOfAList vertices neighbours visited
        updatedVisited = appendList visited unvisited
    in appendList unvisited (findAllReachableFromAList unvisited neighbours updatedVisited)

-- tells if a given vertice is reachable from initial vertice
reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = elemList v (findAllReachable initial neighbours)

-- tells if all vertices from a given list are reachable from inital
allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = andList (mapList (\x -> reachable x initial neighbours) vs)

neighbourFunction :: (Coord -> Tile) -> Coord -> [Coord]
neighbourFunction m c = [x | d <- [R,U,L,D], x <- [adjacentCoord d c], m x /= Wall]

isClosed :: NewMaze -> Bool
isClosed (NewMaze pos maz) = ((maz pos) == Ground || (maz pos) == Storage) && (isGraphClosed pos (neighbourFunction maz) (\x -> maz x /= Blank))

isSane :: NewMaze -> Bool
isSane (NewMaze pos maz) = (listLength (filterList (\x -> maz x == Storage) (findAllReachable pos (neighbourFunction maz)))) >= (listLength (filterList (\x -> maz x == Box) (findAllReachable pos (neighbourFunction maz))))



boolColor :: Bool -> String
boolColor b
  | b = greenColor
  | otherwise = redColor

boolNumberRepresentation :: Bool -> Integer -> String
boolNumberRepresentation b start = (boolColor b) ++ (show start) ++ " "

stringOfBools :: [Bool] -> Integer -> Screen
stringOfBools [] _ = ""
stringOfBools (x:xs) start = (boolNumberRepresentation x start) ++ (stringOfBools xs (start + 1))

etap4 :: Screen
etap4 = stringOfBools (map (\x -> (isClosed x) && (isSane x)) allMazes) 1

etap5 :: IO()
etap5 = runActivity ((resettable . withStartScreen . withUndo) walkActivity)
