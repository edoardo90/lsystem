module Main where
import Graphics.Gloss

data Var = X | F | Plus | Minus | Lf | Rt deriving (Eq)
-- variables : X F
-- constants : + − [ ]


data Turtle = Turtle { position:: (Float, Float), angle:: Float} deriving (Show)

data Config = Config { graphic:: Turtle, history:: [Turtle] } deriving (Show)

class Changer a where
  mutateConfig :: a -> Config -> Config

  newRotation  :: a -> Config -> Float
  newPosition  :: a -> Config -> (Float, Float)
  newHistory   :: a -> Config -> [Turtle]

type Q a = [a]

push :: a -> Q a -> Q a
push  x q  = x : q


pop :: Q a -> (Q a, a)
pop q = (tail q, head q)

precisionC = 4

instance Changer Var where
  mutateConfig var c = Config newTurtle' newHistory' where
    newTurtle' = Turtle (newPosition var c) (newRotation var c)
    newHistory' = newHistory var c

  newPosition var config =
    case var of
      F  ->  goOn config
      Rt -> let oldHistory =  history config
                (_, firstHistory) =  pop oldHistory
            in  position firstHistory
      Lf -> position $ graphic config
      X  -> position $ graphic config
      Plus  -> position $ graphic config
      Minus -> position $ graphic config
      where goOn config = let rotation = angle    $  graphic config
                              (x,y) =      position $  graphic config
                              (x', y') =     (x + branch * cos rotation, y + branch * sin rotation)
                          in  (x', y')
-- Here, F means "draw forward",
-- The square bracket "[" corresponds to saving the current values
-- for position and angle, which are restored
-- when the corresponding "]" is executed.

  newRotation Rt config = let oldHistory =  history config
                              (_, firstHistory) =  pop oldHistory
                          in  angle firstHistory

  newRotation Minus config =
      let rotation = angle $ graphic config
      in  rotation + 25 * (pi / 180)

  newRotation Plus config =
      let rotation = angle $ graphic config
      in  (rotation - 25 * (pi / 180))
--  − means "turn left 25°",
--  + means "turn right 25°".


  newRotation _ config = angle $ graphic  config


  newHistory Rt config =
      let oldHistory =  history config
          (newHistory, _) =  pop oldHistory
      in  newHistory

  newHistory Lf config =
      let rotation = angle $ graphic config
          oldPosition = position $ graphic config
          oldHistory =  history config
          newHistory = push (Turtle oldPosition rotation) oldHistory
      in  newHistory

  newHistory _ config = history config

axiom :: [Var]
axiom = [X]
-- start  : X

transformVar :: Var -> [Var]
transformVar X =  [F, Minus, Lf, Lf, X, Rt, Plus, X, Rt, Plus, F, Lf, Plus, F, X, Rt,Minus,X]
--          (X →   F  −      [    [  X   ]   +    X   ]   +    F   [   +    F  X   ]  −   X
transformVar F = [F, F]
--           (F → FF)
transformVar Lf = [Lf]
transformVar Rt = [Rt]
transformVar Minus = [Minus]
transformVar Plus = [Plus]


transformList :: [Var] -> [Var]
transformList  = concatMap transformVar

algas :: [[Var]]
algas = iterate transformList axiom
algasAtStep n = algas !! n

makeConfigList :: (Changer v) => [Config] ->  v  -> [Config]
makeConfigList configs var =
  let cn = head configs
      c' = mutateConfig var cn
  in  c':configs

configLevel  :: Int -> [Config]
configLevel n = foldl makeConfigList [c0] (algasAtStep n)

positionsLevel  :: Int -> [(Float, Float)]
positionsLevel n = reverse $ map (position . graphic ) (configLevel n)


couples :: [a] -> [[a]]
couples [] = []
couples xs = take 2 xs : couples (drop 2 xs)

nearCouples :: [a] -> [[a]]
nearCouples [] = []
nearCouples [x] =  []
nearCouples [x,y] = [[x,y]]
nearCouples (x:y:z:xs) = [x,y]: [y,z] : nearCouples (z:xs)

type Segment = [(Float, Float)]

noSameDest :: [Segment] -> [Segment]
noSameDest (seg:segs) = seg : noSameDest (filter (noSameDest' seg) segs )
  where noSameDest' seg' = \seg -> last seg /= last seg'
noSameDest [] =  []


colors =  [Color (makeColorI 100 240 100 255)] --[Color green] -- ++
moreColors =  [ Color yellow, Color blue, Color magenta, Color cyan, Color rose,
 Color orange]

colors' = cycle colors
coloredLines n = Pictures $  map (\(color, line) -> color line ) $ zip colors' myLines
  where myLines = map Line $ (noSameDest (nearCouples  (positionsLevel n)))

branch = 6
t0 = Turtle (-400, -800) (25 * pi / 180)
c0 = Config t0 []


main = display (InWindow "Nice Window" (1400, 1400) (20, 20)) black (coloredLines 6)

instance Show Var where
  show F =      "0"
  show X =      "X"
  show Plus  =  "+"
  show Minus  = "-"
  show Lf   =   "["
  show Rt   =   "]"
