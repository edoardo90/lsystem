module Main where
import Graphics.Gloss

data Var = X | F | Plus | Minus | Lf | Rt deriving (Eq)
data Turtle = Turtle { position:: (Float, Float), angle:: Float} deriving (Show)
data Config = Config { graphic:: Turtle, history:: [Turtle] } deriving (Show)

class Changer a where
  mutateConfig :: a -> Config -> Config

instance Changer Var where
  mutateConfig var (Config (Turtle (x,y) rotation ) h) = Config (Turtle (mutatePos (x,y)) (mutateAngle rotation)) (mutateHistory h)
    where (mutatePos, mutateAngle, mutateHistory) =  case var of F ->     (goOn, id, id)
                                                                 Plus  -> (id, turnRight, id)
                                                                 Minus -> (id, turnLeft, id)
                                                                 Lf   ->  (id, id, push)
                                                                 Rt   ->  (restorePos, restoreAngle, tail)
                                                                 X    ->  (id, id, id)
          goOn (x,y) =       (x + branch * cos rotation, y + branch * sin rotation)
          turnRight angle' = angle' - 25 * (pi / 180)
          turnLeft angle' =  angle' + 25 * (pi / 180)
          push h =           Turtle (x,y) rotation : h
          restorePos _ =     position (head h)
          restoreAngle _ =   angle (head h)

axiom :: [Var]
axiom = [X]

transformVar :: Var -> [Var]
transformVar X =  [F, Minus, Lf, Lf, X, Rt, Plus, X, Rt, Plus, F, Lf, Plus, F, X, Rt,Minus,X]
transformVar F = [F, F]
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
makeConfigList configs var = mutateConfig var (head configs) :configs


configLevel  :: Int -> [Config]
configLevel n = foldl makeConfigList [c0] (algasAtStep n)

positionsLevel  :: Int -> [(Float, Float)]
positionsLevel n = reverse $ map (position . graphic ) (configLevel n)

nearCouples :: [a] -> [[a]]
nearCouples [] = []
nearCouples [x] =  []
nearCouples [x,y] = [[x,y]]
nearCouples (x:y:z:xs) = [x,y]: [y,z] : nearCouples (z:xs)

type Segment = [(Float, Float)]
noSameDest :: [Segment] -> [Segment]
noSameDest (seg:segs) = seg : noSameDest (filter (noSameDest' seg) segs)
  where noSameDest' seg' seg = last seg /= last seg'
noSameDest [] =  []

coloredLines n = Pictures $  map (\(color, line) -> color line ) $ zip colors myLines
  where myLines = map Line $ noSameDest (nearCouples  (positionsLevel n))

c0 = Config (Turtle (0,-500) (pi/4)) []
branch = 5
colors = Color (makeColorI 158 191 109 255) : colors
backColor = makeColorI 10 50 10 255

main = display (InWindow "Nice Window" (1400, 1400) (20, 20)) backColor (coloredLines 6)
