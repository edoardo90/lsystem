module Main where
import Graphics.Gloss

data Var = Leaf | Row | Lf | Rt deriving (Eq)
data Turtle = Turtle { position:: (Float, Float), angle:: Float} deriving (Show)
data Config = Config { graphic:: Turtle, history:: [Turtle] } deriving (Show)

class Changer a where
  mutateConfig :: a -> Config -> Config

instance Changer Var where
  mutateConfig var (Config (Turtle (x,y) rotation ) h) = Config (Turtle (mutatePos (x,y)) (mutateAngle rotation)) (mutateHistory h)
    where (mutatePos, mutateAngle, mutateHistory) =  case var of Leaf -> (goOn, id, id)
                                                                 Row  -> (goOn, id, id)
                                                                 Lf   -> (id, turnLeft, push)
                                                                 Rt   -> (restorePos, turnRight, tail)
          goOn (x,y) = (x + branch * cos rotation, y + branch * sin rotation)
          turnLeft angle' = angle' + pi / 4
          push h = (Turtle (x,y) rotation) : h
          turnRight _ = (angle . head) h - pi / 4
          restorePos _ = position (head h)


axiom :: [Var]
axiom = [Leaf]

transformVar :: Var -> [Var]
transformVar Row =  [Row, Row]
transformVar Leaf = [Row, Lf, Leaf, Rt, Leaf]
transformVar Lf =   [Lf]
transformVar Rt =   [Rt]

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


colors = [Color white] --[Color (makeColorI 241 150 128 255), Color (makeColorI 115 116 149 255 ), Color (makeColorI 104 168 173 255 ), Color (makeColorI 196 212 175 255 ),  Color (makeColorI 108 134 114 255 )]--[Color (makeColorI r 150 b 255) | r <- [150,160..250], b <- [50,60..250] ] -- ++
moreColors =  [ Color yellow, Color blue, Color magenta, Color cyan, Color rose,
 Color orange]

colors' = cycle colors
coloredLines n = Pictures $  map (\(color, line) -> color line ) $ zip colors' myLines
  where myLines = map Line $ (noSameDest (nearCouples  (positionsLevel n)))

branch = 20
t0 = Turtle (0,-500) (pi/2)
c0 = Config t0 []

backColor = (makeColorI 212 14 82 255)

main = display (InWindow "Nice Window" (1400, 1400) (20, 20)) backColor (coloredLines 5)

instance Show Var where
  show Leaf = "0"
  show Row  = "1"
  show Lf   = "["
  show Rt   = "]"
