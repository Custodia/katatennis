data Points
  = Love
  | Fifteen
  | Thirty
  deriving (Show, Eq)


data Player
  = Player1
  | Player2
  deriving (Show, Eq)


data Game
  = Ongoing Points Points
  | Deuce
  | Advantage Player
  | Game Player
  deriving (Show)


gameStart :: Game
gameStart = Ongoing Love Love


addPoint :: Points -> Points
addPoint Love = Fifteen
addPoint Fifteen = Thirty
addPoint Thirty = error "Fourty points is always represented as advantage or game."


atThirty :: Player -> Points -> Game
atThirty first points
  | points == Thirty = Advantage first
  | otherwise = Game first


score :: Game -> Player -> Game
score (Ongoing s1 s2) p
  | s1 == Thirty && p == Player1 = atThirty p s2
  | s2 == Thirty && p == Player2 = atThirty p s1
  | p == Player1 = Ongoing (addPoint s1) s2
  | p == Player2 = Ongoing s1 (addPoint s2)
score Deuce p = Advantage p
score (Advantage a) p
  | p == a = Game p
  | otherwise = Deuce
score (Game p) _ = Game p


mapScores :: Game -> [Player] -> Game
mapScores = foldl score
