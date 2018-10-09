module Main where
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

fps :: Int
fps = 60

playerSpeed :: Float
playerSpeed = 2.5

slower :: Float -> Float
slower speed = 0.65 * speed

faster :: Float -> Float
faster speed = 1.3 * speed

type Position = (Float, Float)
type Behavior = Entity -> Entity
type Weapon = Position -> Entity
data GameState = State
  { player :: Entity
  , foes :: [Entity]
  , pews :: [Entity]
  , paused :: Bool
  }
data Entity = Entity
  { position :: Position
  , behaviors :: [Behavior]
  , weapon :: Maybe Weapon
  , isDead :: Bool
  , radius :: Float}

initialState :: GameState
initialState = State
  { player = Entity { position = startHeroPos, behaviors = [], weapon = Nothing, isDead = False, radius = 15}
  , foes = [
    mkFoe (-20,-700) [goUp playerSpeed] Nothing 70 ,
    mkFoe (-20,-200) [goUp (slower playerSpeed)] Nothing 20 ,
    mkFoe (-100,-300) [goUp (slower playerSpeed)] Nothing 20 ,
    mkFoe (60,-300) [goUp (slower playerSpeed)] Nothing 20 ,

    mkFoe (60, 100) [circleCW playerSpeed (pi / 100) 0] Nothing 10,

    mkFoe (0, 500) 
      [targetSpot (slower (slower(playerSpeed))) target
      , circleCW playerSpeed (pi / 100) 0]
      Nothing 5
    ]
  , pews = []
  , paused = False
  }
    where
      startHeroPos = (150,1)
      target = (-10,-10)
      target2= (-35, 20)

mkFoe :: Position -> [Behavior] -> Maybe Weapon -> Float -> Entity
mkFoe pos behs pew rad =
  Entity {
    position = pos
    , behaviors = behs
    , weapon = pew
    , isDead = False
    , radius = rad
    }

distance :: Position -> Position -> Float
distance (x1,y1) (x2,y2) = sqrt( (x2 - x1)^2 + (y2-y1) )

targetSpot :: Float -> Position -> Behavior
targetSpot speed targetPos e =
  ( goDirection angle speed e )
  { behaviors = outBehaviors }
  where
    (x,y) = position e
    (x',y') = targetPos
    dx = x' - x
    dy = y' - y
    angle = atan2 dy dx
    outBehaviors =
      if 7 > distance targetPos (position e)
        then tail $ behaviors e
        else behaviors e

circleCW :: Float -> Float -> Float -> Behavior
circleCW speed turnrate initAngle entity =
  (goDirection initAngle speed entity)
  { behaviors = [circleCW speed turnrate (initAngle - turnrate)]}

goDirection :: Float -> Float -> Behavior
goDirection angle speed entity =
  entity { position = (x+dx, y+dy) }
  where
    (x,y) = position entity
    dx = (cos angle) * speed
    dy = (sin angle) * speed

goDown :: Float -> Behavior
goDown = goDirection (3 * pi / 2)

goUp :: Float -> Behavior
goUp = goDirection (pi / 2)

goRight :: Float -> Behavior
goRight = goDirection 0

goLeft :: Float -> Behavior
goLeft = goDirection pi

window :: Display
window = InWindow "EffectiveOctoFortnite" (700, 700) (10, 10)

background :: Color
background = light $ light blue

render :: GameState -> Picture
render state
  | isDead $ player state =
    uncurry translate (-300, 0) $ color (dark red) $ text "You died"
  |otherwise =
    pictures pics
  where
    playerPic = uncurry translate (position $ player state) (circleSolid (radius $ player state))
    enemies = mkEnemies state
    pews = mkPews state
    pics = [playerPic] ++ enemies ++ pews  -- background etc?

mkEnemies :: GameState -> [Picture]
mkEnemies state = map
    (\x ->   uncurry translate (position x) $ color red $ circleSolid $ radius x )
    $ foes state

mkPews :: GameState -> [Picture]
mkPews state = []

update :: Float -> GameState -> GameState
update ticks state = cleanDeads $ moveThings $ collideThings state

cleanDeads :: GameState -> GameState
cleanDeads state = state
  { foes = [x | x <- foes state, not (isDead x)]
  , pews = [x | x <- pews state, not (isDead x)]
  }

collideThings :: GameState -> GameState
collideThings state = state
  { player = (player state) { isDead = playerDie || isDead (player state)}
  , foes = foePlayer
  }
  where
    foePlayer = [foe { isDead = intersect (player state) foe } | foe <- foes state]
    playerDie = any isDead foePlayer

intersect :: Entity -> Entity -> Bool
intersect e1 e2 =
  collisionRadius > ( sqrt ((x1 - x2)^2 + (y1 - y2)^2 ))
  where
    (x1, y1) = position e1
    (x2, y2) = position e2
    collisionRadius = (radius e1) + (radius e2)

moveThings :: GameState -> GameState
moveThings state = state {
  player = updateEntity $ player state
  , foes = map updateEntity $ foes state }

updateEntity :: Entity -> Entity
updateEntity ent =
  if length (behaviors ent) == 0
    then ent
    else (head $ behaviors ent) ent
  --foldl (flip id) ent (behaviors ent)

handler :: Event -> GameState -> GameState
handler (EventKey (Char 'n') Down _ _) state =
  initialState
-- Move
handler (EventKey (Char 'w') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = behaviors (player state) ++ [goUp playerSpeed]
handler (EventKey (Char 'a') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = behaviors (player state) ++ [goLeft playerSpeed]
handler (EventKey (Char 's') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = behaviors (player state) ++ [goDown playerSpeed]
handler (EventKey (Char 'd') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = behaviors (player state) ++ [goRight playerSpeed]
-- Un-Move
handler (EventKey (Char 'w') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = []
handler (EventKey (Char 'a') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = []
handler (EventKey (Char 's') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = []
handler (EventKey (Char 'd') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = []
handler _ state = state

main :: IO ()
main = play window background fps initialState render handler update
