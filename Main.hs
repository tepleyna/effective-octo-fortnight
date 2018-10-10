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
  , level :: [[Entity]]
  }
data Entity = Entity
  { position :: Position
  , behaviors :: [Behavior]
  , weapon :: Maybe Weapon
  , isDead :: Bool
  , radius :: Float}


simplePew :: [Behavior] -> Weapon
simplePew behaviours = \ (pos) -> Entity pos behaviours Nothing False 7

initialState :: GameState
initialState = State
  { player = Entity { position = startHeroPos, behaviors = [], weapon = Just $ simplePew [goUp $ faster playerSpeed], isDead = False, radius = 15}
  , foes =
      [ mkFoe (-20, -100) [forSteps 1 $ goUp playerSpeed, circleCW playerSpeed (pi/10) pi] Nothing 70
      , mkFoe (-20, -200) [goUp (slower playerSpeed)] Nothing 20
      , mkFoe (-100,-300) [goUp (slower playerSpeed)] Nothing 20
      , mkFoe (60,  -300) [goUp (slower playerSpeed)] Nothing 20
      , mkFoe (60, 100) [addBehavior $ circleCW playerSpeed (pi / 100) 0] Nothing 10
      , mkFoe (0, 500)
        [ targetSpot (slower (slower(playerSpeed))) target
        , circleCW (slower(playerSpeed)) (pi / 100) 180
        ] Nothing 5
      ]
  , pews = []
  , paused = False
  , level = []
  }
  where
    startHeroPos = (150,1)
    target = (-10,-10)

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
distance (x1,y1) (x2,y2) = sqrt( (x2 - x1)^2 + (y2-y1)^2 )

nextBehavior :: Behavior
nextBehavior entity = entity { behaviors = tail $ behaviors entity}

addBehavior :: Behavior -> Behavior
addBehavior behavior entity = entity { behaviors = [behavior] ++ behaviors entity}

forSteps :: Float -> Behavior -> Behavior
forSteps steps behavior = behavior . nextBehavior . case steps of
  0 -> id
  n -> addBehavior $ forSteps (n-1) behavior

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
      if 4 > distance targetPos (position e)
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
mkPews state = map
  (\x ->   uncurry
    translate (position x)
    $ color blue
    $ rectangleSolid (radius x) (radius x) )
  $ pews state

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
  , foes = foePew
  , pews = pewFoe
  }
  where
    foePlayer = [foe { isDead = intersect (player state) foe } | foe <- foes state]
    playerDie = any isDead foePlayer
    foePew    = [ foe { isDead = (isDead foe) || any (intersect foe) (pews state) } | foe <- foePlayer]
    pewFoe    = [ pew { isDead = any (intersect pew) (foes state) } | pew <- pews state]


intersect :: Entity -> Entity -> Bool
intersect e1 e2 =
  collisionRadius > distance (position e1) (position e2)
  where
    collisionRadius = (radius e1) + (radius e2)

moveThings :: GameState -> GameState
moveThings state = state {
  player = updateEntity $ player state
  , foes = map updateEntity $ foes state
  , pews = map updateEntity $ pews state }

updateEntity :: Entity -> Entity
updateEntity ent =
  if length (behaviors ent) == 0
    then ent
    else (head $ behaviors ent) ent

handler :: Event -> GameState -> GameState
-- New Game // reset
handler (EventKey (Char 'n') Down _ _) state = initialState
-- Shoot
handler (EventKey (Char 'h') _ _ _) state =
  case (weapon $ player state) of
      Nothing   -> state
      Just val  -> state { pews = (pews state) ++
          [val pewSpawn]}
  where
    (x,y) = position $ player state
    pewSpawn = (x, y + (radius $ player state))

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
