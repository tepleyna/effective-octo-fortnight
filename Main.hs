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
  { player = Entity { position = startHeroPos, behaviors = [goNowhere 0], weapon = Nothing, isDead = False, radius = 15}
  , foes = [
    Entity {
      position = (-20,-200)
      , behaviors = [goUp (slower playerSpeed)]
      , weapon = Nothing
      , isDead = False
      , radius = 20
      } ,
    Entity {
      position = (150,-200)
      , behaviors = [goUp (slower playerSpeed)]
      , weapon = Nothing
      , isDead = False
      , radius = 20
      } ,
    Entity {
      position = (150,-300)
      , behaviors = [goUp (slower(slower playerSpeed))]
      , weapon = Nothing
      , isDead = False
      , radius = 20
      } ,
    foeWithAim (250, 380) startHeroPos
       ]
  , pews = []
  , paused = False
  }
  where startHeroPos = (150,1)

foeWithAim :: Position -> Position -> Entity
foeWithAim spawn target = Entity {
    position = spawn
    , behaviors = [targetSpot (slower (slower(playerSpeed))) target]
    , weapon = Nothing
    , isDead = False
    , radius = 10}

targetSpot :: Float -> Position -> Behavior
targetSpot speed targetPos e = e {
  position = ( oldX - (speed * asin( diffX/diffY ))
              , oldY - (speed * acos( diffX/diffY)))
}
  where
    (oldX,oldY) = position e
    (targetX,targetY) = targetPos
    diffX = oldX - targetX + 0.001
    diffY = oldY - targetY + 0.001
    dist = sqrt( diffX*diffX + diffY*diffY ) + 0.001

goDown :: Float -> Behavior
goDown speed e = e { position = (x, y-speed) }
 where (x,y) = position e

goUp :: Float -> Behavior
goUp speed e = e { position = (x, y+speed) }
  where (x,y) = position e

goRight :: Float -> Behavior
goRight speed e = e { position = (x+speed, y) }
  where (x,y) = position e

goLeft :: Float -> Behavior
goLeft speed e = e { position = (x-speed, y) }
  where (x,y) = position e

goNowhere :: Float -> Behavior
goNowhere _ e = e

window :: Display
window = InWindow "EffectiveOctoFortnite" (700, 700) (10, 10)

background :: Color
background = light $ light blue

render :: GameState -> Picture
render state
  | isDead $ player state =
    color (dark red) $ text "You died"
  |otherwise =
    pictures pics
  where
    playerPic = uncurry translate (position $ player state) (circleSolid (radius $ player state))
    enemies = mkEnemies state
    pews = mkPews state
    pics = [playerPic] ++ enemies ++ pews  -- background etc?

mkEnemies :: GameState -> [Picture]
mkEnemies state = map
    (\x ->   uncurry translate (position x) $ color red $ circleSolid $radius x )
    $ foes state

mkPews :: GameState -> [Picture]
mkPews state = []


update :: Float -> GameState -> GameState
update ticks state = cleanDeads $ moveThings $ collideThings state

cleanDeads :: GameState -> GameState
cleanDeads state = state
  { foes = [x | x <- foes state, not (isDead x)]
  , pews = [x | x <- foes state, not (isDead x)]
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
updateEntity ent = firstBehavior ent
  where (firstBehavior:_) = behaviors ent

handler :: Event -> GameState -> GameState
-- Move
handler (EventKey (Char 'w') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goUp playerSpeed]
handler (EventKey (Char 'a') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goLeft playerSpeed]
handler (EventKey (Char 's') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goDown playerSpeed]
handler (EventKey (Char 'd') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goRight playerSpeed]
-- Un-Move
handler (EventKey (Char 'w') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goNowhere 0]
handler (EventKey (Char 'a') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goNowhere 0]
handler (EventKey (Char 's') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goNowhere 0]
handler (EventKey (Char 'd') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goNowhere 0]

handler _ state = state

main :: IO ()
main = play window background fps initialState render handler update
