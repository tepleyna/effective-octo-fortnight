module Main where
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

fps :: Int
fps = 60

playerSpeed :: Float
playerSpeed = 3

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
  , weapon :: Maybe Weapon}

initialState :: GameState
initialState = State
  { player = Entity {position = (0,0), behaviors = [goNowhere], weapon = Nothing}
  , foes = [ Entity {position = (-20,20), behaviors = [goNowhere], weapon = Nothing} ]
  , pews = []
  , paused = False
  }

goDown :: Behavior
goDown e = e { position = (x, y-playerSpeed) }
 where (x,y) = position e

goUp :: Behavior
goUp e = e { position = (x, y+playerSpeed) }
  where (x,y) = position e

goRight :: Behavior
goRight e = e { position = (x+playerSpeed, y) }
  where (x,y) = position e

goLeft :: Behavior
goLeft e = e { position = (x-playerSpeed, y) }
  where (x,y) = position e

goNowhere :: Behavior
goNowhere e = e

window :: Display
window = InWindow "EffectiveOctoFortnite" (700, 700) (10, 10)

background :: Color
background = light $ light blue

render :: GameState -> Picture
render state =
  pictures pics -- background etc?
  where
    playerPic = uncurry translate (position $ player state) (circleSolid 15)
    enemies = mkEnemies state
    pews = mkPews state
    pics = [playerPic] ++ enemies ++ pews

mkEnemies :: GameState -> [Picture]
mkEnemies state = map 
    (\x ->   uncurry translate (position x) $ color green $ circle 20 )
    $ foes state

mkPews :: GameState -> [Picture]
mkPews state = []


update :: Float -> GameState -> GameState
update ticks state = state { player = updateEntity (player state) }

updateEntity :: Entity -> Entity
updateEntity ent = firstBehavior ent
  where (firstBehavior:_) = behaviors ent

handler :: Event -> GameState -> GameState
-- Move
handler (EventKey (Char 'w') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goUp]
handler (EventKey (Char 'a') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goLeft]
handler (EventKey (Char 's') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goDown]
handler (EventKey (Char 'd') Down _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goRight]
-- Un-Move
handler (EventKey (Char 'w') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goNowhere]
handler (EventKey (Char 'a') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goNowhere]
handler (EventKey (Char 's') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goNowhere]
handler (EventKey (Char 'd') Up _ _) state =
  state { player = (player state){ behaviors = newBehaviors } }
  where newBehaviors = [goNowhere]

handler _ state = state

main :: IO ()
main = play window background fps initialState render handler update
