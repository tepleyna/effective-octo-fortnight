module Main where
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

fps :: Int
fps = 60

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
  { player = Entity {position = (0,0), behaviors = [goRight], weapon = Nothing}
  , foes = []
  , pews = []
  , paused = False
  }

goRight :: Behavior
goRight e = e { position = (x+5, y) }
 where (x,y) = position e

window :: Display
window = InWindow "EffectiveOctoFortnite" (200, 200) (10, 10)

background :: Color
background = light $ light blue

drawing :: GameState -> Picture
drawing state = circle 80

update :: Float -> GameState -> GameState
update ticks state = state

handler :: Event -> GameState -> GameState
handler _ state = state

main :: IO ()
main = play window background fps initialState drawing handler update
