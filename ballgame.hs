import Graphics.Gloss.Interface.Pure.Game
import System.Random

data Ball = Ball
  { ballPosition :: (Float, Float)
  , ballVelocity :: (Float, Float)
  , ballRadius :: Float
  }

data Block = Block
  { blockPosition :: (Float, Float)
  , blockWidth :: Float
  , blockHeight :: Float
  }

data GameState = GameState
  { ball :: Ball
  , block :: Block
  , blockVelocity :: Float
  , gameOver :: Bool
  }

screenWidth :: Float
screenWidth = 1200

screenHeight :: Float
screenHeight = 800

blockSpeed :: Float
blockSpeed = 200

initialState :: Float -> GameState
initialState gen = GameState
  { ball = Ball
    { ballPosition = (0, -screenHeight / 2 + 50 + 50)
    , ballVelocity = (150, 150)
    , ballRadius = 40
    }
  , block = Block
    { blockPosition = (0, -screenHeight / 2 + 50)
    , blockWidth = 200
    , blockHeight = 20
    }
  , blockVelocity = 0
  , gameOver = False
  }

moveBall :: Float -> Ball -> Ball
moveBall dt ball = ball { ballPosition = newPos }
  where
    (x, y) = ballPosition ball
    (vx, vy) = ballVelocity ball
    newPos = (x + vx * dt, y + vy * dt)

{-moveBlock :: Float -> Block -> Float -> Block
moveBlock dt block velocity = block { blockPosition = (newX, y) }
  where
    (x, y) = blockPosition block
    newX = x + velocity * dt-}

moveBlock :: Float -> Block -> Float -> Block
moveBlock dt block velocity =
  let
    (x, y) = blockPosition block
    newX = x + velocity * dt
    -- Clamp the block's position within the screen limits
    clampedX = max (-screenWidth / 2) (min (screenWidth / 2) newX)
  in
    block { blockPosition = (clampedX, y) }

displayBall :: Ball -> Picture
displayBall ball = translate x y (color red (circleSolid r))
  where
    (x, y) = ballPosition ball
    r = ballRadius ball

displayBlock :: Block -> Picture
displayBlock block = translate x y (color blue (rectangleSolid w h))
  where
    (x, y) = blockPosition block
    w = blockWidth block
    h = blockHeight block

render :: GameState -> Picture
render gameState
  | gameOver gameState =
    pictures
      [ translate (-100) 0 $ scale 0.5 0.5 $ text "Game Over"  -- Reduce the size and align center
      , displayBall (ball gameState)
      , displayBlock (block gameState)
      ]
  | otherwise =
    pictures
      [ displayBall (ball gameState)
      , displayBlock (block gameState)
      ]


update :: Float -> GameState -> GameState
update dt gameState
  | gameOver gameState = gameState -- No further updates if the game is over
  | otherwise =
    let
      updatedBall = moveBall dt (ball gameState)
      updatedBlock = moveBlock dt (block gameState) (blockVelocity gameState)
      bouncedBall = bounceBall updatedBall (block gameState)
      newGameOver = checkGameOver bouncedBall
    in
      gameState { ball = bouncedBall, block = updatedBlock, gameOver = newGameOver }

bounceBall :: Ball -> Block -> Ball
bounceBall ball block =
   let
    (bx, by) = ballPosition ball
    (vx, vy) = ballVelocity ball
    br = ballRadius ball
    (blx, bly) = blockPosition block
    bw = blockWidth block
    bh = blockHeight block

    touchesBlock = by - br <= bly + bh / 2 && bx + br >= blx - bw / 2 && bx - br <= blx + bw / 2
    touchesLeft =  bx - br <= -0.5 * screenWidth
    touchesRight = bx + br >= 0.5 * screenWidth
    touchesTop =  by + br >= 0.5 * screenHeight
    touchesDown =  by - br <= -0.5 * screenHeight

   in
    if touchesBlock
      then ball { ballVelocity = (vx, -vy) }
      else if (touchesLeft || touchesRight)
             then ball { ballVelocity = (-vx, vy) }
             else if (touchesTop || touchesDown)
                    then ball { ballVelocity = (vx, -vy) }
                    else ball

checkGameOver :: Ball -> Bool
checkGameOver ball =
  let
    (_, by) = ballPosition ball
    br = ballRadius ball
  in
    by - br <= -screenHeight / 2  -- Check if the ball reaches a certain height below the block

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) gameState =
  gameState { blockVelocity = -blockSpeed }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) gameState =
  gameState { blockVelocity = blockSpeed }
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) gameState =
  gameState { blockVelocity = 0 }
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) gameState =
  gameState { blockVelocity = 0 }
handleEvent (EventKey (Char 'a') Down _ _) gameState =
  gameState { blockVelocity = -blockSpeed }
handleEvent (EventKey (Char 'd') Down _ _) gameState =
  gameState { blockVelocity = blockSpeed }
handleEvent (EventKey (Char 'a') Up _ _) gameState =
  gameState { blockVelocity = 0 }
handleEvent (EventKey (Char 'd') Up _ _) gameState =
  gameState { blockVelocity = 0 }
handleEvent (EventKey (Char 'r') Down _ _) gameState
  | gameOver gameState = initialState 20
  | otherwise = gameState
handleEvent _ gameState = gameState

main :: IO ()
main = do
  play
    (InWindow "Bouncing Ball" (round screenWidth, round screenHeight) (100, 100))
    white
    160
    (initialState 20)
    render
    handleEvent
    update
