module Graphics (Drawing) where

import qualified Graphics.HGL as HGL
import Control.Monad.Free

data Drawing =
  Graphic HGL.Graphic |
  WithColor HGL.RGB Drawing |
  Above Drawing Drawing

(&) = Above

compTest =
  Graphic undefined &
  (WithColor undefined $
    Graphic undefined &
    Graphic undefined) &
  Graphic undefined

draw :: Drawing -> IO HGL.Graphic
draw (Graphic grph) = return grph
draw (WithColor col drawing) = do
  brush <- HGL.createBrush col
  drawn <- draw drawing
  return $ do
    prev <- HGL.selectBrush brush
    drawn
    _ <- HGL.selectBrush prev
    return ()
