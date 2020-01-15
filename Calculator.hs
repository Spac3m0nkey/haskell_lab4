-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.
import ThreepennyPages
import Expr 
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Data.Maybe
canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300
main :: IO ()
main = startGUI defaultConfig setup
setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     slider  <- mkSlider (1, 100) 4           -- The scale slider
     diff    <- mkButton "Differentiate"       -- Differentiate button
     lbl     <- mkHTML "<p>Zoom</p>"
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.
     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     ctrl <- row [pure draw, pure diff]
     getBody window 
      #+ [column [pure canvas,pure formula,pure lbl,pure slider,pure ctrl]]
     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]
         -- Interaction (install event handlers)
     on UI.click     draw   $ \ _ -> readAndDraw input slider canvas
     on UI.click     diff   $ \ _ -> do 
        old <- get value input
        element input 
          # set value (maybe old (showExpr.differentiate) (readExpr old))
        readAndDraw input slider canvas

     on valueChange' input  $ \ _ -> readAndDraw input slider canvas
     on valueChange' slider $ \ _ -> readAndDraw input slider canvas
readAndDraw :: Element -> Element -> Canvas -> UI ()
readAndDraw formulaInput scaleInput canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value formulaInput
     -- Get the scale
     scaleString <- get value scaleInput
     let scale = fromInteger (read scaleString) / 100.0
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 25 0)) (pure canvas)
     path "gray" [(canWidth/2, 0),(canWidth/2, canHeight)] canvas
     path "gray" [(0, canHeight/2),(canWidth, canHeight/2)] canvas
     let e = readExpr formula
     let ps = maybe [] (\e' -> points e' scale (canWidth, canHeight)) e
     path "blue" ps canvas 

points :: Expr -> Double -> (Int,Int) -> [Point]
points e s (w, h) = 
  let (w', h') = (fromIntegral w / 2.0,fromIntegral h / 2.0) in
    map (\x ->  (x + w', ((eval e (x * s) / s) * (-1.0)) + h')) [-w'..w']
