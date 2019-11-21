import           Style

{-
PART ONE
-}
---------------------------------------------------------------------------------------
-- fillColorStyleLens
---------------------------------------------------------------------------------------
styleTest = Style Red (Stroke Black 1)

testGetColorFromStyle = (getterL strokeColorStyleLens) styleTest

testSetColorFromStyle = (setterL strokeColorStyleLens) styleTest Red

---------------------------------------------------------------------------------------
-- strokeColorStyleLens
---------------------------------------------------------------------------------------
styleStrokeColorTest = Style Red (Stroke Black 1)

testGetStrokeColorFromStyle = strokeWidthStyleLens styleStrokeColorTest

testSetStrokeColorFromStyle = setStyleStrokeColor styleStrokeColorTest Red

---------------------------------------------------------------------------------------
-- strokeWidthStyleLens
---------------------------------------------------------------------------------------
styleStrokeWidthTest = Style Red (Stroke Black 1)

testGetStrokeWidthFromStyle = getStyleStrokeWidth styleStrokeWidthTest

testSetStrokeWidthFromStyle = setStyleStrokeWidth styleStrokeWidthTest 2

{-
PART TWO
-}
---------------------------------------------------------------------------------------
-- fillColorFormLens
---------------------------------------------------------------------------------------
recTest = Rectangle (Point 1 1) (Point 1 1) (Style Black (Stroke Black 1))

testGetColorFromForm = getFormColor recTest

testSetColorOfForm = setFormColor recTest Red

---------------------------------------------------------------------------------------
-- strokeColorFormLens
---------------------------------------------------------------------------------------
testGetFormStrokeColor = getFormStrokeColor recTest

testSetFormStrokeColor = setFormStrokeColor recTest Red

---------------------------------------------------------------------------------------
-- strokeWidthFormLens
---------------------------------------------------------------------------------------
testGetFormStrokeWidth = getFormStrokeWidth recTest

testSetFormStrokeWidth = setFormStrokeWidth recTest 2

---------------------------------------------------------------------------------------
-- Graphic Functions
---------------------------------------------------------------------------------------
graphic =
  [ Rectangle (Point 1 1) (Point 1 1) (Style Black (Stroke Red 1))
  , Rectangle (Point 1 1) (Point 1 1) (Style Red (Stroke Green 2))
  , Circle (Point 1 1) 3 (Style Red (Stroke Green 2))
  ]

fcTest = fc Green graphic

scTest = sc Yellow graphic

swTest = sw 3 graphic

---------------------------------------------------------------------------------------
-- Light
---------------------------------------------------------------------------------------
lightTest :: Graphic
lightTest = light Red === light Yellow === light Green

trafficLightSVG = svgBorder (toSVG formToSVG lightTest)

-- write the traffic light svg to a file
createTrafficLight :: IO ()
createTrafficLight = writeFile "trafficLight.svg" trafficLightSVG
