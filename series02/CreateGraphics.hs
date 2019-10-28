import           ExtendGraphics

-- Background for traffic light
testForm1 :: Form
testForm1 = Rectangle (Point 100.0 100.0) (Point 150.0 350.0) (Style Black)

-- states of the traffic light
testForm2 :: Form
testForm2 = Circle (Point 100.0 600.0) 50 (Style Red)

testForm3 :: Form
testForm3 = Circle (Point 100.0 600.0) 50 (Style Yellow)

testForm4 :: Form
testForm4 = Circle (Point 100.0 600.0) 50 (Style Green)

-- create graphics for the forms
backgroundBox :: Graphic
backgroundBox = single testForm1

redCircle :: Graphic
redCircle = single testForm2

yellowCircle :: Graphic
yellowCircle = single testForm3

greenCircle :: Graphic
greenCircle = single testForm4

-- align graphics to form a traffic light
trafficLightGraphic :: Graphic
trafficLightGraphic =
  backgroundBox `atop` ((redCircle === yellowCircle === greenCircle))

-- wrap the traffic light in svg tags
trafficLightSVG :: String
trafficLightSVG = svgBorder (toSVG trafficLightGraphic)

-- write the traffic light svg to a file
createTrafficLight :: IO ()
createTrafficLight = writeFile "trafficLight.svg" trafficLightSVG
