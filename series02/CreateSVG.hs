import Graphics

-- create Forms
rectangleExample = rectangle 100 200

circleExample = circle 100
    -- create Rectangle

-- create Files
createRect :: IO ()
createRect = writeFile "rect.svg" (toSVG rectangleExample)
    -- create Circle

createCircle :: IO ()
createCircle = writeFile "circle.svg" (toSVG circleExample)
