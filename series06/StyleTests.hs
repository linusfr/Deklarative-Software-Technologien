import           Style

---------------------------------------------------------------------------------------
-- fillColorFormLens
---------------------------------------------------------------------------------------
recTest = Rectangle (Point 1 1) (Point 1 1) (Style Black (Stroke Black 1))

testGetColorFromForm = getFormColor recTest

testSetColorOfForm = setFormColor recTest Red

---------------------------------------------------------------------------------------
-- strokeColorStyleLens
---------------------------------------------------------------------------------------
styleStrokeColorTest = Style Red (Stroke Black 1)

testGetStrokeColorFromStyle = getStyleStrokeColor styleStrokeColorTest

testSetStrokeColorFromStyle = setStyleStrokeColor styleStrokeColorTest Red

---------------------------------------------------------------------------------------
-- strokeWidthStyleLens
---------------------------------------------------------------------------------------
styleStrokeWidthTest = Style Red (Stroke Black 1)

testGetStrokeWidthFromStyle = getStyleStrokeWidth styleStrokeWidthTest

testSetStrokeWidthFromStyle = setStyleStrokeWidth styleStrokeWidthTest 2
