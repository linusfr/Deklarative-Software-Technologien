import Graphics

example = circle 10

main :: IO()
main = writeFile "graphic.svg" (toSVG example)