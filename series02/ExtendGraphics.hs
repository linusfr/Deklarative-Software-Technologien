module Graphic where

import Graphics

-- Graphic -> sum of forms
-- can be an empty Graphic or a Form with another Graphic
data Graphic
  = Nil
  | Graphic Form Graphic

single :: Form -> Graphic
single form = Graphic form Nil

(+++) :: Graphic -> Graphic -> Graphic
(Graphic f1 g1) +++ g2 = Graphic f1 (g1 +++ g2)
Nil +++ g = g
