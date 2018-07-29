module Main where

import Math (pi, sqrt)
import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

-- Exercise 1: `circleArea`

circleArea :: Number -> Number
circleArea r = pi * r * r

main :: Effect Unit
main = do
  log "Hello sailor!"
  logShow (diagonal 3.0 4.0)
  logShow (circleArea 42.0)
