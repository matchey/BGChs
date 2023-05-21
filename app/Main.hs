module Main where

import qualified Calculate (getResults)

main :: IO ()
main = do
  let score_list = [[("a", 170), ("b", 180)], [("c", 170), ("d", 210), ("e", 180)], [("f", 184)]]
  -- let score_list = [[("a", 170), ("b", 190)], [("c", 180), ("d", 150), ("e", 210)], [("f", 194)]]
  -- let score_list = [[("a", 170), ("b", 190), ("c", 180), ("d", 150), ("e", 210)], [("f", 194)]]
  let rate = 10
  let min_unit = 50 -- must be > 0

  putStrLn "\nrate:"
  print rate
  putStrLn "\ninput:"
  print score_list

  putStrLn "\nresult:"
  let result = Calculate.getResults min_unit rate score_list
  print result
