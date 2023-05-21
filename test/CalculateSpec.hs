module Main where

import qualified Data.Set as Set
import Test.Hspec

import qualified Calculate (getResults)

getResults :: Input -> Set.Set (Set.Set (String, Int))
getResults input =
  setFromScoresList $ Calculate.getResults (min_unit input) (rate input) (scores_list input)

setFromScoresList :: [[(String, Int)]] -> Set.Set (Set.Set (String, Int))
setFromScoresList xs = Set.fromList $ map Set.fromList xs

data Input = Input{
  min_unit :: Int,
  rate :: Int,
  scores_list :: [[(String, Int)]]
}

main :: IO ()
main = hspec $ do
  let data_list = [(Input 10 10 [[("a", 170)]],
                    setFromScoresList [[("a",   0)]]),

                   (Input 10 10 [[("a",  170)], [("b", 201)]],
                    setFromScoresList [[("a", -310)], [("b", 310)]]),

                   (Input 10  0 [[("a",  170)], [("b", 200)]],
                    setFromScoresList [[("a",    0)], [("b",   0)]]),

                   (Input 10 10 [[("a",  170)], [("b", 200)], [("c", 190)]],
                    setFromScoresList [[("a", -340)], [("b", 270)], [("c", 70)]]),

                   (Input  1 10 [[("a",  170)], [("b", 201)]],
                    setFromScoresList [[("a", -310)], [("b", 310)]]),

                   (Input  10 10 [[("a", 170), ("b", 190)], [("c", 180), ("d", 200)]],
                    setFromScoresList [[("a",-100),("b",-100)],[("c",100),("d",100)]]),

                   (Input  10 10 [[("a", 170), ("b", 190)], [("c", 180), ("d", 201)]],
                    setFromScoresList [[("a",-110),("b",-100)],[("c",100),("d",110)]]),

                   (Input  10 10 [[("a", 190), ("b", 170)], [("c", 180), ("d", 201)]],
                    setFromScoresList [[("a",-100),("b",-110)],[("c",100),("d",110)]]),

                   (Input  50 10 [[("a", 170), ("b", 190)], [("c", 180), ("d", 200)]],
                    setFromScoresList [[("a",-100),("b",-100)],[("c",100),("d",100)]]),

                   (Input  10 10 [[("a", 170), ("b", 180)], [("c", 170), ("d", 210), ("e", 180)], [("f", 184)]],
                    setFromScoresList [[("f",90)],[("a",-150),("b",-140)],[("c",60),("d",70),("e",70)]]),

                   (Input  50 10 [[("a", 170), ("b", 180)], [("c", 170), ("d", 210), ("e", 180)], [("f", 184)]],
                    setFromScoresList [[("f",100)],[("a",-150),("b",-150)],[("c",50),("d",100),("e",50)]]),

                   (Input  1  1 [[("a",  170)], [("b", 201)]],
                    setFromScoresList [[("a", -31)], [("b", 31)]])]

  mapM_ check data_list

  where
    check x =
      describe "calculate income from scores." $ do
          it (showInput $ fst x) $ do
                getResults (fst x) `shouldBe` (snd x)
    showInput value = "min_unit: " ++ show (min_unit value) ++ ", rate: " ++ show (rate value) ++ ", scores_list: " ++ show (scores_list value)

