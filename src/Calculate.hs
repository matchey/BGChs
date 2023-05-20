
module Calculate (getResults, Score, ScoreList, IncomeList) where

import Data.List (sort, sortOn, sortBy, minimumBy)
-- import Debug.Trace

getResults :: Int -> Int -> [ScoreList] -> [IncomeList]
getResults min_unit rate scores = playerIncomes min_unit team_incomes
  where
    team_incomes = teamIncomes min_unit rate scores

playerIncomes :: Int -> [(Int, ScoreList)] -> [IncomeList]
playerIncomes = map . distribute
  where
    distribute min_unit result = map consistent ranked
      where
        income = div (fst result) min_unit
        scores = snd result
        num = length scores
        div_income = div income num
        mod_income = mod income num
        ranked = zip (rank (map negate (map snd scores))) scores
        rank xs = map snd $ sortOn (fst.fst) $ zip (sortOn snd . zip [0..] $ xs) [0..]
        -- income_check = trace ("div mod: " ++ show((div_income, mod_income))) (1)
        consistent x =
          if fst x < mod_income then
            (fst $ snd x, div_income * min_unit + min_unit)
          else
            (fst $ snd x, div_income * min_unit)

teamIncomes :: Int -> Int -> [ScoreList] -> [(Int, ScoreList)]
teamIncomes min_unit rate scores = zip (consistentIncomes min_unit (getRawIncomes rate sorted)) sorted
  where
    sorted = sortBy (\a b -> (minscore b) `compare` (minscore a)) scores
    minscore = minimum . map snd

consistentIncomes :: Int -> [Double] -> [Int]
consistentIncomes min_unit scores = map consistent ranked
  where
    income = roundIncome min_unit scores
    plist = filter (>0)  income
    nlist = filter (<=0) income
    psum = sum plist
    nsum = sum nlist
    diff = div (psum + nsum) min_unit
    div_diff = div diff (length nlist)
    mod_diff =  mod diff (length nlist)
    ranked = zip (rank scores) income
    rank xs = map snd $ sortOn (fst.fst) $ zip (sortOn snd . zip [0..] $ xs) [0..]
    consistent x =
      if snd x <= 0 then
        if fst x < mod_diff then
          snd x - div_diff * min_unit - min_unit
        else
          snd x - div_diff * min_unit
      else
        snd x

roundIncome :: Int -> [Double] -> [Int]
roundIncome = map . ceil
  where
    ceil min_unit score = ceiling (score / fromIntegral min_unit) * min_unit

getRawIncomes :: Int -> [ScoreList] -> [Double]
getRawIncomes rate team_scores =
  let scale = getScale rate team_scores
      diff  = diffFromAverage $ teamAverages team_scores
  in map (*scale) diff

diffFromAverage :: [Double] -> [Double]
diffFromAverage scores = map (\x -> x - average scores) scores

getScale :: Int -> [ScoreList] -> Double
getScale target team_scores =   fromIntegral (maximum num_list)
                              * fromIntegral (target * sum num_list)
                              / fromIntegral (maximum num_list * length num_list)
                              where
                                num_list = map length team_scores

teamAverages :: [ScoreList] -> [Double]
teamAverages team_scores = map scoreAverage team_scores

scoreAverage :: ScoreList -> Double
scoreAverage = average . map snd

average :: (Real a) => [a] -> Double
average xs = realToFrac (sum xs) / fromIntegral (length xs)

type Score = (String, Int)
type ScoreList = [Score]
type IncomeList = ScoreList

