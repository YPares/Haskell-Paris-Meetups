module HardcoreSolution where

import Control.Monad

totalMoney :: Int
totalMoney = 1505

menu :: [(String, Int)]
menu = [("Mixed Fruit", 215),       ("French Fries", 275),
        ("Side Salad", 335),        ("Hot Wings", 355),
        ("Mozzarella Sticks", 420), ("Sampler Plate", 580)]

solution menu money = 
    head [ y |
            i <- [0..],
            y <- replicateM i menu,
            sum (map snd y) == money]

