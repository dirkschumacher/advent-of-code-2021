Day 1
================

## Part 1

``` clojure
(ns aoc.2021.day1.part1)
(def data (r/as.integer (r/readLines "input.txt")))
(defn count-increase [input]
  (->>
    (map 
     (fn [x y] (if (> x y) 1 0)) 
     input 
     (r/as.integer (r/dplyr::lag input 1)))
    (rest)
    (reduce + 0)))
(count-increase data)
```

    ## 1448

## Part 2

``` clojure
(ns aoc.2021.day1.part2)
(defn roll_sum [x] (r/RcppRoll::roll_sum x :n 3))
(def data (roll_sum aoc.2021.day1.part1/data))
(aoc.2021.day1.part1/count-increase data)
```

    ## 1471
