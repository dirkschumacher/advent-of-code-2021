Day 2
================

## Part 1

``` clojure
(ns aoc.2021.day2.part1)
(def input (r/readLines "input.txt"))
(def instructions 
  (map
    (fn [x] {:action (str (get x 0)) :value (int (get x 1))})
    (r/strsplit input " " :fixed true)))
(def res (reduce
  (fn [acc el]
    (let [action (get el :action)
          value (get el :value)
          d-hor (if (= action "forward") value 0)
          d-depth (if (= action "down") value (if (= action "up") (* -1 value) 0))]
      {:depth (+ (get acc :depth) d-depth) 
        :horizontal (+ (get acc :horizontal) d-hor)})) 
  {:depth 0 :horizontal 0} 
  instructions))
(r/do.call * (vals res))
```

    ## 1840243

## Part 2

``` clojure
(ns aoc.2021.day2.part2)
(def res (reduce
  (fn [acc el]
    (let [action (get el :action)
          value (get el :value)
          current-aim (get acc :aim)
          d-hor (if (= action "forward") value 0)
          d-aim (if (= action "down") value (if (= action "up") (* -1 value) 0))]
      {:depth (+ (get acc :depth) (* current-aim d-hor))
        :horizontal (+ (get acc :horizontal) d-hor)
        :aim (+ current-aim d-aim)})) 
  {:depth 0 :horizontal 0 :aim 0} 
  aoc.2021.day2.part1/instructions))
(* (get res :depth) (get res :horizontal))
```

    ## 1727785422
