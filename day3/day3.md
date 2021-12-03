Day 3
================

## Part 1

``` clojure
(ns aoc.2021.day3.part1)
(def input (r/readLines "input.txt"))
(defn to-int-input [x]
  (->> x 
    (map (fn [y] (get (r/strsplit y "" :fixed true) 0)))
    (map r/as.integer)))
(def int-input (to-int-input input))
(defn gen-bit-frequencies [int-input] 
  (->>
    int-input 
    (reduce r/c)
    (r/matrix :nrow (count int-input ) :byrow true)
    (r/colMeans)))
(def bit-frequencies (gen-bit-frequencies int-input))
(defn most-common [bit-freq]
    (map (fn [x] (if (>= x 0.5) 1 0)) bit-freq))
(defn least-common [bit-freq]
    (map (fn [x] (if (<= x 0.5) 1 0)) bit-freq))
(defn pow [exponent num]
  (if (= num 0) 1 (int (reduce * (r/rep.int exponent num)))))
(defn bit-to-int [bit-seq] 
  (let [exps (map (partial pow 2) (r/seq.int 0 (- (count bit-seq) 1)))]
    (reduce + (map * bit-seq (r/rev exps)))))
(* (bit-to-int (most-common bit-frequencies))
    (bit-to-int (least-common bit-frequencies)))
```

    ## 4118544

## Part 2

``` clojure
(ns aoc.2021.day3.part2)
(defn filter-bits [input pos bit]
  (filter 
   (fn [x] (= (get x pos) bit)) input))
(defn find-bit [i e int-input]
  (-> 
    (reduce 
      (fn [acc position]
       (if (= (count acc) 1)
        acc
        (let [frequencies (aoc.2021.day3.part1/gen-bit-frequencies acc)
              current-bit-freq (get frequencies position)
              current-bit (if (>= current-bit-freq 0.5) i e)
              new-input (filter-bits acc position current-bit)]
          new-input)))
      int-input
      (r/seq :from 0 :to (count (get int-input 0))))
     (get 0)
     (aoc.2021.day3.part1/bit-to-int)))

; some sample data for testing
(def sample-data 
  (aoc.2021.day3.part1/to-int-input ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"]))
(aoc.2021.day3.part1/gen-bit-frequencies sample-data)
(find-bit 1 0 sample-data)
(find-bit 0 1 sample-data)

(def oxygen (find-bit 1 0 aoc.2021.day3.part1/int-input))
(def co2 (find-bit 0 1 aoc.2021.day3.part1/int-input))

(* oxygen co2)
```

    ## 3832770
