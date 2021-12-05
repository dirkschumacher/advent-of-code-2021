Day 5
================

## Part 1

``` clojure
(ns day5.part1)
(def input (r/readLines "input.txt"))

; some helper functions first
(defn new-point [x y]
  {:x x :y y})
(defn parse-point [point-str]
  (let [points (first (r/strsplit point-str "," :fixed true))
        points (map int points)]
    (new-point (first points) (last points))))
(parse-point "959,103")

(defn new-line [x y]
  {:from x :to y 
    :is-hv (or (= (get x :x) (get y :x)) (= (get x :y) (get y :y)))})
(defn parse-line [line-str]
  (let [splitted-str (first (r/strsplit line-str "->" :fixed true))]
    (new-line 
      (parse-point (first splitted-str)) 
      (parse-point (last splitted-str)))))
(parse-line "959,103 -> 139,923")

(def lines (map parse-line input))
(def hv-lines (filter (fn [line] (get line :is-hv)) lines))

(defn point-grid [line]
  ; get all points on the line. We know the two points share a point.
  (let [from (get line :from)
        to (get line :to)
        x1 (get from :x)
        x2 (get to :x)
        y1 (get from :y)
        y2 (get to :y)
        x-eq (= x1 x2)
        c1 (if x-eq y1 x1)
        c2 (if x-eq y2 x2)
        pts (r/seq :from c1 :to c2 :by (r/sign (- c2 c1)))]
    {:xs (r/as.integer (if x-eq x1 pts))
     :ys (r/as.integer (if (not x-eq) y1 pts))}))
(point-grid (first hv-lines))

(def grid 
  ; the resulting grid where we keep track of the lines
  (r/matrix 0 :ncol 1000 :nrow 1000))

(defn diff-matrix [grid line]
  ; for a given line, construct a matrix that updates our bookkeeping matrix
  (let [pts (point-grid line)]
    (r/`[<-` grid (r/+ (get pts :ys) 1) (r/+ (get pts :xs) 1) 1)))

(defn find-overlaps [diff-matrix grid lines]
  (->
    (reduce 
      (fn [mat line] (r/+ mat (diff-matrix grid line)))
      grid 
      lines)
    (r/base::`>` 1)
    (r/sum)))
(find-overlaps diff-matrix grid hv-lines)
```

    ## [1] 7318

## Part 2

``` clojure
(ns day5.part2)
; we need a new function that gives us the difference matrix
(defn point-grid [line]
  ; this ever gets called when :is-hv is false
  (let [from (get line :from)
        to (get line :to)
        x1 (get from :x)
        x2 (get to :x)
        y1 (get from :y)
        y2 (get to :y)
        xs (r/seq :from x1 :to x2 :by (r/sign (- x2 x1)))
        ys (r/seq :from y1 :to y2 :by (r/sign (- y2 y1)))]
    {:xs xs :ys ys}))

(defn diff-matrix [grid line]
  (if (get line :is-hv) 
    (day5.part1/diff-matrix grid line)
    (let [pts (point-grid line)
          ys (r/+ (get pts :ys) 1) 
          xs (r/+ (get pts :xs) 1)
          mat (r/`[` grid ys xs :drop false)
          mat (r/`diag<-` mat 1)]
      (r/`[<-` grid ys xs mat))))

(day5.part1/find-overlaps 
  diff-matrix
  day5.part1/grid
  day5.part1/lines)
```

    ## [1] 19939
