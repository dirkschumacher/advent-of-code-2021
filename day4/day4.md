Day 4
================

## Part 1

``` clojure
(ns day4.part1)
(def input (r/readLines "input.txt"))
(def bingo-seq 
  (-> 
    (first input)
    (r/strsplit ",")
    (first)
    (r/as.integer)))
(defn parse-board [input-vec]
  ; parse a board in raw encoding to an R matrix
  (->>
    (r/strsplit input-vec "\s+")
    (map r/as.integer)
    (reduce r/c)
    (r/na.omit)
    (r/matrix :byrow true :ncol 5 :nrow 5)))
(defn make-game [board-matrix]
  {:board-matrix board-matrix :draws []})
(def all-boards 
  (loop [offset 2
         boards []]
    (if (< (- (count input) offset) 5)
      boards
      (let [n (fn [x] (get input (+ offset x)))
            board (parse-board (r/c (n 0) (n 1) (n 2) (n 3) (n 4)))] 
        (recur (+ offset 6) (conj boards board))))))
(def games (map make-game all-boards))
(defn play-round [games draw]
  (map 
    (fn [game]
      (let [mat (get game :board-matrix)
            is-draw (r/== mat draw)
            new-mat (r/`[<-` mat is-draw r/NA_real_)]
        {:board-matrix new-mat :draws (conj (get game :draws) draw)})) 
    games))

(defn has-won [game]
  (let [mat (get game :board-matrix)
        bingo-mat (r/is.na mat)
        won (or 
             (r/any (r/== (r/rowSums bingo-mat) 5)) 
             (r/any (r/== (r/colSums bingo-mat) 5)))
        last-draw (last (get game :draws))
        score (int (* (r/sum mat :na.rm true) last-draw))]
    {:won won :score score}))

(defn play-game [games bingo-seq]
  (loop [pos 0
         games games]
    (if (= pos (count bingo-seq)) 
      []
      (let [draw (get bingo-seq pos)
            new-games (play-round games draw)
            won-games (filter (fn [game] (get (has-won game) :won)) new-games)]
        (if (> (count won-games) 0)
          won-games
          (recur (inc pos) new-games))))))
(def winners (play-game games bingo-seq))
(has-won (first winners)) ; we know there is a winner
```

    ## {:won true :score 33462}

## Part 2

``` clojure
(ns day4.part2)
(defn play-game [games bingo-seq]
  (loop [pos 0
         games games
         last-winners []]
    (if (= pos (count bingo-seq)) 
      last-winners
      (let [draw (get bingo-seq pos)
            new-games (day4.part1/play-round games draw)
            won-games (filter (fn [game] (get (day4.part1/has-won game) :won)) new-games)
            rest (filter (fn [game] (not (get (day4.part1/has-won game) :won))) new-games)]
        (recur (inc pos) rest (if (> (count won-games) 0) won-games last-winners))))))
(def winners (play-game day4.part1/games day4.part1/bingo-seq))
(day4.part1/has-won (first winners)) 
```

    ## {:won true :score 30070}
