(ns tictactoe.tictactoe
  (:require [tictactoe.const :refer [win-patterns]]
            [tictactoe.print :refer :all]))

(defn empty-board
  "Make a new empty board"
  []
  [:empty :empty :empty :empty :empty :empty :empty :empty :empty])

(defn coordinates-to-index
  "Turn x,y into an index"
  [x y]
  (cond
    (and (< x 3) (< y 3)) (+ y (* x 3))
    :else -1))

(defn move
  "Mark a move on the board"
  ([board mark index]
    (let [space (get board index)]
      (cond
        (= space :empty) (assoc board index mark)
        :else board)))
  ([board mark x y]
    (move board mark (coordinates-to-index x y))))

(defn board-pattern-win?
  "Does this board win for this pattern for this player"
  [board pattern player]
  (apply = player (map (fn [index] (get board index)) pattern)))

(defn win?
  "Does a board win for a player?"
  [board player]
  (some #{true} (map (fn [pattern] (board-pattern-win? board pattern player)) win-patterns)))

(defn opponent
  "Return the current player's opponent"
  [player]
  (if (= player :x) :o :x))

(defn score
  "Score a board for a winning position"
  [board player depth]
  (let [win-player (win? board player)
        win-opponent (win? board (opponent player))]
    (cond
      win-player (- 10 depth)
      win-opponent (+ -10 depth)
      :else 0)))

(defn empty-positions
  "Return a list of empty positions"
  [board]
  (keep-indexed #(if (= :empty %2) %1) board))

(defn random-move
  "Makes a random move from available empty board positions"
  ([board]
   (random-move board :o))
  ([board player]
   (let [moves (empty-positions board)
         cnt (count moves)
         position (rand-int cnt)]
     (move board player (nth moves position)))))

(defn pos-max
  ([x] x)
  ([x y] (if (> (get x 0) (get y 0)) x y))
  ([x y & more] (apply pos-max (pos-max x y) more)))

(defn pos-min
  ([x] x)
  ([x y] (if (< (get x 0) (get y 0)) x y))
  ([x y & more] (apply pos-min (pos-min x y) more)))

(defn minimax
  "Not the prettiest"
  [board player depth]
  (let [children (empty-positions board)
        scr (score board :x depth)
        max-or-min (if (= player :x) pos-max pos-min)
        over? (or (not (= scr 0)) (= 0 (count children)))]
    (cond
      over? [scr -1]
      :else (apply max-or-min (map (fn [index] (assoc (minimax (move board player index) (opponent player) (inc depth)) 1 index)) children)))))

(defn next-move
  "Use minimax to figure out the next best move and make it"
  [board]
  (if (= (count (filter #{:empty} board)) 9)
    (random-move board :x)
    (let [mv (minimax board :x 0)]
      (move board :x (get mv 1)))))

(defn play-game
  "Play a game with a given second player function.  Can be random-move or user-input currently."
  [player-func]
  (let [start (rand-int 2)
        turn-max (+ start 9)]
    (loop [board (empty-board)
           turn start]
      (print-turn start turn)
      (print-board board)
      (let [scr (score board :x turn)]
        (cond
          (not (= scr 0)) scr
          (= turn turn-max) scr
          (even? turn) (recur (player-func board) (inc turn))
          (odd? turn) (recur (next-move board) (inc turn)))))))

(defn update-totals
  [scr win draw]
  (cond
    (= scr 0) [win (inc draw) 0]
    (> scr 0) [(inc win) draw 0]
    (< scr 0) [win draw 1]))

(defn play-n-games
  "Play through n games and return an array of [win draw] counts"
  [num-games]
  (loop [win 0
         draw 0]
    (if (>= (+ win draw) num-games) 
      [win draw]
      (let [scr (play-game random-move)
            [new-win new-draw lose] (update-totals scr win draw)]
        (print-score scr)
        (if (= lose 0)
          (recur new-win new-draw)
          scr)))))

(defn safe-parse-int
  [string]
  (try
    (Integer/parseInt string)
    (catch Exception e -1)))

(defn user-input
  "Prompt player to put in a row,col coordinate for their move.  This doesn't check if it's an illegal move."
  [board]
  (println "Please enter your move in the form of row,col with 0,0 as upper left: ")
  (let [input (read-line)
        sp (.split input ",")
        x (safe-parse-int (get sp 0))
        y (safe-parse-int (get sp 1))
        new-board (move board :o (coordinates-to-index x y))]
    (if (= new-board board)
      (do
          (println "Invalid input, try again")
          (user-input board))
      new-board)))
