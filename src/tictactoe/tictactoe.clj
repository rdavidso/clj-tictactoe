(ns tictactoe.tictactoe
  (:require [tictactoe.const :refer [win-patterns]]
            [tictactoe.print :refer :all]))

(defn empty-board
  "Make a new empty board"
  []
  [:empty :empty :empty :empty :empty :empty :empty :empty :empty])

(defn empty-positions
  "Return a list of empty positions"
  [board]
  (keep-indexed #(if (= :empty %2) %1) board))

(defn coordinates-to-index
  "Turn row,col into an inderow"
  [row col]
  (cond
    (and (< row 3) (< col 3) (>= row 0) (>= col 0)) (+ col (* row 3))
    :else -1))

(defn move
  "Mark a move on the board"
  ([board mark index]
    (let [space (get board index)]
      (cond
        (= space :empty) (assoc board index mark)
        :else board)))
  ([board mark row col]
    (move board mark (coordinates-to-index row col))))

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

(defn pos-max
  ([x] x)
  ([x y] (if (> (get x 0) (get y 0)) x y))
  ([x y & more] (apply pos-max (pos-max x y) more)))

(defn pos-min
  ([x] x)
  ([x y] (if (< (get x 0) (get y 0)) x y))
  ([x y & more] (apply pos-min (pos-min x y) more)))

(defn minimax
  "Find the best move given the current board"
  [board player depth scoring-player]
  (let [children (empty-positions board)
        scr (score board scoring-player depth)
        max-or-min (if (= player scoring-player) pos-max pos-min)
        over? (or (not (= scr 0)) (= 0 (count children)))]
    (cond
      over? [scr -1]
      :else (apply max-or-min (map (fn [index] (assoc (minimax (move board player index) (opponent player) (inc depth) scoring-player) 1 index)) children)))))

(defn random-move
  "Makes a random move from available empty board positions"
  ([board]
   (random-move board :o))
  ([board player]
   (let [moves (empty-positions board)
         cnt (count moves)
         position (rand-int cnt)]
     (move board player (nth moves position)))))

(defn ai-minimax-move
  "Use minimax to figure out the next best move and make it"
  ([board]
   (ai-minimax-move board :o))
  ([board player]
   (if (= (count (filter #{:empty} board)) 9)
     (random-move board player)
     (let [mv (minimax board player 0 player)]
       (move board player (get mv 1))))))

(defn safe-parse-int
  [string]
  (try
    (Integer/parseInt string)
    (catch Exception e -1)))

(defn user-input
  "Prompt player to put in a row,col coordinate for their move."
  [board]
  (println "Please enter your move in the form of row,col with 0,0 as upper left: ")
  (let [input (read-line)
        sp (.split input ",")
        row (safe-parse-int (get sp 0))
        col (safe-parse-int (get sp 1))
        new-board (move board :o row col)]
    (if (= new-board board)
      (do
          (println "Invalid input, try again")
          (user-input board))
      new-board)))

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
          (odd? turn) (recur (ai-minimax-move board :x) (inc turn)))))))

(defn update-totals
  [scr win draw lose]
  (cond
    (= scr 0) [win (inc draw) lose]
    (> scr 0) [(inc win) draw lose]
    (< scr 0) [win draw (inc lose)]))

(defn play-n-games
  "Play through n games and return an array of [win draw] counts"
  [num-games ai-type]
  (loop [win 0
         draw 0
         lose 0]
    (if (>= (+ win draw lose) num-games) 
      [win draw lose]
      (let [scr (play-game ai-type)
            [new-win new-draw new-lose] (update-totals scr win draw lose)]
        (print-score scr)
        (recur new-win new-draw new-lose)))))
