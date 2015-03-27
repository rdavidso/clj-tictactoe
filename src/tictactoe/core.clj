(ns tictactoe.core
  (:gen-class))

(defn make-board
  "Make a new empty board"
  []
  [:empty :empty :empty :empty :empty :empty :empty :empty :empty])

(defn translate
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
    (move board mark (translate x y))))

(def win-patterns
  [[0 1 2]
   [3 4 5]
   [6 7 8]
   [0 3 6]
   [1 4 7]
   [2 5 8]
   [0 4 8]
   [2 4 6]])

(defn check-board
  "Does this board win for this pattern for this player"
  [board pattern player]
  (apply = player (map (fn [index] (get board index)) pattern)))

(defn win?
  "Does a board win for a player?"
  [board player]
  (some #{true} (map (fn [pattern] (check-board board pattern player)) win-patterns)))

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
  [board]
  (let [moves (empty-positions board)
        cnt (count moves)
        position (rand-int cnt)]
    (move board :o (nth moves position))))

(defn pos-max
  ([x] x)
  ([x y] (if (> (get x 0) (get y 0)) x y))
  ([x y & more] (apply pos-max (pos-max x y) more)))

(defn pos-min
  ([x] x)
  ([x y] (if (< (get x 0) (get y 0)) x y))
  ([x y & more] (apply pos-min (pos-min x y) more)))

(defn minimax
  [board player depth]
  (let [children (empty-positions board)
        scr (score board :x depth)
        max-or-min (if (= player :x) pos-max pos-min)
        over? (or (not (= scr 0)) (= 0 (count children)))]
    (cond
      over? [scr -1]
      :else (apply max-or-min (map (fn [index] (assoc (minimax (move board player index) (opponent player) (inc depth)) 1 index)) children)))))

(defn minimax2
  [board player depth]
  (let [next-moves (empty-positions board)
        scr (score board :x depth)]
    (if (or (not (= scr 0)) (empty? next-moves))
      [scr -1]
      (let [next-boards (map (fn [mv] [(move board player mv) mv]) next-moves)]
        (if (= player :x)
          (let [maxes (map (fn [next-board] (assoc (minimax2 (get next-board 0) (opponent player) (inc depth)) 1 (get next-board 1))) next-boards)]
            (apply pos-max maxes))
          (let [mins (map (fn [next-board] (assoc (minimax2 (get next-board 0) (opponent player) (inc depth)) 1 (get next-board 1))) next-boards)]
            (apply pos-min mins)))))))

(defn next-move
  "Use minimax to figure out the next best move and make it"
  [board]
  (let [mv (minimax board :x 0)]
    (move board :x (get mv 1))))

(defn -main
  "This is really slow, and the opponent always goes first"
  [& args]
  (println "Bees")
  (loop [board (make-board)
         turn 0
         win 0
         draw 0]
    (println [turn win draw])
    (let [scr (score board :x turn)]
      (cond
        (> (+ win draw) 100) [win draw]
        (< scr 0) board
        (> scr 0) (recur (make-board) 0 (inc win) draw)
        (= turn 9) (recur (make-board) 0 win (inc draw))
        (even? turn) (recur (random-move board) (inc turn) win draw)
        (odd? turn) (recur (next-move board) (inc turn) win draw)))))
