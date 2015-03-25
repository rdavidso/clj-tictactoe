(ns tictactoe.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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
   [3 4 6]])

(defn check-board
  "Does this board win for this pattern for this player"
  [board pattern player]
  (apply = player (map (fn [index] (get board index)) pattern)))

(defn win?
  "Does a board win for a player?"
  [board player]
  (some #{true} (map (fn [pattern] (check-board board pattern player)) win-patterns)))

(defn opponent
  [player]
  (if (= player :x) :o :x))

(defn score
  "Score a board for a winning position"
  [board player]
  (let [win-player (win? board player)
        win-opponent (win? board (opponent player))]
    (cond
      win-player 10
      win-opponent -10
      :else 0)))
