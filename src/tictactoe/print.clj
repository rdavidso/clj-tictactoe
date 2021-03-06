(ns tictactoe.print
  (:require [tictactoe.const :refer [vdiv hdiv]]))

(defn get-str-pos
  "Given a board and a position, return a printable string"
  [board i]
  (let [pos (get board i)]
    (if (= pos :empty) "  " pos)))

(defn print-board
  "Print the current board"
  [can-print board]
  (loop [i 0]
    (let [p1 (get-str-pos board i)
          p2 (get-str-pos board (+ i 1))
          p3 (get-str-pos board (+ i 2))]
      (can-print p1 vdiv p2 vdiv p3)
      (if (< i 5)
        (do
          (can-print hdiv)
          (recur (+ i 3)))))))

(defn print-turn
  "Some helper text to say what turn number and whose it is."
  [can-print start turn]
  (if (or (and (= turn 9) (= start 0)) (and (= turn 10) (= start 1)))
    (can-print "Final board!")
    (let [player (if (even? turn) :o :x)]
      (can-print "Player " player ", Turn " (if (= start 0) (inc turn) turn)))))

(defn print-summary
  [can-print totals]
  (let [[win draw lose] totals]
    (can-print "AI won" win "times, drew the game" draw "times, and lost" lose "times.")))

(defn print-score
  "Print the final text at the end of a game."
  [can-print scr]
  (can-print
    (cond
      (= scr 0) "The game ended in a draw."
      (> scr 0) "Player X wins!"
      (< scr 0) "Player O wins!")))
