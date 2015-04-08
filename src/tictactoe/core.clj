(ns tictactoe.core
  (:gen-class)
  (:require [tictactoe.tictactoe :refer [play-game user-input ai-minimax-move random-move play-n-games]]
            [tictactoe.print :refer [print-score print-summary]]))

(defn parse-n-games-arg
  [num-games-str]
  (try
    (Integer/parseInt num-games-str)
    (catch Exception e "Please enter a positive integer for number of games.")))

(defn get-ai-type
  [string]
  (cond
    (= string "ai") ai-minimax-move
    (= string "random") random-move
    :else random-move))

(defn -main
  [& args]
  (if (nil? args)
    (print-score (play-game user-input))
    (let [num-games (parse-n-games-arg (nth args 0))
          ai-type (get-ai-type (nth args 1 "random"))]
      (if (integer? num-games)
        (print-summary (play-n-games num-games ai-type))
        (println num-games)))))
