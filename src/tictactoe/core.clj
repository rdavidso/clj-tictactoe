(ns tictactoe.core
  (:gen-class)
  (:require [tictactoe.tictactoe :refer [play-game user-input play-n-games]]
            [tictactoe.print :refer [print-score print-summary]]))

(defn -main
  [& args]
  (if (nil? args)
    (print-score (play-game user-input))
    (print-summary (play-n-games (Integer/parseInt (nth args 0))))))
