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

(defn parse-args
  [args]
  (if (nil? args)
    {}
    (let [num-games (parse-n-games-arg (nth args 0))
          ai-type (get-ai-type (nth args 1 nil))]
      {:num-games num-games :ai-type ai-type})))

(defn setup-game
  [args]
  (if (empty? args)
    {:game-fn play-game :output-fn print-score :input-fn user-input :print-fn println}
    {:game-fn play-n-games :output-fn print-summary :input-fn (:ai-type args) :num-games (:num-games args) :print-fn println}))

(defn -main
  [& args]
  (let [setup (setup-game (parse-args args))]
    (if (string? (:num-games setup))
      (println (:num-games setup))
      ((:game-fn setup) setup))))
