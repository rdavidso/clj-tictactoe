(ns tictactoe.core-test
  (:require [clojure.test :refer :all]
            [tictactoe.tictactoe :refer :all]
            [tictactoe.print :refer [*can-print*]]))

(def ^:dynamic *board* nil)

(defn board-fixture [test]
  (binding [*board* (empty-board)
            *can-print* false]
    (test)))

(use-fixtures :each board-fixture)

(deftest make-empty-board
  (testing "Make empty board"
    (is (= [:empty :empty :empty :empty :empty :empty :empty :empty :empty] *board*))))

(deftest translate-coords
  (testing "Translate from x,y to index"
    (are [i x y] (= i (coordinates-to-index x y))
         0 0 0
         1 0 1
         2 0 2
         3 1 0
         4 1 1
         5 1 2
         6 2 0
         7 2 1
         8 2 2
         -1 1 5
         -1 5 1
         -1 7 7
         -1 1 -1
         -1 -2 -2
         -1 -1 0)))

(deftest make-move
  (testing "Make a valid move"
    (is (= [:empty :empty :empty :empty :x :empty :empty :empty :empty] (move *board* :x 1 1)))))

(deftest make-invalid-move
  (testing "Make an invalid move"
    (is (= [:empty :empty :empty :empty :empty :empty :empty :empty :empty] (move *board* :x 1 5)))))

(deftest make-invalid-move-not-empty
  (testing "Make an invalid move"
    (binding [*board* (move *board* :x 1 1)]
      (is (= [:empty :empty :empty :empty :x :empty :empty :empty :empty] (move *board* :o 1 1))))))

(deftest make-multiple-valid-moves
  (testing "Make multiple valid moves"
    (binding [*board* (-> *board*
                          (move :x 1 1)
                          (move :o 0 0)
                          (move :x 0 2)
                          (move :o 2 0))]
      (is (= [:o :empty :x :empty :x :empty :o :empty :empty] *board*)))))

(deftest did-win
  (testing "Do these boards win?"
    (are [x y] (= x (win? y :x))
         true [:x :x :x :empty :empty :empty :empty :empty :empty]
         nil [:x :e :x :e :e :e :e :e :e]
         true [:o :x :o :o :x :x :x :x :o])))

(deftest score-board
  (testing "Score these boards"
    (are [x y z] (= x (score y :x z))
         10 [:x :o :x :o :o :x :o :x :x] 0
         -10 [:o :x :x :x :o :o :x :o :o] 0
         0 [:x :o :x :e :e :e :x :o :x] 0)))

(deftest get-empty-positions
  (testing "Get empty board positions"
    (are [x y] (= x (empty-positions y))
         [3 4 5] [:x :o :x :empty :empty :empty :x :o :x]
         [0 1 2 3 4 5 6 7 8] *board*
         [] [:o :x :o :o :x :x :x :x :o])))

(deftest get-ai-minimax-move
  (testing "Get next move from AI"
    (are [x y] (= x (minimax y :x 0 :x))
         [9 6] [:o :o :x :empty :x :empty :empty :empty :empty]
         [0 3] [:o :o :x :empty :x :empty :o :empty :empty])))

(deftest make-ai-minimax-move
  (testing "Make the move it should"
    (are [x y] (= x (ai-minimax-move y :x))
         [:o :o :x :empty :x :empty :x :empty :empty] [:o :o :x :empty :x :empty :empty :empty :empty]
         [:o :o :x :x :x :empty :o :empty :empty] [:o :o :x :empty :x :empty :o :empty :empty])))

(deftest play-a-game
  (testing "Play a random game vs AI"
    (is (>= (play-game random-move) 0))))

(deftest play-ai-vs-itself
  (testing "Play a game of AI vs AI"
    (is (= (play-game ai-minimax-move) 0))))
