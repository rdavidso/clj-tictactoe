(ns tictactoe.core-test
  (:require [clojure.test :refer :all]
            [tictactoe.core :refer :all]))

(def ^:dynamic *board* nil)

(defn board-fixture [test]
  (binding [*board* (make-board)]
    (test)))

(use-fixtures :each board-fixture)

(deftest make-empty-board
  (testing "Make empty board"
    (is (= [:empty :empty :empty :empty :empty :empty :empty :empty :empty] *board*))))

(deftest translate-coords
  (testing "Translate from x,y to index"
    (are [i x y] (= i (translate x y))
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
         -1 7 7)))

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
    (are [x y] (= x (score y :x))
         10 [:x :o :x :o :o :x :o :x :x]
         -10 [:o :x :x :x :o :o :x :o :o]
         0 [:x :o :x :e :e :e :x :o :x])))
