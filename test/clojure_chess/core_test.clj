(ns clojure-chess.core-test
  (:require [clojure.test :refer :all]
            [clojure-chess.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

;; https://www.google.com/search?q=chess+middle+game&client=firefox-b-d&sxsrf=AOaemvITegLHw_P6-b6fnDybEiIOjPZ0-g:1638349557811&source=lnms&tbm=isch&sa=X&ved=2ahUKEwjYuqTbn8L0AhXmhP0HHYmrD2IQ_AUoAXoECAEQAw&biw=1440&bih=815&dpr=2#imgrc=7ma3DpkiRQKESM
(def testing-board [[:r :e :e :e :r :k :e :e]
                    [:e :e :p :e :q :p :p :p]
                    [:p :e :n :b :p :n :e :e]
                    [:e :p :e :p :e :b :e :e]
                    [:e :e :e :P :e :e :e :e]
                    [:e :B :N :e :P :P :B :e]
                    [:P :P :P :e :N :e :P :P]
                    [:e :K :R :e :Q :e :e :R]])

(deftest square-empty?-test
  (testing
   (is (= (square-empty? testing-board [0 0]) false))))

(deftest get-pseudolegal-destinations-white-knight-test
  (testing
      (is (= #{[4 5] [7 6]}
             (get-pseudolegal-destinations testing-board [6 4])))))

(deftest get-pseudolegal-destinations-white-bishop-test
  (testing
   (is (= #{[4 5] [3 4] [4 7] [6 5] [2 3]}
          (get-pseudolegal-destinations testing-board [5 6])))))

(deftest get-pseudolegal-destinations-white-queen-test
  (testing
   (is (= #{[7 3] [7 5] [7 6] [6 3] [6 5]}
          (get-pseudolegal-destinations testing-board [7 4])))))

(deftest get-pseudolegal-destinations-black-king-test
  (testing
   (is (= #{[7 3] [7 5] [7 6] [6 3] [6 5]}
          (get-pseudolegal-destinations testing-board [7 4])))))

(deftest get-pseudolegal-destinations-black-rook-test
  (testing
   (is (= #{[0 6]}
          (get-pseudolegal-destinations testing-board [0 5])))))

(deftest get-pseudolegal-destinations-pawn
  (testing
   (is (= #{[2 3] [3 3]}
          (get-pseudolegal-destinations initial-board [1 3])))))


(deftest occupied-squares-test
  (testing
      (is (= '([0 0] [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] [1 0] [1 1] [1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [6 0] [6 1] [6 2] [6 3] [6 4] [6 5] [6 6] [6 7] [7 0] [7 1] [7 2] [7 3] [7 4] [7 5] [7 6] [7 7])
             (occupied-squares initial-board)))))


(deftest squares-attacked-by-player-test
  (testing
      (is (= #{[0 1] [0 2] [0 3]
               [1 0] [0 6] [1 3]
               [3 0] [3 4] [4 3] [3 7] [4 1] [4 6] [5 0] [3 2]
               [4 5] [5 6]
               [2 6] [4 4] [5 3] [6 2] [5 7]
               [2 7] [3 6]}
           (squares-attacked-by-player testing-board :black)))))

(deftest in-check?-test
  (testing
      (is (= false
             (in-check? testing-board :white)))))
