(ns clojure-chess.core-test
  (:require [clojure.test :refer :all]
            [clojure-chess.board :refer :all]
            [clojure-chess.rules :refer :all]))


(def testing-fen "rnbqkbnr/ppp1pp1p/8/1B1p2p1/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq")
(def testing-board (fen->board testing-fen))

(deftest get-pseudolegal-destinations-test
  (testing "black knight"
    (is (= #{[2 0] [2 2] [1 3]}
           (get-pseudolegal-destinations testing-board [0 1]))))
  (testing "black pawn"
    (is (= #{[2 2] [3 2]}
           (get-pseudolegal-destinations testing-board [1 2]))))
  (testing "black king"
    (is (= #{[1 3]}
           (get-pseudolegal-destinations testing-board [0 4]))))
  (testing "white pawn"
    (is (= #{[3 4] [3 3]}
           (get-pseudolegal-destinations
            (fen->board "rnbqkbnr/pp2pp1p/2p5/1B1p2p1/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq")
            [4 4])))))

(deftest get-legal-destinations-test
  (testing "black knight"
    (is (= #{[2 2] [1 3]}
           (get-legal-destinations testing-board :black [0 1]))))
  (testing "black pawn"
    (is (= #{[2 2]}
           (get-legal-destinations testing-board :black [1 2]))))
  (testing "black king"
    (is (= #{}
           (get-legal-destinations testing-board :black [0 4])))))

(deftest in-check?-test
  (testing
      (is (= true
             (in-check? testing-board :black)))))

(deftest check-mate?-test
  (testing
   (is (= false
          (check-mate? testing-board :black))))
  (testing
   (is (= true
          (check-mate?
           (fen->board "3rkbnr/1p1bp3/1q1p3p/p5pQ/3n4/PPR5/5PPP/6K1")
           :black)))))
