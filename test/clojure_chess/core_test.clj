(ns clojure-chess.core-test
  (:require [clojure.test :refer :all]
            [clojure-chess.board :refer :all]
            [clojure-chess.rules :refer :all]))

(def testing-fen "rnbqkbnr/ppp1pp1p/8/1B1p2p1/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq")
(def testing-game-state (fen->game-state testing-fen))

(deftest get-pseudolegal-destinations-test
  (testing "black knight"
    (is (= #{[2 0] [2 2] [1 3]}
           (get-pseudolegal-destinations testing-game-state [0 1]))))
  (testing "black pawn"
    (is (= #{[2 2] [3 2]}
           (get-pseudolegal-destinations testing-game-state [1 2]))))
  (testing "black king"
    (is (= #{[1 3]}
           (get-pseudolegal-destinations testing-game-state [0 4]))))
  (testing "white pawn"
    (is (= #{[3 3] [3 4]}
           (get-pseudolegal-destinations
            (fen->game-state "rnbqkbnr/pp2pp1p/2p5/1B1p2p1/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq")
            [4 4]))))
  (testing "white pawn en passant test"
    (is (= #{[2 3] [2 4] [2 5]}
           (get-pseudolegal-destinations
            (fen->game-state "rnbqkbnr/ppp1p1pp/3p4/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6")
            [3 4])))))

(deftest get-legal-destinations-test
  (testing "black knight"
    (is (= #{[2 2] [1 3]}
           (get-legal-destinations testing-game-state [0 1]))))
  (testing "black pawn"
    (is (= #{[2 2]}
           (get-legal-destinations testing-game-state [1 2]))))
  (testing "black king"
    (is (= #{}
           (get-legal-destinations testing-game-state [0 4])))))

(deftest in-check?-test
  (testing
   (is (= true
          (in-check? testing-game-state)))))

(deftest check-mate?-test
  (testing
   (is (= false
          (check-mate? testing-game-state))))
  (testing
   (is (= true
          (check-mate? (fen->game-state "r1bqkbnr/p2ppQpp/npp5/8/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq"))))))
