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
           (get-legal-destinations testing-game-state [0 4]))))
  (testing "white king with castling possible on both sides"
      (is (= #{[7 2] [7 3] [7 5] [7 6] [6 4] [6 5]}
             (get-legal-destinations
              (fen->game-state "rnb1kbnr/p4ppp/1pp5/3p1qN1/2BpP3/2N1BP2/PPPQ2PP/R3K2R w KQkq")
              [7 4]))))
  (testing "black king without castling"
    (is (= #{[0 3] [1 3] [1 4]}
           (get-legal-destinations
            (fen->game-state "rnb1kbnr/p4ppp/1pp5/3p1qN1/2BpP3/2N1BP1P/PPPQ2P1/R3K2R b KQkq")
            [0 4]))))
  (testing "black king with castling possible on king's side"
    (is (= #{[0 6] [0 5] [1 3] [0 3]}
           (get-legal-destinations
            (fen->game-state "rnb1k2r/p4ppp/1ppb1n2/3N1qN1/2BpP3/1P2BP1P/P1PQ2P1/R3K2R b KQkq")
            [0 4])))))

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

(deftest castling-possible?-test
  (testing "white castle king side"
    (is (= true
           (castling-possible?
            (fen->game-state "rnbqkbnr/pp3ppp/2pp4/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq")
            :ks))))
  (testing "white castle king side under castling squares attacked"
    (is (= false
           (castling-possible?
            (fen->game-state "rn1qkbnr/ppp3pp/3p1p2/4p3/2B1P3/5NPb/PPPP1P1P/RNBQK2R w KQkq")
            :ks))))
  (testing "white castle king side under check"
    (is (= false
           (castling-possible?
            (fen->game-state "rnb1kbnr/ppp2ppp/3p4/4p1N1/2B1P3/5Pq1/PPPP2PP/RNBQK2R w KQkq")
            :ks)))))

