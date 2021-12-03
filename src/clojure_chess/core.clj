(ns clojure-chess.core
  (:require [clojure-chess.rules :refer :all]
            [clojure.string :as str]))

(def game-state {:board               initial-board
                 :game-over?          false
                 :player-on-move      :white
                 :white-can-castle-ks? true
                 :white-can-castle-qs? true
                 :black-can-castle-ks? true
                 :black-can-castle-qs? true
                 :moves-history       []})

(def notation->sq {:a1 [7 0] :b1 [7 1] :c1 [7 2] :d1 [7 3] :e1 [7 4] :f1 [7 5] :g1 [7 6] :h1 [7 7]
                   :a2 [6 0] :b2 [6 1] :c2 [6 2] :d2 [6 3] :e2 [6 4] :f2 [6 5] :g2 [6 6] :h2 [6 7]
                   :a3 [5 0] :b3 [5 1] :c3 [5 2] :d3 [5 3] :e3 [5 4] :f3 [5 5] :g3 [5 6] :h3 [5 7]
                   :a4 [4 0] :b4 [4 1] :c4 [4 2] :d4 [4 3] :e4 [4 4] :f4 [4 5] :g4 [4 6] :h4 [4 7]
                   :a5 [3 0] :b5 [3 1] :c5 [3 2] :d5 [3 3] :e5 [3 4] :f5 [3 5] :g5 [3 6] :h5 [3 7]
                   :a6 [2 0] :b6 [2 1] :c6 [2 2] :d6 [2 3] :e6 [2 4] :f6 [2 5] :g6 [2 6] :h6 [2 7]
                   :a7 [1 0] :b7 [1 1] :c7 [1 2] :d7 [1 3] :e7 [1 4] :f7 [1 5] :g7 [1 6] :h7 [1 7]
                   :a8 [0 0] :b8 [0 1] :c8 [0 2] :d8 [0 3] :e8 [0 4] :f8 [0 5] :g8 [0 6] :h8 [0 7]})



;; (def sq->notation {[7 0] "a1" [7 1] "b1" [7 2] "c1" [7 3] "d1" [7 4] "e1" [7 5] "f1" [7 6] "g1" [7 7] "h1"
;;                    [6 0] "a2" [6 1] "b2" [6 2] "c2" [6 3] "d2" [6 4] "e2" [6 5] "f2" [6 6] "g2" [6 7] "h2"
;;                    [5 0] "a3" [5 1] "b3" [5 2] "c3" [5 3] "d3" [5 4] "e3" [5 5] "f3" [5 6] "g3" [5 7] "h3"
;;                    [4 0] "a4" [4 1] "b4" [4 2] "c4" [4 3] "d4" [4 4] "e4" [4 5] "f4" [4 6] "g4" [4 7] "h4"
;;                    [3 0] "a5" [3 1] "b5" [3 2] "c5" [3 3] "d5" [3 4] "e5" [3 5] "f5" [3 6] "g5" [3 7] "h5"
;;                    [2 0] "a6" [2 1] "b6" [2 2] "c6" [2 3] "d6" [2 4] "e6" [2 5] "f6" [2 6] "g6" [2 7] "h6"
;;                    [1 0] "a7" [1 1] "b7" [1 2] "c7" [1 3] "d7" [1 4] "e7" [1 5] "f7" [1 6] "g7" [1 7] "h7"
;;                    [0 0] "a8" [0 1] "b8" [0 2] "c8" [0 3] "d8" [0 4] "e8" [0 5] "f8" [0 6] "g8" [0 7] "h8"})

(defn get-col-notation [col]
  (condp = col
    7 "a"
    6 "b"
    5 "c"
    4 "d"
    3 "e"
    2 "f"
    1 "g"
    0 "h"))

(defn get-input []
  (println "Enter your move in format eg. e2-e4")
  (read-line))

(defn parse-input [input]
  (str/split input #"-"))

(defn move->kws [move]
  (map keyword move))

(defn valid-input? [from-sq to-sq]
  (and (not= from-sq nil)
       (not= to-sq nil)
       (square-on-board? from-sq)
       (square-on-board? to-sq)))

(defn print-board [board]
  (print "    ")
  (doseq [col (range 7 -1 -1)]
    (print (get-col-notation col) " "))
  (println)
  (dorun
   (map
    (fn [row i] (println i row))
    board
    (iterate dec 8))))

(defn -main [& _args]
  (loop [{:keys [board game-over?]} game-state]
    (print-board board)
    (if game-over?
      (println "Game over")
      (let [input (get-input)
            parsed-input (parse-input input)
            [src dest] (move->kws parsed-input)
            [from-sq to-sq] (map notation->sq [src dest])]
        (if-not (valid-input? from-sq to-sq)
          (println "Invalid input for:" (str/join "-" input))
          (recur (assoc game-state :board (move-piece (:board game-state) from-sq to-sq))))))))




