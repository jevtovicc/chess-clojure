(ns clojure-chess.rules
  (:require
   [clojure-chess.utils :refer [my-any? take-while-including]]))

(def white-piece? #{:R :N :B :Q :K :P})
(def black-piece? #{:r :n :b :q :k :p})
(def all-squares
  (for [x (range 8)
        y (range 8)]
    [x y]))

(def notation->sq {:a1 [7 0] :b1 [7 1] :c1 [7 2] :d1 [7 3] :e1 [7 4] :f1 [7 5] :g1 [7 6] :h1 [7 7]
                   :a2 [6 0] :b2 [6 1] :c2 [6 2] :d2 [6 3] :e2 [6 4] :f2 [6 5] :g2 [6 6] :h2 [6 7]
                   :a3 [5 0] :b3 [5 1] :c3 [5 2] :d3 [5 3] :e3 [5 4] :f3 [5 5] :g3 [5 6] :h3 [5 7]
                   :a4 [4 0] :b4 [4 1] :c4 [4 2] :d4 [4 3] :e4 [4 4] :f4 [4 5] :g4 [4 6] :h4 [4 7]
                   :a5 [3 0] :b5 [3 1] :c5 [3 2] :d5 [3 3] :e5 [3 4] :f5 [3 5] :g5 [3 6] :h5 [3 7]
                   :a6 [2 0] :b6 [2 1] :c6 [2 2] :d6 [2 3] :e6 [2 4] :f6 [2 5] :g6 [2 6] :h6 [2 7]
                   :a7 [1 0] :b7 [1 1] :c7 [1 2] :d7 [1 3] :e7 [1 4] :f7 [1 5] :g7 [1 6] :h7 [1 7]
                   :a8 [0 0] :b8 [0 1] :c8 [0 2] :d8 [0 3] :e8 [0 4] :f8 [0 5] :g8 [0 6] :h8 [0 7]})

(defn square-empty? [board sq]
  (= (get-in board sq) :e))

(defn square-on-board? [[row col]]
  (and (<= 0 row 7) (<= 0 col 7)))

(defn piece-color [p]
  (cond
    (white-piece? p) :white
    (black-piece? p) :black))

(defn same-piece-color? [p1 p2]
  (= (piece-color p1) (piece-color p2)))

(defn get-piece [board sq]
  (get-in board sq))

(defn remove-piece [board from-sq]
  (assoc-in board from-sq :e))

(defn place-piece [board to-sq p]
  (assoc-in board to-sq p))

(defn move-piece [board from-sq to-sq]
  (let [piece (get-piece board from-sq)]
    (-> board
        (remove-piece from-sq)
        (place-piece to-sq piece))))

(defn occupied-squares [board]
  (remove (partial square-empty? board) all-squares))

(defn add-squares [sq1 sq2]
  (map + sq1 sq2))

(defn get-pieces-in-row [board row]
  (board row))

(defn get-opponent [player]
  (condp = player
    :white :black
    :black :white))

(defn get-players-king [player]
  (condp = player
    :white :K
    :black :k))

;; directions
(def dir-up         [1 0])
(def dir-down       [-1 0])
(def dir-right      [0 1])
(def dir-left       [0 -1])
(def dir-up-right   [1 1])
(def dir-up-left    [1 -1])
(def dir-down-right [-1 1])
(def dir-down-left  [-1 -1])

(def rook-directions [dir-left dir-right dir-up dir-down])
(def bishop-directions [dir-up-left dir-up-right dir-down-left dir-down-right])
(def all-directions (concat rook-directions bishop-directions))
(def knight-directions [[2 1] [2 -1] [1 2] [1 -2]
                        [-2 1] [-2 -1] [-1 2] [-1 -2]])

(defmulti get-pseudolegal-destinations (fn [game-state from-sq] (get-piece (:board game-state) from-sq)))

(defmethod get-pseudolegal-destinations :n
  [{board :board} from-sq]
  (->> knight-directions
       (map (partial add-squares from-sq))
       (filter square-on-board?)
       (remove #(same-piece-color? :n (get-piece board %)))
       set))

(defmethod get-pseudolegal-destinations :N
  [{board :board} from-sq]
  (->> knight-directions
       (map (partial add-squares from-sq))
       (filter square-on-board?)
       (remove #(same-piece-color? :N (get-piece board %)))
       set))

(defn get-squares-in-direction [board from-sq dir]
  (let [piece (get-piece board from-sq)]
    (->> (add-squares from-sq dir)
         (iterate (partial add-squares dir))
         (take-while-including (partial square-empty? board))
         (filter square-on-board?)
         (remove #(same-piece-color? piece (get-piece board %))))))

(defn get-squares-in-directions [board from-sq dirs]
  (set (mapcat (partial get-squares-in-direction board from-sq) dirs)))

(defn castling-squares [side player]
  (case [side player]
    [:ks :white] #{[7 4] [7 5] [7 6]}
    [:qs :white] #{[7 1] [7 2] [7 3] [7 4]}
    [:ks :black] #{[0 4] [0 5] [0 6]}
    [:qs :black] #{[0 1] [0 2] [0 3] [0 4]}))

(defn castling-squares-empty? [board side player]
  (let [castling-squares (castling-squares side player)
        king (get-players-king player)]
    (->> castling-squares
         (remove #(= king (get-piece board %)))
         (every? (partial square-empty? board)))))

(declare squares-attacked-by-player)

(defn castling-squares-attacked?
  [{:keys [board player] :as game-state} attacking-player side]
  (let [castling-squares (castling-squares side player)]
    (my-any? castling-squares (squares-attacked-by-player game-state attacking-player))))

(defn castling-possible? [{:keys [board player] :as game-state} side]
  (let [opponent (get-opponent player)]
    (and (castling-squares-empty? board side player)
         (not (castling-squares-attacked? game-state opponent side)))))

;; TODO: remove repetition. Multimethod dispatch on alternatives (eg. :r | :R) ???
(defmethod get-pseudolegal-destinations :r
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq rook-directions))

(defmethod get-pseudolegal-destinations :R
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq rook-directions))

(defmethod get-pseudolegal-destinations :b
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq bishop-directions))

(defmethod get-pseudolegal-destinations :B
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq bishop-directions))

(defmethod get-pseudolegal-destinations :q
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq all-directions))

(defmethod get-pseudolegal-destinations :Q
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq all-directions))

;; TODO: find more functional implementation
(defmethod get-pseudolegal-destinations :k
  [{:keys [board black-can-castle-ks? black-can-castle-qs?] :as game-state} from-sq]
  (->> (into [(when (and black-can-castle-ks? (castling-possible? game-state :ks))
                [0 6])
              (when (and black-can-castle-qs? (castling-possible? game-state :qs))
                [0 2])]
             (->> all-directions
                  (map (partial add-squares from-sq))
                  (filter square-on-board?)
                  (remove #(same-piece-color? :k (get-piece board %)))))
       (keep identity)
       set))

(defmethod get-pseudolegal-destinations :K
  [{:keys [board white-can-castle-ks? white-can-castle-qs?] :as game-state} from-sq]
  (->> (into [(when (and white-can-castle-ks? (castling-possible? game-state :ks))
                [7 6])
              (when (and white-can-castle-qs? (castling-possible? game-state :qs))
                [7 2])]
             (->> all-directions
                  (map (partial add-squares from-sq))
                  (filter square-on-board?)
                  (remove #(same-piece-color? :K (get-piece board %)))))
       (keep identity)
       set))

;; TODO: find more functional implementation
(defmethod get-pseudolegal-destinations :p
  [{board :board en-passant :en-passant} from-sq]
  (let [not-moved? (= (first from-sq) 1)
        one-up (add-squares dir-up from-sq)
        two-up (add-squares dir-up one-up)
        one-up-right (add-squares dir-up-right from-sq)
        one-up-left (add-squares dir-up-left from-sq)]
    (->> [(when (square-empty? board one-up)
            one-up)
          (when (and not-moved? (square-empty? board one-up) (square-empty? board two-up))
            two-up)
          (when (and (not (square-empty? board one-up-left))
                     (not (same-piece-color? :p (get-piece board one-up-left))))
            one-up-left)
          (when (and (not (square-empty? board one-up-right))
                     (not (same-piece-color? :p (get-piece board one-up-right))))
            one-up-right)
          (when en-passant
            ((keyword en-passant) notation->sq))]
         (keep identity)
         (filter square-on-board?)
         set)))

;; TODO: find more functional implementation
(defmethod get-pseudolegal-destinations :P
  [{board :board en-passant :en-passant} from-sq]
  (let [not-moved? (= (first from-sq) 6)
        one-down (add-squares dir-down from-sq)
        two-down (add-squares dir-down one-down)
        one-down-right (add-squares dir-down-right from-sq)
        one-down-left (add-squares dir-down-left from-sq)]
    (->> [(when (square-empty? board one-down)
            one-down)
          (when (and not-moved? (square-empty? board one-down) (square-empty? board two-down))
            two-down)
          (when (and (not (square-empty? board one-down-left))
                     (not (same-piece-color? :P (get-piece board one-down-left))))
            one-down-left)
          (when (and (not (square-empty? board one-down-right))
                     (not (same-piece-color? :P (get-piece board one-down-right))))
            one-down-right)
          (when en-passant
            ((keyword en-passant) notation->sq))]
         (keep identity)
         (filter square-on-board?)
         set)))

(defn remove-castling-from-game-state [game-state]
  (merge game-state {:white-can-castle-ks? false
                     :white-can-castle-qs? false
                     :black-can-castle-ks? false
                     :black-can-castle-qs? false}))

;; TODO: change from pseudolegal to legal destinations. Watch for infinite recursion
;; TODO: find better solution to infinite recursion on game-state without castling
(defn squares-attacked-by-player [{board :board :as game-state} player]
  (let [game-state (remove-castling-from-game-state game-state)]
    (->> board
         occupied-squares
         (filter #(= player (piece-color (get-piece board %))))
         (mapcat (partial get-pseudolegal-destinations game-state))
         set)))

(defn in-check? [{:keys [board player] :as game-state}]
  (let [attacked-king (get-players-king player)
        opponent (get-opponent player)]
    (->> (squares-attacked-by-player game-state opponent)
         (my-any? #(= attacked-king (get-piece board %))))))

(defn in-check-after-move? [{board :board :as game-state} from-sq to-sq]
  (in-check? (assoc game-state :board (move-piece board from-sq to-sq))))

(defn get-legal-destinations [game-state from-sq]
  (->> (get-pseudolegal-destinations game-state from-sq)
       (remove (partial in-check-after-move? game-state from-sq))
       set))

(defn check-mate? [{:keys [board player] :as game-state}]
  (->> board
       occupied-squares
       (filter (fn [sq] (= (piece-color (get-piece board sq)) player)))
       (mapcat (partial get-legal-destinations game-state))
       set
       empty?))
