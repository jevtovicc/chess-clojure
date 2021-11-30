(ns clojure-chess.core)

(def initial-board [[:r :n :b :q :k :b :n :r]
                    [:p :p :p :p :p :p :p :p]
                    [:e :e :e :e :e :e :e :e]
                    [:e :e :e :e :e :e :e :e]
                    [:e :e :e :e :e :e :e :e]
                    [:e :e :e :e :e :e :e :e]
                    [:P :P :P :P :P :P :P :P]
                    [:R :N :B :Q :K :B :N :R]])

(def white-piece? #{:R :N :B :Q :K :P})
(def black-piece? #{:r :n :b :q :k :p})
(def all-squares
  (for [x (range 8)
        y (range 8)]
    [x y]))

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

;; rules

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

(defn take-while-including [pred coll]
  (let [[take-while-part remainding-part] (split-with pred coll)]
    (concat take-while-part [(first remainding-part)])))

(defn add-squares [sq1 sq2]
  (map + sq1 sq2))

(defmulti get-pseudolegal-destinations (fn [board from-sq] (get-piece board from-sq)))

(defmethod get-pseudolegal-destinations :n
  [board from-sq]
  (->> (map (partial add-squares from-sq) knight-directions)
       (filter square-on-board?)
       (remove #(same-piece-color? :n (get-piece board %)))))

(defmethod get-pseudolegal-destinations :N
  [board from-sq]
  (->> (map (partial add-squares from-sq) knight-directions)
       (filter square-on-board?)
       (remove #(same-piece-color? :N (get-piece board %)))))

(get-pseudolegal-destinations initial-board [0 1])

(defn get-pieces-in-row [board row]
  (board row))


;; TODO: find more functional implementation
(defn get-squares-in-direction [board from-sq dir]
  (let [piece (get-piece board from-sq)]
    (->> (loop [to-sq (add-squares from-sq dir)
                acc []]
           (if-not (square-empty? board to-sq)
             (conj acc to-sq)
             (recur (add-squares to-sq dir) (conj acc to-sq))))
         (filter square-on-board?)
         (remove #(same-piece-color? piece (get-piece board %))))))


;; TODO: remove repetition. Multimethod dispatch on alternatives (eg. :r | :R) ???
(defmethod get-pseudolegal-destinations :r
  [board from-sq]
  (mapcat (partial get-squares-in-direction board from-sq) rook-directions))

(defmethod get-pseudolegal-destinations :R
  [board from-sq]
  (mapcat (partial get-squares-in-direction board from-sq) rook-directions))

(defmethod get-pseudolegal-destinations :b
  [board from-sq]
  (mapcat (partial get-squares-in-direction board from-sq) bishop-directions))

(defmethod get-pseudolegal-destinations :B
  [board from-sq]
  (mapcat (partial get-squares-in-direction board from-sq) bishop-directions))

(defmethod get-pseudolegal-destinations :q
  [board from-sq]
  (mapcat (partial get-squares-in-direction board from-sq) all-directions))

(defmethod get-pseudolegal-destinations :Q
  [board from-sq]
  (mapcat (partial get-squares-in-direction board from-sq) all-directions))

(defmethod get-pseudolegal-destinations :k
  [board from-sq]
  (->> (map (partial add-squares from-sq) all-directions)
       (filter square-on-board?)
       (remove #(same-piece-color? :k (get-piece board %)))))

(defmethod get-pseudolegal-destinations :K
  [board from-sq]
  (->> (map (partial add-squares from-sq) all-directions)
       (filter square-on-board?)
       (remove #(same-piece-color? :K (get-piece board %)))))

;; TODO: implement function body
(defmethod get-pseudolegal-destinations :p
  [board from-sq]
  [])

;; TODO: implement function body
(defmethod get-pseudolegal-destinations :P
  [board from-sq]
  [])

(get-pseudolegal-destinations (move-piece initial-board [0 0] [3 2]) [3 2])
;; => ((3 1) (3 0) (3 3) (3 4) (3 5) (3 6) (3 7) (4 2) (5 2) (6 2) (2 2))

(get-pseudolegal-destinations (move-piece initial-board [0 2] [3 2]) [3 2])
;; => ((4 1) (5 0) (4 3) (5 4) (6 5) (2 1) (2 3))

(get-pseudolegal-destinations (move-piece initial-board [0 3] [3 2]) [3 2])
;; => ((3 1) (3 0) (3 3) (3 4) (3 5) (3 6) (3 7) (4 2) (5 2) (6 2) (2 2) (4 1) (5 0) (4 3) (5 4) (6 5) (2 1) (2 3))
;; TODO: implement function body



(get-pseudolegal-destinations (move-piece initial-board [0 4] [2 2]) [2 2])

;; TODO: fix implementation
(defn squares-attacked-by-opponent [board player-on-move]
  (->> (filter (fn [sq] (= player-on-move (piece-color (get-piece board sq)))) all-squares)
       (mapcat (partial get-pseudolegal-destinations board))))

(squares-attacked-by-opponent initial-board :white)

;; TODO: fix implementation
(defn in-check? [board player]
  (let [attacked-king (condp = player
                        :white :K
                        :black :k)]
    (->> (squares-attacked-by-opponent board player)
         (filter (fn [sq] (= attacked-king (get-piece board sq))))
         seq?)))

(in-check? (move-piece (move-piece initial-board [0 4] [3 2]) [7 2] [5 0]) :white)
;; => true




