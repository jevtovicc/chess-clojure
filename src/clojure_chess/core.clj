(ns clojure-chess.core)

;; utils
(defn my-any?
  "Returns true if some element in coll satisfies predicate"
  [pred col]
  (not (not-any? pred col)))

(defn take-while-including [pred coll]
  (let [[take-while-part remainding-part] (split-with pred coll)]
    (concat take-while-part [(first remainding-part)])))

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

(defn add-squares [sq1 sq2]
  (map + sq1 sq2))

(defmulti get-pseudolegal-destinations (fn [board from-sq] (get-piece board from-sq)))

(defmethod get-pseudolegal-destinations :n
  [board from-sq]
  (->> (map (partial add-squares from-sq) knight-directions)
       (filter square-on-board?)
       (remove #(same-piece-color? :n (get-piece board %)))
       set))

(defmethod get-pseudolegal-destinations :N
  [board from-sq]
  (->> (map (partial add-squares from-sq) knight-directions)
       (filter square-on-board?)
       (remove #(same-piece-color? :N (get-piece board %)))
       set))

(defn get-pieces-in-row [board row]
  (board row))

(defn get-squares-in-direction [board from-sq dir]
  (let [piece (get-piece board from-sq)]
    (->> (iterate (partial add-squares dir) (add-squares from-sq dir))
         (take-while-including (partial square-empty? board))
         (filter square-on-board?)
         (remove #(same-piece-color? piece (get-piece board %))))))

;; TODO: remove repetition. Multimethod dispatch on alternatives (eg. :r | :R) ???
(defmethod get-pseudolegal-destinations :r
  [board from-sq]
  (set (mapcat (partial get-squares-in-direction board from-sq) rook-directions)))

(defmethod get-pseudolegal-destinations :R
  [board from-sq]
  (set (mapcat (partial get-squares-in-direction board from-sq) rook-directions)))

(defmethod get-pseudolegal-destinations :b
  [board from-sq]
  (set (mapcat (partial get-squares-in-direction board from-sq) bishop-directions)))

(defmethod get-pseudolegal-destinations :B
  [board from-sq]
  (set (mapcat (partial get-squares-in-direction board from-sq) bishop-directions)))

(defmethod get-pseudolegal-destinations :q
  [board from-sq]
  (set (mapcat (partial get-squares-in-direction board from-sq) all-directions)))

(defmethod get-pseudolegal-destinations :Q
  [board from-sq]
  (set (mapcat (partial get-squares-in-direction board from-sq) all-directions)))

(defmethod get-pseudolegal-destinations :k
  [board from-sq]
  (->> (map (partial add-squares from-sq) all-directions)
       (filter square-on-board?)
       (remove #(same-piece-color? :k (get-piece board %)))
       set))

(defmethod get-pseudolegal-destinations :K
  [board from-sq]
  (->> (map (partial add-squares from-sq) all-directions)
       (filter square-on-board?)
       (remove #(same-piece-color? :K (get-piece board %)))
       set))

;; TODO: implement function body
(defmethod get-pseudolegal-destinations :p
  [board from-sq]
  [])

;; TODO: implement function body
(defmethod get-pseudolegal-destinations :P
  [board from-sq]
  [])

(defn occupied-squares [board]
  (remove (partial square-empty? board) all-squares))

(defn squares-attacked-by-player [board player]
  (->> (occupied-squares board)
       (filter (fn [sq] (= player (piece-color (get-piece board sq)))))
       (mapcat (partial get-pseudolegal-destinations board))
       set))

(defn in-check? [board player]
  (let [[attacked-king opponent] (condp = player
                                   :white [:K :black]
                                   :black [:k :white])]
    (->> (squares-attacked-by-player board opponent)
         (my-any? (fn [sq] (= attacked-king (get-piece board sq)))))))




