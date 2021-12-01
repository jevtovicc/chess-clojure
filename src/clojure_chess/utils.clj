(ns clojure-chess.utils)

(defn my-any?
  "Returns true if some element in coll satisfies predicate"
  [pred col]
  (not (not-any? pred col)))

(defn take-while-including
  "Returns seq of all elements in coll that satisfies predicate,
  including first element that fails predicate"
  [pred coll]
  (let [[take-while-part remainding-part] (split-with pred coll)]
    (concat take-while-part [(first remainding-part)])))
