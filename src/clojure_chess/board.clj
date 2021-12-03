(ns clojure-chess.board
  (:require
   [clojure-chess.utils :refer [char->kw in?]]
   [clojure.string :as str]))

(defn fen-row->board-row
  "Converts string representation of row to vector of keywords"
  [fen-row]
  (reduce
   (fn [acc ch]
     (if (Character/isLetter ch)
       (conj acc (char->kw ch))
       (let [num (Character/digit ch 10)]
         (apply conj acc (take num (cycle [:e]))))))
   []
   fen-row))

(defn fen->board [fen]
  (-> fen
      (str/split #"/")
      (as-> res
            (conj (vec (butlast res)) (first (str/split (last res) #" "))))
      (->>
       (map (partial fen-row->board-row))
       vec)))

(defn fen-player->kw-player [fen-player]
  (condp = fen-player
    "w" :white
    "b" :black))

(defn get-player-from-fen [fen]
  (-> fen
      (str/split #"/")
      last
      (str/split #" ")
      second
      fen-player->kw-player))

(defn get-castling-info-from-fen [fen]
  (-> fen
      (str/split #"/")
      last
      (str/split #" ")
      (get 2)))

(defn en-passant? [fen]
  (-> fen
      (str/split #"/")
      last
      (str/split #" ")
      (get 3)))

(defn fen->game-state [fen]
  (let [board (fen->board fen)
        player (get-player-from-fen fen)
        castling-info (get-castling-info-from-fen fen)]
    {:board board
     :player player
     :white-can-castle-ks? (in? castling-info \K)
     :white-can-castle-qs? (in? castling-info \Q)
     :black-can-castle-ks? (in? castling-info \k)
     :black-can-castle-qs? (in? castling-info \q)
     :en-passant (en-passant? fen)}))

(def starting-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq")
(def game-state (fen->game-state starting-fen))
