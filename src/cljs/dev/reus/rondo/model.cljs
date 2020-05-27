(ns dev.reus.rondo.model
  (:require [dev.reus.rondo.gamedata :as gamedata]
            [cljs.pprint :refer [pprint]]))

(defonce keys-pressed [0 0])

(defn dot-product [v1 v2]
  "Helper function to compute the dot product of two vectors."
  (apply + (map * v1 v2)))

(defn magnitude [[vx vy]]
  "Helper function to compute the magnitude of a vector."
  (.sqrt js/Math (+ (* vx vx) (* vy vy))))

(defn normalize [[vx vy]]
  "Helper function to get the normalized version of a vector."
  (let [mag (magnitude [vx vy])]
    (if (zero? mag)
      [0 0]
      (mapv #(/ % mag) [vx vy]))))

(defn print-player-info! [player]
  "Print information of a player."
  (let [p (select-keys player [:name :pos])]
    (pprint p)))

(defn print-state [state]
  "Print game state."
  (let [players (:players state)]
    (doseq [player players]
      (print-player-info! player))
    state))

(defn point-in-player? [[x y] player]
  "Determine whether the point (x,y) is part of a player's body."
  (let [player-size (:player-size gamedata/settings)
        ab [(* 2 player-size) 0]
        bc [0 (* 2 player-size)]
        [x-player y-player] (:pos player)
        pa [(- x-player player-size) (- y-player player-size)]
        pb [(+ x-player player-size) (- y-player player-size)]
        am (map - [x y] pa)
        bm (map - [x y] pb)]
    (and (<= 0 (dot-product ab am))
         (<= (dot-product ab am) (dot-product ab ab))
         (<= 0 (dot-product bc bm))
         (<= (dot-product bc bm) (dot-product bc bc)))))

(defn point-in-players? [[x y] players]
  "Given a vector of players, determine whether the point (x,y)
   is part of a player's body."
  (loop [i 0]
    (if-let [player (get players i)]
      (if (point-in-player? [x y] player)
        i
        (recur (inc i))))))

(defn reset-position-player [state index]
  "Reset the position of the selected player."
  (assoc-in state [:players index :pos] (:pos (get gamedata/players index))))

(defn reset-positions [state]
  "Reset all player's positions."
  state)

(defn random-position-player [state index]
  "Change player's position to a random position."
  (loop []
    (let [x (+ 5 (rand-int 391))
          y (+ 5 (rand-int 391))
          p1 [(- x 5) (- y 5)]
          p2 [(+ x 5) (- y 5)]
          p3 [(+ x 5) (+ y 5)]
          p4 [(- x 5) (+ y 5)]
          ts (remove #(or (= % index) (false? %)) (map #(point-in-player? %1 state) [p1 p2 p3 p4]))]
      (if (pos? (count ts))
        (recur)
        (assoc-in state [:players index :pos] [x y])))))

(defn ui-select-team [state t]
  "Set team t as selected in game state."
    (assoc state :selected-team t :selected-player nil))

(defn ui-select-player [state player-index]
  "Set player with index player-index as selected in game state."
    (assoc state :selected-team nil :selected-player player-index))

(defn move-player-fn [dt]
  "update velocity and position for player."
  (fn [{vel :velocity pos :pos :as player}]
    (let [new-pos (mapv + pos (map #(* dt %) vel))]
      (assoc player :pos new-pos))))

(defn move-players [{players :players :as state}]
  (let [move-player (move-player-fn (:frame-time state))]
    (assoc state :players (mapv move-player players))))
