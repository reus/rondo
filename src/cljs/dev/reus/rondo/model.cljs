(ns dev.reus.rondo.model
  (:require [dev.reus.rondo.gamedata :as gamedata]
            [dev.reus.rondo.canvas2d :as canvas2d]
            [cljs.pprint :refer [pprint]]))

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

(defn find-color [team]
  "Determine the shirt-color of a team. Use a keyword."
  (loop [i 0]
    (if-let [t (get gamedata/teams i)]
      (if (= (:id t) team)
        (:color t)
        (recur (inc i)))
      [0 0 0])))

(defn init-players [players]
  "Initialize player vector. Adds an index, color and colorstring
   to each player hashmap."
  (loop [i 0 p-ext []]
    (if-let [player (get players i)]
      (let [team (:team player)
            color (find-color team)
            [r g b] (map #(* 256 %) color)
            color-str (str "rgb(" r "," g "," b ")")]
        (recur (inc i) (conj p-ext (assoc player
                                          :index i
                                          :color color
                                          :color-str color-str))))
      p-ext)))

(defn init-state []
  "Create the initial state hashmap."
  (let [players (init-players gamedata/players)
        ;webgl (dev.reus.rondo.webgl/setup-webgl (count players))]
        drawing-context (canvas2d/init-drawing-context)]
    {:refresh-rate 20
     :start-millis (.now js/Date)
     :current-millis (.now js/Date)
     :frame-millis 0
     :frame 1
     :drawing-context drawing-context
     :player-with-ball 0
     :players players
     :ui-state nil}))

(defn dot-product [v1 v2]
  "Helper function to compute the dot product."
  (apply + (map * v1 v2)))

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

(defn reset-positions [state]
  "Reset all player's positions."
  state)

(defn reset-position-selected-player [state]
  "Reset the position of the selected player."
  (let [index (:selected-player state)]
    (assoc-in state [:players index :pos] (:pos (get gamedata/players index)))))

(defn random-position-selected-player [state]
  "Change player's position to a random position."
  (let [index (:selected-player state)]
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
          (assoc-in state [:players index :pos] [x y]))))))

(defn randomize-positions [state]
  "Randomize all player's positions"
  (let [players (loop [i 0 ps (:players state) positions #{}]
                  (if-let [p (get ps i)]
                    (let [x (+ 5 (* 10 (rand-int 40)))
                          y (+ 5 (* 10 (rand-int 56)))]
                      (if (contains? positions [x y])
                        (recur i ps positions)
                        (recur (inc i) (assoc-in ps [i :pos] [x y]) (conj positions [x y]))))
                    ps))]
    (assoc state :players players)))

(defn ui-select-team [state t]
  "Set team t as selected in game state."
    (assoc state :selected-team t :selected-player nil))

(defn ui-select-player [state player-index]
  "Set player with index player-index as selected in game state."
    (assoc state :selected-team nil :selected-player player-index))
