(ns dev.reus.rondo.model2
  (:require [dev.reus.rondo.gamedata :as gamedata]))

(defn dot-product [v1 v2]
  "Compute the dot product of two vectors."
  (apply + (map * v1 v2)))

(defn magnitude [[vx vy]]
  "Compute the magnitude of a vector."
  (.sqrt js/Math (+ (* vx vx) (* vy vy))))

(defn normalize [[vx vy]]
  "Get the normalized version of a vector."
  (let [mag (magnitude [vx vy])]
    (if (zero? mag)
      [0 0]
      (mapv #(/ % mag) [vx vy]))))

(defn find-color [team]
  "Determine the shirt-color of a player. Use a team keyword."
  (loop [i 0]
    (if-let [t (get gamedata/teams i)]
      (if (= (:id t) team)
        (:color t)
        (recur (inc i)))
      [0 0 0])))

(defn init-player [idx player]
  "Initialize player vector. Adds different game related
   properties to each player hashmap."
  (let [team (:team player)
        color (find-color team)
        [r g b] (map #(* 256 %) color)
        color-str (str "rgb(" r "," g "," b ")")]
    (assoc player
           :index idx
           :color color
           :color-str color-str)))

(defn init-players []
  (mapv init-player (range) gamedata/players))

(defn init-teams []
  gamedata/teams)

(defn update-players [state]
  state)

(defn update-ball [{ball :ball :as state}]
  (if-let [p (:player ball)]
    (let [p-pos (get-in state [:players p :pos])
          p-direction (get-in state [:players p :direction])]
      (let [new-pos (mapv + p-pos (map #(* % (+ (:player-radius gamedata/settings)
                                                (:ball-radius gamedata/settings)
                                                (:distance-to-ball gamedata/settings))) p-direction))]
        (assoc-in state [:ball :pos] new-pos)))
    (let [ke (:ke ball)]
      (if (pos? ke)
        (let [dir (:direction ball)
              vmag (.sqrt js/Math (* ke 2))
              [vel-x vel-y] (map #(* % vmag) dir)
              dt (:frame-time state)
              pos (:pos ball)
              new-position (mapv + pos [(* dt vel-x) (* dt vel-y)])]
          (assoc state :ball {:direction dir
                              :pos new-position
                              :velocity [vel-x vel-y]
                              :ke (- ke (* 1 vmag vmag dt))
                              :player nil}))
        state))))

(defn reset-position [state i]
  (let [player (get-in state [:players i])
        pos (get-in gamedata/players [i :pos])
        reset (assoc player :pos pos :acceleration [0 0] :velocity [0 0])
        new-state (assoc-in state [:players i] reset)]
    new-state))

(defn random-position [state i]
  (let [player (get-in state [:players i])
        [min-x min-y] [(:player-radius gamedata/settings) (:player-radius gamedata/settings)]
        [max-x max-y] (map #(- % (:player-radius gamedata/settings))
                           (:pitch-size gamedata/settings))
        new-pos [(+ min-x (int (* max-x (.random js/Math))))
                 (+ min-y (int (* max-y (.random js/Math))))]
        new-player (assoc player :pos new-pos)
        new-state (assoc-in state [:players i] new-player)]
    new-state))

(defn point-in-player? [[x y] player]
  "Determine whether the point (x,y) is part of a player's body."
  (let [player-radius (:player-radius gamedata/settings)
        ab [(* 2 player-radius) 0]
        bc [0 (* 2 player-radius)]
        [x-player y-player] (:pos player)
        pa [(- x-player player-radius) (- y-player player-radius)]
        pb [(+ x-player player-radius) (- y-player player-radius)]
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

(defn control-player [player dir]
  (println player dir)
  player)
