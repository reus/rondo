(ns dev.reus.rondo.model
  (:require [dev.reus.rondo.gamedata :as gamedata]
            [cljs.pprint :refer [pprint]]))

(defonce keys-pressed [0 0])

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

(defn move-ball [{ball :ball :as state}]
  (let [ke (:ke ball)]
    (if (pos? ke)
      (let [dir (:direction ball)
            vmag (.sqrt js/Math (* ke 2))
            [vel-x vel-y] (map #(* % vmag) dir)
            dt (:frame-time state)
            pos (:pos ball)]
        (assoc state :ball {:direction dir
                            :pos (mapv + pos [(* dt vel-x) (* dt vel-y)])
                            :velocity [vel-x vel-y]
                            :ke (- ke (* 1 vmag vmag dt))
                            :player nil}))
      state)))

(defn collision? [x1 y1 r1 x2 y2 r2]
  (<= (+ (* (- x2 x1) (- x2 x1))
         (* (- y2 y1) (- y2 y1)))
      (* (+ r1 r2) (+ r1 r2))))

(defn distance-between-circles [x1 y1 r1 x2 y2 r2]
 (- (.sqrt js/Math (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))) r1 r2))

(defn distance-to-ball [{ball :ball players :players :as state}]
  (let [player-with-ball (:player ball)
        [x-ball y-ball] (if (nil? player-with-ball)
                          (:pos ball)
                          (let [p (get players player-with-ball)
                                ball-radius (:ball-radius gamedata/settings)
                                player-radius (:player-radius gamedata/settings)
                                d (:distance-to-ball gamedata/settings)
                                [p-x p-y] (:pos p)
                                [pd-x pd-y] (:direction p)]
                            [(+ p-x (* pd-x (+ d ball-radius player-radius)))
                             (+ p-y (* pd-y (+ d ball-radius player-radius)))]))
        r-ball (:ball-radius gamedata/settings)
        r-player (:player-radius gamedata/settings)
        update-distance-to-ball (fn [p]
                                  (if (= (:index p) player-with-ball)
                                    (assoc p :distance-to-ball 0)
                                    (let [[x-player y-player] (:pos p)]
                                      (assoc p :distance-to-ball (distance-between-circles x-player
                                                                                         y-player
                                                                                         r-player
                                                                                         x-ball
                                                                                         y-ball
                                                                                         r-ball)))))]
    (assoc state :players (mapv update-distance-to-ball players))))

(defn pickup? [x y reach dir-x dir-y x-ball y-ball ball-radius]
  (let [distance (distance-between-circles x y reach x-ball y-ball ball-radius)]
    (if (<= distance 0)
      (let [proj-mag (dot-product [(- x-ball x) (- y-ball y)] [dir-x dir-y])]
        (cond
          (>= proj-mag (* 0.5 reach)) true
          :else false))
      false)))

(defn player-pickup-ball [{ball :ball players :players :as state}]
  (let [[x-ball y-ball] (:pos ball)
        ball-radius (:ball-radius gamedata/settings)]
    (loop [i 0]
      (if-let [player (get players i)]
        (let [[x y] (:pos player)
              [dir-x dir-y] (:direction player)
              reach (:reach player)
              pickup (pickup? x y reach dir-x dir-y x-ball y-ball ball-radius)]
          (if pickup
            (assoc state :ball {:player i :velocity nil :direction nil :ke 0 :pos nil})
            (recur (inc i))))
        state))))



