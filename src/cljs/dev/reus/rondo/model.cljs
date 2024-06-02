(ns dev.reus.rondo.model
  (:require [dev.reus.rondo.math :as math :refer [dot-product magnitude normalize
                                                  add subtract add-scaled distance angle
                                                  rotate scale-by projection para project
                                                  proj distance-circles circles-overlap?]]
            [dev.reus.rondo.gamedata :as gamedata]))

(defn find-color [team]
  (->> gamedata/teams
       (filter (comp (partial = team) :id))
       first
       :color))

(defn init-player [idx player]
  "Initialize player vector. Adds different game related
   properties to each player hashmap."
  (let [team (:team player)
        color (find-color team)
        [r g b] (map #(* 256 %) color)
        color-str (str "rgb(" r "," g "," b ")")]
    (assoc player
           :goal {:status :idle}
           :index idx
           :color color
           :color-str color-str)))

(defn init-players []
  "Initialize player data to be used in gameloop."
  (mapv init-player (range) gamedata/players))

(defn init-teams []
  "Initialize team data to be used in gameloop."
  gamedata/teams)

(defn compute-integrals
  ([player acc-dir dt]
   (compute-integrals player acc-dir 0 dt))
  ([{[vx vy] :velocity pos :pos dir :direction :as player} acc-dir turn dt]
   (let [magn (magnitude [vx vy])
         new-acc (mapv + (map #(* 100 %) acc-dir) (map #(* -0.3 magn %) [vx vy]) (map * (map #(* turn %) [-1 1]) [vy vx]))
         new-vel (mapv + [vx vy] (map #(* dt %) new-acc))
         new-pos (mapv + pos (map #(* dt %) new-vel))
         new-dir (if (< magn 7)
                   (rotate dir (* 0.03 turn))
                   (normalize new-vel))]
       (assoc player :acceleration new-acc :velocity new-vel :pos new-pos :prev-pos pos :direction new-dir))))

(defn set-direction [{vel :velocity :as player}]
  (assoc player :direction (normalize vel)))

(defn get-player-update-fn [state]
  (fn [{acc :acceleration vel :velocity pos :pos dir :direction :as player}]
    (let [dt (* 0.001 (:frame-time state))
          goal (:goal player)]
      (case (:status goal)
        :human-controlled (let [run (:run goal)
                                acc-dir (map #(* run %) (:acc-dir goal))
                                turn (*  run (:turn goal))]
                            (if (zero? turn)
                              (compute-integrals player acc-dir dt)
                              (if (= acc-dir [0 0])
                                (compute-integrals player acc-dir turn dt)
                                (compute-integrals player acc-dir turn dt))))
        :move-direction (let [acc-dir (:direction goal)]
                              (-> player
                                  (compute-integrals acc-dir dt)))
        :move-destination (let [dest (:destination goal)
                                run (:run goal)
                                dest-vector (subtract dest pos)
                                norm (map #(* run %)(normalize dest-vector))
                                new-player (-> player
                                               (compute-integrals norm dt))
                                vec-new-player-to-dest (subtract dest (:pos new-player))]
                            (if (> (dot-product dest-vector vec-new-player-to-dest) 10) ;check if destination has been (almost) reached
                              new-player
                              (assoc player :goal {:status :decelerate})))
        :decelerate (let [vel (:velocity player)
                          magn (magnitude vel)]
                      (if (> magn 5)
                        (compute-integrals player [0 0] dt)
                        (assoc player :acceleration [0 0] :velocity [0 0] :goal {:status :idle})))
        :approach-ball (let [ball (:ball state)
                             ball-state (:state ball)]
                         (if (= :moving ball-state)
                           (let [pos-ball (:pos ball)
                                 dir-ball (normalize (:velocity ball))
                                 pos-player (:pos player)
                                 vec-ball-to-player (subtract pos-player pos-ball)
                                 pr (proj dir-ball vec-ball-to-player)]
                             (println pos-ball)
                             (println dir-ball)
                             (println pos-player)
                             (println vec-ball-to-player)
                             (println pr)
                             (assoc player :goal {:status :move-destination
                                                  :destination (add pos-ball pr)
                                                  :run 3}))
                           player))
        :set-idle (assoc player :goal {:status :idle} :acceleration [0 0] :velocity [0 0])
        player))))

(defn update-players [{players :players :as state}]
  (let [update-fn (get-player-update-fn state)]
    (assoc state :players (mapv update-fn players))))

(defn position-ball-with-player [{pos :pos dir :direction}]
  (mapv + pos (map #(* % (+ (:player-radius gamedata/settings)
                            (:ball-radius gamedata/settings)
                            (:distance-to-ball gamedata/settings))) dir)))

(defn update-ball [{ball :ball :as state}]
  (if-let [p (:player ball)]
    (case (:state ball)
      (:with-player
       :shooting) (let [player (get-in state [:players p])
                         new-pos (position-ball-with-player player)]
                     (assoc-in state [:ball :pos] new-pos))
      :shot-initiated (let [player (get-in state [:players p])
                            new-pos (position-ball-with-player player)
                            t (:time state)
                            new-ball (assoc ball :shot-start-time t :pos new-pos)]
                        (assoc state :ball new-ball))
      :release-shot (let [player (get-in state [:players p])
                          pos (:pos player)
                          dir (:direction player)
                          t (:time state)
                          shot-start-time (:shot-start-time ball)
                          ke (* (- t shot-start-time) 100)
                          vmag (.sqrt js/Math (* ke 2))
                          vel (map #(* % vmag) dir)
                          new-pos (position-ball-with-player player)]
                      (assoc state :ball {:state :moving
                                          :player nil
                                          :velocity vel
                                          :ke ke
                                          :pos new-pos
                                          :shot-start-time nil})))
    (case (:state ball)
      :moving (let [ke (:ke ball)]
                (if (> ke 1)
                  (let [vmag (.sqrt js/Math (* ke 2))
                        vel (:velocity ball)
                        new-vel (map #(* % vmag) (normalize vel))
                        dt (* 0.001 (:frame-time state))
                        pos (:pos ball)
                        new-position (mapv + pos (map #(* dt %) new-vel))]
                    (assoc state :ball {:state :moving
                                        :pos new-position
                                        :velocity new-vel
                                        :ke (- ke (* 1 vmag vmag dt))
                                        :player nil
                                        :shot-start-time nil}))
                  (assoc state :ball {:state :still
                                      :pos (:pos ball)
                                      :velocity [0 0]
                                      :ke 0
                                      :player nil
                                      :shot-start-time nil})))
      :still state)))

(defn reset-position [state i]
  (let [player (get-in state [:players i])
        pos (get-in gamedata/players [i :pos])
        dir (get-in gamedata/players [i :direction])
        reset (assoc player :pos pos :acceleration [0 0] :velocity [0 0] :direction dir :goal {:status :idle})
        new-state (assoc-in state [:players i] reset)]
    new-state))

(defn random-position [state i]
  (let [player (get-in state [:players i])
        [min-x min-y] [(:player-radius gamedata/settings) (:player-radius gamedata/settings)]
        [max-x max-y] (map #(- % (:player-radius gamedata/settings))
                           (:pitch-size gamedata/settings))
        new-pos [(+ min-x (int (* max-x (.random js/Math))))
                 (+ min-y (int (* max-y (.random js/Math))))]
        new-player (assoc player :pos new-pos :acceleration [0 0] :velocity [0 0] :goal {:status :idle})
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

(defn player-player-collisions [{players :players :as state}]
  (let [r (:player-radius gamedata/settings)
        ps (mapv #(select-keys % [:pos :index]) players)
        positions (for [p ps :let [i (:index p)]]
                    [p (mapv :pos (filter #(not= (:index %) i) ps))])]
    (loop [ps positions new-players []]
      (if-let [x (first ps)]
        (let [collisions (map #(circles-overlap? (get (first x) :pos) r % r) (last x))]
          (let [index (get (first x) :index)
                player (get players index)]
            (if (some identity collisions)
              (recur (rest ps) (conj new-players (assoc player :goal {:status :idle}
                                                        :pos (or (get player :prev-pos)
                                                                 (get player :pos)))))
              (recur (rest ps) (conj new-players player)))))
        (assoc state :players new-players)))))

(defn player-ball-collisions [{players :players ball :ball :as state}]
  (let [ball-pos (get ball :pos)
        ball-radius (get gamedata/settings :ball-radius)
        player-radius (get gamedata/settings :player-radius)]
    (loop [i 0 collisions []]
      (if-let [p (get players i)]
        (if (circles-overlap? (get p :pos) player-radius ball-pos ball-radius)
          (recur (inc i) (conj collisions (get p :index)))
          (recur (inc i) collisions))
        (case (count collisions)
          0 state
          1 (let [index (first collisions)
                  player (get-in state [:players index])
                  pos-player (:pos player)
                  vector-player-ball (map - ball-pos pos-player)
                  norm-vector-player-ball (normalize vector-player-ball)
                  dir (:direction player)
                  angle (angle dir norm-vector-player-ball)]
              (if (< angle (get gamedata/settings :max-angle-pickup-ball))
                (assoc state :ball {:state :with-player
                                    :player index
                                    :ke 0 ;; kinetic energy
                                    :velocity nil
                                    :pos (position-ball-with-player player)
                                    :shot-start-time nil})
                ;; ball bounces on player, calculate new velocity
                ;; based on method described in book
                ;; "Physics for javascript games, animation, and simulations"
                ;; (Adrian Dobre and Dev Ramtal)
                (let [v-ball (get ball :velocity)
                      v-player (get player :velocity)
                      normal-velo-ball (project v-ball vector-player-ball)
                      normal-velo-player (project v-player vector-player-ball)
                      tangent-velo-ball (subtract v-ball normal-velo-ball)
                      L (- (+ ball-radius player-radius) (magnitude vector-player-ball))
                      vrel (magnitude (subtract normal-velo-ball normal-velo-player))
                      ball-pos-impact (add-scaled ball-pos normal-velo-ball (/ (* -1 L) vrel))
                      u1 (projection normal-velo-ball vector-player-ball)
                      u2 (projection normal-velo-player vector-player-ball)
                      v1 (+ (* -1 u1) (* 2 u2)) ;assuming the weight of the ball is much smaller then the player
                      normal-velo-1 (para vector-player-ball v1)
                      new-v-ball (add normal-velo-1 tangent-velo-ball)]
                  (assoc state :ball {:state :moving
                                      :player nil
                                      :ke (* (get ball :ke) ;; lose kinetic energy
                                             (get gamedata/settings :factor-ke-loss-on-collision))
                                      :velocity new-v-ball
                                      :pos ball-pos-impact
                                      :shot-start-time nil}))))
          2 (do
              (println 2)
              state)
          state)))))

(defn check-ball [{ball :ball :as state}]
  (let [[x y] (get ball :pos)]
    (if (or (js/isNaN x) (js/isNaN y))
      (do
        (println state)
        (assoc-in state [:ball :pos] [10 10]))
      state)))

(comment
  (find-color :nl)
)
