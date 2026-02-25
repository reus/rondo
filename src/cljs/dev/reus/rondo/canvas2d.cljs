(ns dev.reus.rondo.canvas2d
  (:require [dev.reus.rondo.gamedata :as gamedata]
            [dev.reus.rondo.math :as math]
            [dev.reus.rondo.ui :as ui]
            [dev.reus.rondo.utils :refer [by-id]]))

(defonce pi (.-PI js/Math))
(defonce _2pi (* 2 (.-PI js/Math)))
(defonce half-pi (* 0.5 (.-PI js/Math)))

(defn init-drawing-context []
  (let [canvas (by-id "canvas")
        context (.getContext canvas "2d")]
    {:context context}))

(defn draw-player! [p state]
  (let [[x y] (:pos p)
        player-radius (:player-radius gamedata/settings)
        [dx dy] (:direction p)
        [vx vy] (:velocity p)
        speed (math/magnitude [vx vy])
        frame (:frame state)
        ;; Animation: 4-frame walk cycle, speed-dependent
        stride-period (if (> speed 50) 3 6)
        anim-frame (if (>= speed 2)
                     (mod (quot frame stride-period) 4)
                     -1) ;; -1 = idle
        ;; Rotation angle: atan2 of direction, offset by -π/2 because
        ;; our local "forward" is -Y (up)
        angle (+ (.atan2 js/Math dy dx) half-pi)
        selected-player (get @ui/ui-state :selected-player)
        ctx (:context (:drawing-context state))
        jersey (:color-str p)
        skin (:skin-str p)
        hair-color (:hair-color-str p)
        hair-style (:hair p)]
    ;; Selection indicator — pulsing ground marker drawn before the sprite
    (when (= (:index p) selected-player)
      (let [pulse (.sin js/Math (* frame 0.12))
            radius (+ 9 (* 2 pulse))
            alpha (+ 0.25 (* 0.1 pulse))]
        (set! (.-fillStyle ctx) (str "rgba(200,200,60," alpha ")"))
        (.beginPath ctx)
        (.arc ctx x y radius 0 _2pi)
        (.fill ctx)))
    (.save ctx)
    (.translate ctx x y)
    (.rotate ctx angle)
    (set! (.-lineCap ctx) "round")
    (set! (.-lineJoin ctx) "round")
    (set! (.-imageSmoothingEnabled ctx) false)
    ;; Feet (dark shoes peeking out behind body)
    (set! (.-fillStyle ctx) "#222")
    (case anim-frame
      0 (do (.beginPath ctx) (.arc ctx -1.5 5.5 1 0 _2pi) (.fill ctx)
            (.beginPath ctx) (.arc ctx 1 3.5 1 0 _2pi) (.fill ctx))
      1 (do (.beginPath ctx) (.arc ctx -1.2 4.5 1 0 _2pi) (.fill ctx)
            (.beginPath ctx) (.arc ctx 1.2 4.8 1 0 _2pi) (.fill ctx))
      2 (do (.beginPath ctx) (.arc ctx -1 3.5 1 0 _2pi) (.fill ctx)
            (.beginPath ctx) (.arc ctx 1.5 5.5 1 0 _2pi) (.fill ctx))
      3 (do (.beginPath ctx) (.arc ctx -1.2 4.8 1 0 _2pi) (.fill ctx)
            (.beginPath ctx) (.arc ctx 1.2 4.5 1 0 _2pi) (.fill ctx))
      (do (.beginPath ctx) (.arc ctx -1.2 5 1 0 _2pi) (.fill ctx)
          (.beginPath ctx) (.arc ctx 1.2 5 1 0 _2pi) (.fill ctx)))
    ;; Arms (skin-colored, subtle forward/back swing)
    (set! (.-strokeStyle ctx) skin)
    (set! (.-lineWidth ctx) 1.5)
    (.beginPath ctx)
    (case anim-frame
      0 (do (.moveTo ctx -3.5 1) (.lineTo ctx -5 0)
            (.moveTo ctx 3.5 1)  (.lineTo ctx 5 2.5))
      1 (do (.moveTo ctx -3.5 1) (.lineTo ctx -5 1.5)
            (.moveTo ctx 3.5 1)  (.lineTo ctx 5 1.5))
      2 (do (.moveTo ctx -3.5 1) (.lineTo ctx -5 2.5)
            (.moveTo ctx 3.5 1)  (.lineTo ctx 5 0))
      3 (do (.moveTo ctx -3.5 1) (.lineTo ctx -5 1.5)
            (.moveTo ctx 3.5 1)  (.lineTo ctx 5 1.5))
      (do (.moveTo ctx -3.5 1) (.lineTo ctx -5 2)
          (.moveTo ctx 3.5 1)  (.lineTo ctx 5 2)))
    (.stroke ctx)
    ;; Torso (jersey/team color)
    (set! (.-fillStyle ctx) jersey)
    (.fillRect ctx -3.5 0.5 7 3)
    ;; Head (skin-colored circle)
    (set! (.-fillStyle ctx) skin)
    (.beginPath ctx)
    (.arc ctx 0 -2 3 0 _2pi)
    (.fill ctx)
    ;; Hair on top of head
    (set! (.-fillStyle ctx) hair-color)
    (case hair-style
      :short (do ;; short hair — cap on back half of head
               (.beginPath ctx)
               (.arc ctx 0 -2 3 0.3 (- _2pi 0.3))
               (.fill ctx))
      :long (do ;; long hair — covers more of head + extends past shoulders
              (.beginPath ctx)
              (.arc ctx 0 -2 3.2 0.2 (- _2pi 0.2))
              (.fill ctx)
              (.fillRect ctx -2.5 -1 5 3))
      ;; default to short
      (do (.beginPath ctx)
          (.arc ctx 0 -2 3 0.3 (- _2pi 0.3))
          (.fill ctx)))
    (.restore ctx)))

(defn draw-ball! [ball ctx]
  (if-let [[x y] (:pos ball)]
    (do
      (.beginPath ctx)
      (set! (.-fillStyle ctx) "white")
      (.moveTo ctx x y)
      (.arc ctx x y (:ball-radius gamedata/settings) 0 _2pi)
      (.fill ctx))))


(defn draw-scene! [state]
  (let [{players :players
         drawing-context :drawing-context} state
        ctx (:context drawing-context)
        [pitch-width pitch-height] (:pitch-size gamedata/settings)]
    (set! (.-fillStyle ctx) "green")
    (.fillRect ctx 0 0 pitch-width pitch-height)
    (set! (.-lineWidth ctx) 1)
    (set! (.-strokeStyle ctx) "white")
    (.strokeRect ctx 10 10 (- pitch-width 20) (- pitch-height 20))
    (doseq [p players]
      (draw-player! p state))
    (draw-ball! (:ball state) (:context (:drawing-context state)))))
