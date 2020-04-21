(ns dev.reus.rondo.canvas2d
  (:require [dev.reus.rondo.gamedata :as gamedata]
            [dev.reus.rondo.utils :refer [by-id]]))

(defonce pi (.-PI js/Math))
(defonce _2pi (* 2 (.-PI js/Math)))
(defonce _3pi (* 3 (.-PI js/Math)))
(defonce _5pi (* 5 (.-PI js/Math)))
(defonce _7pi (* 7 (.-PI js/Math)))

(defonce rotations [
                    (/ _3pi 2)
                    (/ _7pi 4)
                    0
                    (/ pi 4)
                    (/ pi 2)
                    (/ _3pi 4)
                    pi
                    (/ _5pi 4)
                    ])

(defn init-drawing-context []
  (let [canvas (by-id "canvas")
        context (.getContext canvas "2d")]
    {:context context}))

(defn draw-player! [p state]
  (let [[x y] (:pos p)
        p-size (:player-size gamedata/settings)
        rot (get rotations (:rotation p))
        ctx (:context (:drawing-context state))]
    (set! (.-fillStyle ctx) (:color-str p))
    (set! (.-lineWidth ctx) 1)
    (set! (.-strokeStyle ctx) (:color-str p))
    (set! (.-imageSmoothingEnabled ctx) false)
    (.beginPath ctx)
    (.arc ctx x y p-size 0 _2pi)
    (.fill ctx)
    (.moveTo ctx x y)
    (.lineTo ctx (+ x (* 2 p-size (.cos js/Math rot))) (+ y (* 2 p-size (.sin js/Math rot))))
    (.stroke ctx)
    (when (= (:index p) (:selected-player state))
      (.moveTo ctx x y)
      (.beginPath ctx)
      (.arc ctx x y p-size rot (+ rot _2pi))
      (set! (.-lineWidth ctx) 2)
      (set! (.-strokeStyle ctx) "black")
      (.stroke ctx))
    ))

(defn draw-scene! [state]
  (let [{players :players
         drawing-context :drawing-context} state]
    (let [ctx (:context drawing-context)
          [pitch-width pitch-height] (:pitch-size dev.reus.rondo.gamedata/settings)]
      (set! (.-fillStyle ctx) "green")
      (.fillRect ctx 0 0 pitch-width pitch-height)
      (doseq [p players]
        (draw-player! p state)))))

