(ns dev.reus.rondo.canvas2d
  (:require [dev.reus.rondo.gamedata]
            [dev.reus.rondo.utils :refer [by-id]]))

(defonce pi (.-PI js/Math))
(defonce _2pi (* 2 (.-PI js/Math)))
(defonce deg2rad (/ (.-PI js/Math) 180))
(defonce cor (/ (.-PI js/Math) 45))

(defn init-drawing-context []
  (let [canvas (by-id "canvas")
        context (.getContext canvas "2d")]
    {:context context}))

(defn draw-player! [p state]
  (let [[x y] (:pos p)
        ctx (:context (:drawing-context state))]
    (set! (.-fillStyle ctx) (:color-str p))
    (.beginPath ctx)
    (.arc ctx x y (:player-size dev.reus.rondo.gamedata/settings) 0 _2pi)
    (set! (.-lineWidth ctx) 0)
    (.fill ctx)
    (.closePath ctx)
    (.arc ctx x y (:player-size dev.reus.rondo.gamedata/settings) 0 (* (- (:rotation p) 90) deg2rad))
    (when (= (:index p) (:selected-player (:ui-state state)))
      (set! (.-strokeStyle ctx) "black")
      (set! (.-lineWidth ctx) 2)
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

