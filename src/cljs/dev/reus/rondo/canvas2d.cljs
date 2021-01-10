(ns dev.reus.rondo.canvas2d
  (:require [dev.reus.rondo.gamedata :as gamedata]
            [dev.reus.rondo.ui2 :as ui]
            [dev.reus.rondo.utils :refer [by-id]]))

(defonce pi (.-PI js/Math))
(defonce _2pi (* 2 (.-PI js/Math)))

(defn init-drawing-context []
  (let [canvas (by-id "canvas")
        context (.getContext canvas "2d")]
    {:context context}))

(defn draw-player! [p state]
  (let [[x y] (:pos p)
        player-radius (:player-radius gamedata/settings)
        ball-radius (:ball-radius gamedata/settings)
        [xx yy] (map #(* % (:reach p)) (:direction p))
        [xxx yyy] (map #(* % (+ player-radius ball-radius (:distance-to-ball gamedata/settings))) (:direction p))
        selected-player (get @ui/ui-state :selected-player)
        ctx (:context (:drawing-context state))]
    (set! (.-fillStyle ctx) (:color-str p))
    (set! (.-lineWidth ctx) 1)
    (set! (.-strokeStyle ctx) (:color-str p))
    (set! (.-imageSmoothingEnabled ctx) false)
    (.beginPath ctx)
    (.arc ctx x y player-radius 0 _2pi)
    (.fill ctx)
    (.beginPath ctx)
    (if (not= (:player (:ball state)) (:index p))
      (do
        (.moveTo ctx x y)
        (.lineTo ctx (+ x xx) (+ y yy))
        (.stroke ctx)))
    (when (= (:index p) selected-player)
      (.moveTo ctx x y)
      (.beginPath ctx)
      (.arc ctx x y player-radius 0 _2pi)
      (set! (.-lineWidth ctx) 2)
      (set! (.-strokeStyle ctx) "black")
      (.stroke ctx))
    ))

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
