(ns dev.reus.rondo-worker.core
  (:require [dev.reus.rondo.math :as math]))

(defn process-state [message]
  (println "received message in worker")
  (let [view (js/Uint16Array. message.data)]
    (println (aget view 0))
    (println (.-length view))
    (js/setTimeout #(.postMessage js/self #js [10 9 8 7 6]) 3000)))

(set! (.-onmessage js/self) process-state)

(.postMessage js/self 0)

(println (math/distance [0 0] [5 3]))
