(ns dev.reus.rondo-worker.core)

(defn process-state [message]
  (println "received message in worker" message.data)
  (js/setTimeout #(.postMessage js/self #js [10 9 8 7 6]) 3000))

(set! (.-onmessage js/self) process-state)
