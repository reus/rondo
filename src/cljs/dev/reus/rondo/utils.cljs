(ns dev.reus.rondo.utils)

(defn by-id [id]
  (.getElementById js/document id))
