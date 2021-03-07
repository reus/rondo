(ns dev.reus.rondo.model)

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

(defn add [v1 v2]
  (mapv + v1 v2))

(defn subtract [v1 v2]
  (mapv - v1 v2))

(defn add-scaled [v1 v2 k]
  (mapv + v1 (map #(* k %) v2)))

(defn distance [x y]
  "Give the distance between two coordinates."
  (magnitude (subtract x y)))

(defn angle [v1 v2]
  (.acos js/Math (/ (dot-product v1 v2) (* (magnitude v1) (magnitude v2)))))

(defn rotate [[x1 y1] a]
  (let [x2 (- (* (.cos js/Math a) x1) (* (.sin js/Math a) y1))
        y2 (+ (* (.sin js/Math a) x1) (* (.cos js/Math a) y1))]
    [x2 y2]))

(defn scale-by [v k]
  "scale a vector by k"
  (mapv #(* % k) v))

(defn projection [v1 v2]
  (let [length1 (magnitude v1)
        length2 (magnitude v2)]
    (if (some #{0} (list length1 length2))
      0
      (/ (dot-product v1 v2) length2))))

(defn para [v u]
  (let [length (magnitude v)]
    (scale-by v (/ u length))))

(defn project [v1 v2]
  (para v2 (projection v1 v2)))

(defn proj [u v] ; projection of v onto u
  (scale-by u (/ (dot-product v u)
                 (dot-product u u))))

(defn distance-circles [p1 r1 p2 r2]
  (- (distance p1 p2) (+ r1 r2)))

(defn circles-overlap? [p1 r1 p2 r2]
  (< (distance-circles p1 r1 p2 r2) 0))
