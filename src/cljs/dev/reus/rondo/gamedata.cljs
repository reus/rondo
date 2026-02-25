(ns dev.reus.rondo.gamedata)

(def settings {:pitch-size [400 400]
               :player-radius 5
               :ball-radius 3
               :max-angle-pickup-ball 0.79 ;in radians, about 45 degrees
               :distance-to-ball 1
               :distance-to-side 10
               :factor-ke-loss-on-collision 0.3})

(def teams [{:name "Germany"
             :id :de
             :color [0.9 0.9 0.9]}
            {:name "Holland"
             :id :nl
             :color [1.0 0.5 0.0]}])

(def players [{:name "R. Koeman"
               :pos [190 36]
               :direction [0 1]
               :velocity [0 0]
               :acceleration [0 1]
               :speed 80
               :team :nl
               :nr 4
               :skin [0.96 0.80 0.69]
               :hair-color [0.85 0.75 0.45]
               :hair :short}
              {:name "Vanenburg"
               :pos [320 320]
               :direction [0 -1]
               :velocity [0 0]
               :acceleration [0 0]
               :speed 90
               :team :nl
               :nr 7
               :skin [0.96 0.80 0.69]
               :hair-color [0.20 0.13 0.08]
               :hair :long}
              {:name "Van Aerle"
               :pos [44 122]
               :direction [0.7 0.7]
               :velocity [0 0]
               :acceleration [0 0]
               :speed 120
               :team :nl
               :nr 6
               :skin [0.87 0.72 0.53]
               :hair-color [0.20 0.13 0.08]
               :hair :short}
              {:name "Rolff"
               :pos [271 203]
               :direction [0 1]
               :velocity [0 0]
               :acceleration [0 0]
               :speed 110
               :team :de
               :nr 20
               :skin [0.96 0.80 0.69]
               :hair-color [0.85 0.75 0.45]
               :hair :short}
              {:name "Mill"
               :pos [58 159]
               :direction [0.7 0.7]
               :velocity [0 0]
               :acceleration [0 0]
               :speed 100
               :team :de
               :nr 11
               :skin [0.55 0.36 0.24]
               :hair-color [0.10 0.07 0.04]
               :hair :short}])
