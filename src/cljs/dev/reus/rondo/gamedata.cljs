(ns dev.reus.rondo.gamedata)

(defonce settings {:pitch-size [400 400]
                   :player-radius 5
                   :ball-radius 3
                   :max-angle-pickup-ball 0.79 ;in radians, about 45 degrees
                   :distance-to-ball 1
                   :distance-to-side 10
                   :factor-ke-loss-on-collision 0.3
                   })

(defonce teams
           [{:name "Germany"
             :id :de
             :color [0.9 0.9 0.9]}
            {:name "Holland"
             :id :nl
             :color [1.0 0.5 0.0]}])

(defonce players [
              {:name "R. Koeman"
               :pos [190 36]
               :direction [0 1]
               :velocity [0 0]
               :acceleration [0 1]
               :speed 80
               :turn 10
               :reach 10
               :team :nl
               :nr 4}
              {:name "Vanenburg"
               :pos [320 320]
               :direction [0 -1]
               :velocity [0 0]
               :acceleration [0 0]
               :speed 90
               :turn 15
               :reach 10
               :team :nl
               :nr 7
               }
              {:name "Van Aerle"
               :pos [44 122]
               :direction [0.7 0.7]
               :velocity [0 0]
               :acceleration [0 0]
               :speed 120
               :turn 10
               :reach 10
               :team :nl
               :nr 6
               }
              {:name "Rolff"
               :pos [271 203]
               :direction [0 1]
               :velocity [0 0]
               :acceleration [0 0]
               :speed 110
               :turn 10
               :reach 10
               :team :de
               :nr 20
               }
              {:name "Mill"
               :pos [58 159]
               :direction [0.7 0.7]
               :velocity [0 0]
               :acceleration [0 0]
               :speed 100
               :turn 10
               :reach 10
               :team :de
               :nr 11
               }
              ])
