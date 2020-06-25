(ns dev.reus.rondo.gamedata)

(defonce settings {:pitch-size [400 400]
                   :player-radius 5
                   :ball-radius 3
                   :distance-to-ball 1
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
               :acceleration [0 0]
               :speed 80
               :turn 30
               :reach 10
               :team :nl
               :nr 4}
              {:name "Vanenburg"
               :pos [320 320]
               :direction [0 -1]
               :velocity [0 0]
               :acceleration [0 0]
               :speed 90
               :turn 90
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
               :turn 30
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
               :turn 30
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
               :turn 30
               :reach 10
               :team :de
               :nr 11
               }
              ])

; (def players [
;               {:name "Van Breukelen"
;                :pos [190 16]
;                :team :nl
;                :nr 1
;                }
;               {:name "R. Koeman"
;                :pos [208 98]
;                :team :nl
;                :nr 4
;                }
;               {:name "Van Aerle"
;                :pos [51 122]
;                :team :nl
;                :nr 6
;                }
;               {:name "Rijkaard"
;                :pos [274 147]
;                :team :nl
;                :nr 17
;                }
;               {:name "Van Tiggelen"
;                :pos [196 170]
;                :team :nl
;                :nr 2
;                }
;               {:name "Wouters"
;                :pos [138 129]
;                :team :nl
;                :nr 20
;                }
;               {:name "Mühren"
;                :pos [312 295]
;                :team :nl
;                :nr 8
;                }
;               {:name "Vanenburg"
;                :pos [100 235]
;                :team :nl
;                :nr 7
;                }
;               {:name "E. Koeman"
;                :pos [221 284]
;                :team :nl
;                :nr 13
;                }
;               {:name "Gullit"
;                :pos [160 286]
;                :team :nl
;                :nr 10
;                }
;               {:name "Van Basten"
;                :pos [136 332]
;                :team :nl
;                :nr 12
;                }
;               {:name "Immel"
;                :pos [207 525]
;                :team :de
;                :nr 1
;                }
;               {:name "Herget"
;                :pos [242 409]
;                :team :de
;                :nr 5
;                }
;               {:name "Brehme"
;                :pos [306 375]
;                :team :de
;                :nr 3
;                }
;               {:name "Kohler"
;                :pos [203 349]
;                :team :de
;                :nr 4
;                }
;               {:name "Borowka"
;                :pos [134 346]
;                :team :de
;                :nr 6
;                }
;               {:name "Matthäus"
;                :pos [145 259]
;                :team :de
;                :nr 8
;                }
;               {:name "Thon"
;                :pos [196 190]
;                :team :de
;                :nr 10
;                }
;               {:name "Rolff"
;                :pos [75 353]
;                :team :de
;                :nr 20
;                }
;               {:name "Mill"
;                :pos [58 159]
;                :team :de
;                :nr 11
;                }
;               {:name "Klinsmann"
;                :pos [138 154]
;                :team :de
;                :nr 18
;                }
;               {:name "Völler"
;                :pos [273 164]
;                :team :de
;                :nr 9
;                }
;               ])
