(ns dev.reus.rondo.app
  (:require [dev.reus.rondo.utils :refer [by-id]]
            [dev.reus.rondo.canvas2d]
            [dev.reus.rondo.gamedata]
            [cljs.pprint :refer [pprint]]
            [reagent.core :as reagent]
            [cljs.core.async :as async :refer [<! >! chan put! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defn print-player-info! [player]
  (let [p (select-keys player [:name :pos])]
    (pprint p)))

(defn find-color [team]
  (loop [i 0]
    (if-let [t (get dev.reus.rondo.gamedata/teams i)]
      (if (= (:id t) team)
        (:color t)
        (recur (inc i)))
      [0 0 0])))

(defn init-players [players]
  (loop [i 0 p-ext []]
    (if-let [player (get players i)]
      (let [team (:team player)
            color (find-color team)
            [r g b] (map #(* 256 %) color)
            color-str (str "rgb(" r "," g "," b ")")]
        (recur (inc i) (conj p-ext (assoc player :index i
                                          :color color
                                          :color-str color-str))))
      p-ext)))

(defn init-state []
  (let [players dev.reus.rondo.gamedata/players
        ;webgl (dev.reus.rondo.webgl/setup-webgl (count players))]
        drawing-context (dev.reus.rondo.canvas2d/init-drawing-context)]
    {:refresh-rate 20
     :start-millis (.now js/Date)
     :current-millis (.now js/Date)
     :frame-millis 0
     :frame 1
     :drawing-context drawing-context
     :selected-team nil
     :selected-player nil
     :selected-player-color [0.1 0.2 1.0]
     :player-with-ball 0
     :players (init-players players)}))

(defn render! [state]
  ;(dev.reus.rondo.webgl/draw-scene! state))
  (dev.reus.rondo.canvas2d/draw-scene! state))

(defn step [s]
  (let [current-millis (.now js/Date)
        dmillis (- current-millis (:current-millis s))
        frame (:frame s)
        new-state (assoc s :current-millis current-millis
                         :frame-millis dmillis
                         :frame (inc frame))]
    new-state))

(defn print-state [state]
  (let [players (:players state)]
    (doseq [player players]
      (print-player-info! player))
    state))

(defn dot-product [v1 v2]
  (apply + (map * v1 v2)))

(defn point-in-player? [[x y] state]
  (let [player-size (:player-size dev.reus.rondo.gamedata/settings)
        ab [(* 2 player-size) 0]
        bc [0 (* 2 player-size)]]
    (loop [i 0]
      (if-let [player (get (:players state) i)]
        (let [[x-player y-player] (:pos player)
              pa [(- x-player player-size) (- y-player player-size)]
              pb [(+ x-player player-size) (- y-player player-size)]
              am (map - [x y] pa)
              bm (map - [x y] pb)]
          (if (and (<= 0 (dot-product ab am))
                   (<= (dot-product ab am) (dot-product ab ab))
                   (<= 0 (dot-product bc bm))
                   (<= (dot-product bc bm) (dot-product bc bc)))
            i
            (recur (inc i))))
        false))))

(defn reset-positions [state]
  state)

(defn reset-position-selected-player [state]
  (let [index (:selected-player state)]
    (assoc-in state [:players index :pos] (:pos (get dev.reus.rondo.gamedata/players index)))))

(defn random-position-selected-player [state]
  (let [index (:selected-player state)]
    (loop []
      (let [x (+ 5 (rand-int 391))
            y (+ 5 (rand-int 391))
            p1 [(- x 5) (- y 5)]
            p2 [(+ x 5) (- y 5)]
            p3 [(+ x 5) (+ y 5)]
            p4 [(- x 5) (+ y 5)]
            ts (remove #(or (= % index) (false? %)) (map #(point-in-player? %1 state) [p1 p2 p3 p4]))]
        (if (pos? (count ts))
          (recur)
          (assoc-in state [:players index :pos] [x y]))))))

(defn randomize-positions [state]
  (let [players (loop [i 0 ps (:players state) positions #{}]
                  (if-let [p (get ps i)]
                    (let [x (+ 5 (* 10 (rand-int 40)))
                          y (+ 5 (* 10 (rand-int 56)))]
                      (if (contains? positions [x y])
                        (recur i ps positions)
                        (recur (inc i) (assoc-in ps [i :pos] [x y]) (conj positions [x y]))))
                    ps))]
    (assoc state :players players)))

(defn start-game-loop [state ui-channel r-atom worker] 
  (let [player-channel (chan 100)
        rate (:refresh-rate state)]
    (render! state)
    (go (loop [refresh (timeout rate) s state]
          (let [[v c] (alts! [refresh ui-channel player-channel])]
            (condp = c
              refresh (let [new-state (step s)]
                        (render! new-state)
                        (recur (timeout rate) new-state))
              ui-channel (let [new-state (v s)]
                           (reset! r-atom new-state)
                           (recur refresh new-state))
              player-channel (let [new-state v]
                               (.postMessage worker v)
                               (recur refresh new-state))))))
    player-channel))

(defn ui-select-team [state team]
    (assoc state :selected-team team :selected-player nil))

(defn ui-select-player [state player-index]
    (assoc state :selected-team nil :selected-player player-index))

(defn ui-player [state ui-chan]
  (let [player-index (:selected-player state)
        player (get (:players state) player-index)
        return (fn [f] (fn [] (put! ui-chan f)))]
    [:div
     [:a {:on-click (return (fn [s] (assoc s :selected-player nil)))} "Back"]
     [:div "name: " (:name player)]
     [:div [:input {:type "button" :value "Reset position" :on-click (return (fn [s] (reset-position-selected-player s)))}]]
     [:div [:input {:type "button" :value "Random position" :on-click (return (fn [s] (random-position-selected-player s)))}]]
     ]))

(defn ui-team [state ui-chan]
  (let [return (fn [f] (fn [] (put! ui-chan f)))]
    [:div
     [:a {:on-click (return (fn [s] (assoc s :selected-team nil)))} "Back"]]))

(defn draw-ui [state-atom ui-chan]
  (let [return (fn [f] (fn [] (put! ui-chan f)))
        state @state-atom]
    [:div
     [:div.teams
      (for [team dev.reus.rondo.gamedata/teams]
        ^{:key team} [:ul [:li {:class (if (= (:id team)
                                              (:selected-team state))
                                         "selected team"
                                         "team")
                                :on-click (return (fn [s] (ui-select-team s (:id team))))}
                           (:name team)]
                      (for [player (filter (comp #{(:id team)} :team) (:players state))]
                        ^{:key player} [:li {:class (if (= (:index player)
                                                           (:selected-player state))
                                                      "selected player"
                                                      "player")
                                             :on-click (return
                                                         (fn [s]
                                                           (ui-select-player s (:index player))))}
                                        [:span {:class "nr"} (:nr player)]
                                        [:span {:class "name"} (:name player)]])])]
     [:div.settings
        (if (:selected-player state)
          (ui-player state ui-chan)
          (if (:selected-team state) 
            (ui-team state ui-chan)
            "General settings"))]]))

(defn process-mousedown [state event]
  (let [canvas (.-target event)
        rect (.getBoundingClientRect canvas)
        x (- (.-clientX event) (.-left rect))
        y (- (.-clientY event) (.-top rect))
        mouse-selected-player (point-in-player? [x y] state)
        selected-player (get state :selected-player)]
    (if mouse-selected-player
      (if (= mouse-selected-player selected-player)
        (assoc state :selected-team nil :selected-player nil)
        (assoc state :selected-team nil :selected-player mouse-selected-player))
      state)))

(defn process-keydown [state event]
  (.log js/console "test")
  state)

(defn setup-canvas-handlers [ui-chan]
  (let [canvas (by-id "canvas")]
    (.addEventListener canvas "mousedown" (fn [e] (put! ui-chan (fn [s] (process-mousedown s e)))))))

(defn setup-key-handlers [ui-chan]
  (.addEventListener js/document "keydown" (fn [e] (put! ui-chan (fn [s] (process-keydown s e))))))

(defn setup-ui [state]
  (let [ui-chan (chan)
        reagent-state (reagent/atom state)
        ui (fn [] (draw-ui reagent-state ui-chan))]
    (go (loop []
          (<! (timeout 1000))
          (>! ui-chan identity)
          (recur)))
    (setup-canvas-handlers ui-chan)
    (setup-key-handlers ui-chan)
    (reagent/render [ui] (by-id "ui"))
    [ui-chan reagent-state]))

(defn start-game []
  (let [state (init-state)
        [ui-channel r-atom] (setup-ui state)
        worker (js/Worker. "/worker.js")
        player-channel (start-game-loop state ui-channel r-atom worker)]
    (set! (.-onmessage worker) (fn [msg]
                                 (go
                                   (<! (timeout (10)))
                                   (>! player-channel msg.data))))
    ;(.postMessage worker state)
    1))

(defonce game (start-game))

(defn on-js-reload [])
