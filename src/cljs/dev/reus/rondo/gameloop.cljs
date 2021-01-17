(ns dev.reus.rondo.gameloop
  (:require [dev.reus.rondo.canvas2d :as canvas2d]
            [dev.reus.rondo.ui2 :as ui]
            [dev.reus.rondo.model2 :as model]
            [cljs.core.async :as async :refer [<! >! chan timeout]]
            [cljs.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defn init-state []
  "Create the initial state hashmap."
  (let [players (model/init-players)
        teams (model/init-teams)
        drawing-context (canvas2d/init-drawing-context)
        initial-time (.now js/Date)]
    {:refresh-rate 20
     :frame 1
     :time initial-time
     :frame-time 0
     :fps-info {:frame 1
                :time initial-time
                :fps 0}
     :drawing-context drawing-context
     :ball {
            :player 1
            :ke 0 ;; kinetic energy
            :velocity nil
            :direction nil
            :pos nil
            }
     :players players
     :teams teams}))

(defn update-fps [state]
  "update fps info in state"
  (let [fps-info (:fps-info state)
        current-frame (:frame state)
        previous-frame (:frame fps-info)
        current-time (:time state)
        previous-time (:time fps-info)
        fps (* 1000 (/ (- current-frame previous-frame)
                       (- current-time previous-time)))]
    (assoc state :fps-info {:frame current-frame
                            :time current-time
                            :fps fps })))

(defn update-time [state]
  "update time and frame count in state"
  (let [t (.now js/Date)
        previous-time (:time state)
        frame-time (- t previous-time)]
    (-> state
        (assoc :time t :frame-time frame-time)
        (update :frame inc))))

(defn step [state]
  "take a step in state updates"
  (-> state
      update-time
      model/update-players
      model/check-player-collisions
      model/update-ball))


(defn setup-worker []
  "Create a webworker object and a player channel through which the worker
   can communicate."
  (let [worker (js/Worker. "/worker.js")
        player-channel (chan 100)]
    (set! (.-onmessage worker) (fn [msg]
                                 (go
                                   (<! (timeout (1000)))
                                   (>! player-channel msg.data))))
                                        ;(.postMessage worker state)
    player-channel))

(defn render! [state]
  "Render all game objects to the drawing context."
  (canvas2d/draw-scene! state))

(defn process-ui [state e]
  (case (:type e)
    :reset-position (model/reset-position state (:index e))
    :random-position (model/random-position state (:index e))
    :print-state (do
                   (pprint state)
                   state)
    :print-ui-state (do
                      (pprint @ui/ui-state)
                      state)
    :print-player-info (let [player (get-in state [:players (:index e)])]
                         (pprint player)
                         state)
    :click-canvas (let [event (:event e)
                        canvas (.-target event)
                        rect (.getBoundingClientRect canvas)
                        x (- (.-clientX event) (.-left rect))
                        y (- (.-clientY event) (.-top rect))
                        mouse-selected-player (model/point-in-players? [x y] (:players state))]
                    (if mouse-selected-player
                      (do
                        (swap! ui/ui-state assoc :selected-player mouse-selected-player :selected-team nil)
                        state)
                      (if-let [selected-player (:selected-player @ui/ui-state)]
                        (assoc-in state [:players selected-player :goal] {:status :move-destination :destination [x y]})
                        state)))
    :goal (update-in state [:players (:index e)] #(assoc % :goal (:goal e)))
    state))

(defn game-loop [state ui-channel player-channel]
  (let [rate (:refresh-rate state)]
    (render! state)
    (go (loop [refresh (timeout rate)
               fps-update (timeout 1000)
               s state]
          (let [[v c] (alts! [refresh
                              fps-update
                              ui-channel
                              player-channel] :priority true)]
            (condp = c
              refresh (let [new-state (step s)]
                        (render! new-state)
                        (recur (timeout rate) fps-update new-state))
              fps-update (let [new-state (update-fps s)
                               fps (get-in new-state [:fps-info :fps])]
                           (ui/update-fps! fps)
                           (recur refresh (timeout 1000) new-state))
              ui-channel (let [new-state (process-ui s v)]
                           (recur refresh fps-update new-state))
              player-channel (let [new-state v]
                               ;(.postMessage worker v)
                               (recur refresh fps-update new-state))))))))
(defn start-game []
  "Start the game.
   * Initialize game state
   * Set up user interface and receive channel
   * Run the webworker script and receive channel
   * Start the game loop and pass the channels"
  (let [state (init-state)
        ui-channel (ui/setup-ui state)
        player-channel (setup-worker)]
    (game-loop state ui-channel player-channel)))

(defonce game (start-game))
