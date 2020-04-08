(ns dev.reus.rondo.app
  (:require [dev.reus.rondo.canvas2d :as canvas2d]
            [dev.reus.rondo.ui :as ui]
            [dev.reus.rondo.model :as model]
            [cljs.core.async :as async :refer [<! >! chan timeout]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defn render! [state]
  "Render all game objects to the drawing context."
  ;(dev.reus.rondo.webgl/draw-scene! state))
  (canvas2d/draw-scene! state))

(defn step [s]
  "Take a step in the game heartbeat. Increments the framecount, and updates the milliseconds."
  (let [current-millis (.now js/Date)
        dmillis (- current-millis (:current-millis s))
        frame (:frame s)
        new-state (assoc s :current-millis current-millis
                         :frame-millis dmillis
                         :frame (inc frame))]
    new-state))

(defn setup-worker []
  "Create a webworker object and a player channel with which the worker
   can communicate."
  (let [worker (js/Worker. "/worker.js")
        player-channel (chan 100)]
    (set! (.-onmessage worker) (fn [msg]
                                 (go
                                   (<! (timeout (10)))
                                   (>! player-channel msg.data))))
                                        ;(.postMessage worker state)
    player-channel))


(defn process-ui [state evt]
  "With the event just received from the ui channel, process the event
   and update the state when necessary."
  (case (:type evt)
    :click (let [event (:event evt)
                 ps (:players state)
                 canvas (.-target event)
                 rect (.getBoundingClientRect canvas)
                 x (- (.-clientX event) (.-left rect))
                 y (- (.-clientY event) (.-top rect))
                 mouse-selected-player (model/point-in-players? [x y] (:players state))]
             (if mouse-selected-player
               (swap! ui/ui-state assoc :selected-player mouse-selected-player :selected-team nil)
               (swap! ui/ui-state assoc :selected-player nil :selected-team nil))
             (assoc state :ui-state @ui/ui-state))
    (assoc state :ui-state @ui/ui-state)))

(defn game-loop [state ui-channel player-channel]
  "Start the game loop. Starts a go loop that listens for three channels:
   * refresh channel: redraws the game world.
   * ui channel: listens for input from user.
   * player channel: listen for input from players.
   Returns the channel through which players can communicate."
  (let [rate (:refresh-rate state)]
    (render! state)
    (go (loop [refresh (timeout rate) s state]
          (let [[v c] (alts! [refresh ui-channel player-channel])]
            (condp = c
              refresh (let [new-state (step s)]
                        (render! new-state)
                        (recur (timeout rate) new-state))
              ui-channel (let [new-state (process-ui s v)]
                           (recur refresh new-state))
              player-channel (let [new-state v]
                               ;(.postMessage worker v)
                               (recur refresh new-state))))))))

(defn start-game []
  "Start the game.
   * Initialize game state
   * Set up user interface
   * Run the webworker script
   * Start the game loop and binds the webworker code to the player channel."
  (let [state (model/init-state)
        ui-channel (ui/setup-ui)
        player-channel (setup-worker)]
    (game-loop state ui-channel player-channel)))

(defn on-js-reload []
  "Figwheel convenience function.")

(defonce game (start-game))

(go (loop [refresh (timeout rate) s state]
      (let [[v c] (alts! [refresh ui-channel])]
        (condp = c
          refresh (let [new-state (step s)]
                    (render! new-state)
                    (recur (timeout rate) new-state))
          ui-channel (let [new-state (process-ui s v)]
                       (recur refresh new-state))))))
