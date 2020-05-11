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
  (ui/update-fps! state)
  (canvas2d/draw-scene! state))

(defn update-time [{:keys [start-time current-time fps-time frame] :as state}]
  "Update the time and fps of the game"
  (let [next-millis (.now js/Date)
        dmillis (- next-millis current-time)
        next-frame (inc frame)
        time-since-fps (- next-millis fps-time)
        new-state (assoc state :current-time next-millis
                         :frame-time 0.001 * dmillis
                         :frame next-frame)]
    (if (> time-since-fps 1000)
      (assoc new-state :frame 1 :fps-time next-millis
             :fps (int (/ 1000 (/ time-since-fps frame))))
      new-state)))

(defn process-input [state]
  "Update a player's direction and acceleration based
   on the (arrow) keys pressed."
  (if-let [p (:selected-player state)]
    (let [max-acc (get-in state [:players p :max-acc])
          dir (get model/directions (get-in state [:players p :rotation]))
          dt (:frame-trime state)
          [rot forward] (:keys-pressed state)]
      (-> state
          (update-in [:players p :rotation] #(mod (+ % %2) 8) rot)
          (assoc-in [:players p :acceleration] (mapv #(* (* forward 10000) %) dir))
          (assoc-in [:keys-pressed 0] 0)))
    state))

(defn step [state]
  (-> state
      (update-time)
      (process-input)
      (model/move-players)))

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
    :select-player (assoc state :selected-player (:index evt) :selected-team nil)
    :select-team (assoc state :selected-team (:team-id evt) :selected-player nil)
    :click (let [event (:event evt)
                 ps (:players state)
                 canvas (.-target event)
                 rect (.getBoundingClientRect canvas)
                 x (- (.-clientX event) (.-left rect))
                 y (- (.-clientY event) (.-top rect))
                 mouse-selected-player (model/point-in-players? [x y] (:players state))]
             (if mouse-selected-player
               (assoc state :selected-player mouse-selected-player :selected-team nil)
               (assoc state :selected-player nil :selected-team nil)))
    :keyup (let [event (:event evt)]
             (case event.key
               "ArrowLeft" (assoc-in state [:keys-pressed 0] 0)
               "ArrowRight" (assoc-in state [:keys-pressed 0] 0)
               "ArrowUp" (assoc-in state [:keys-pressed 1] 0)
               "ArrowDown" (assoc-in state [:keys-pressed 1] 0)
               state))

    :keydown (let [event (:event evt)]
               (case event.key
                 "p" (do
                       (if-let [sel (:selected-player state)]
                         (println (get (:players state) sel))
                         (println (:players state)))
                       state)
                 "s" (do
                       (println state)
                       state)
                 "ArrowLeft" (assoc-in state [:keys-pressed 0] -1)
                 "ArrowRight" (assoc-in state [:keys-pressed 0] 1)
                 "ArrowUp" (assoc-in state [:keys-pressed 1] 1)
                 "ArrowDown" (assoc-in state [:keys-pressed 1] -1)
                 state))
    :random-position (let [i (:index evt)]
                      (println i)
                      state)
    :reset-position (let [i (:index evt)]
                      (println i)
                      state)
    :ui-state-changed @ui/ui-state
    state))


(defn game-loop [state ui-channel player-channel]
  "Start the game loop. Starts a go loop that listens for three channels:
   * refresh channel: redraws the game world.
   * ui channel: listens for input from user.
   * player channel: listen for input from players.
   Returns the channel through which players can communicate."
  (let [rate (:refresh-rate state)]
    (render! state)
    (go (loop [refresh (timeout rate) s state]
          (let [[v c] (alts! [refresh ui-channel player-channel] :priority true)]
            (condp = c
              ui-channel (let [new-state (process-ui s v)]
                           (recur refresh (reset! ui/ui-state new-state)))
              refresh (let [new-state (step s)]
                        (render! new-state)
                        (recur (timeout rate) (reset! ui/ui-state new-state)))
              player-channel (let [new-state v]
                               ;(.postMessage worker v)
                               (recur refresh (reset! ui/ui-state new-state)))))))))

(defn start-game []
  "Start the game.
   * Initialize game state
   * Set up user interface
   * Run the webworker script
   * Start the game loop and binds the webworker code to the player channel."
  (let [state (model/init-state)
        ui-channel (ui/setup-ui state)
        player-channel (setup-worker)]
    (game-loop state ui-channel player-channel)))

(defn on-js-reload []
  "Figwheel convenience function.")

(defonce game (start-game))
