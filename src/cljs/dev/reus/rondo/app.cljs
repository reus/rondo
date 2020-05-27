(ns dev.reus.rondo.app
  (:require [dev.reus.rondo.canvas2d :as canvas2d]
            [dev.reus.rondo.ui :as ui]
            [dev.reus.rondo.model :as model]
            [dev.reus.rondo.gamedata :as gamedata]
            [cljs.core.async :as async :refer [<! >! chan timeout]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defonce map-keys-to-direction {
                     [0 0] [0 0]
                     [0 -1] [0 -1]
                     [1 -1] [0.7 -0.7]
                     [1 0] [1 0]
                     [1 1] [0.7 0.7]
                     [0 1] [0 1]
                     [-1 1] [-0.7 0.7]
                     [-1 0] [-1 0]
                     [-1 -1] [-0.7 -0.7]})

(defn find-color [team]
  "Determine the shirt-color of a team. Use a keyword."
  (loop [i 0]
    (if-let [t (get gamedata/teams i)]
      (if (= (:id t) team)
        (:color t)
        (recur (inc i)))
      [0 0 0])))

(defn init-players [players]
  "Initialize player vector. Adds different game related
   properties to each player hashmap."
  (loop [i 0 p-ext []]
    (if-let [player (get players i)]
      (let [team (:team player)
            color (find-color team)
            [r g b] (map #(* 256 %) color)
            color-str (str "rgb(" r "," g "," b ")")]
        (recur (inc i) (conj p-ext (assoc player
                                          :index i
                                          :color color
                                          :color-str color-str))))
      p-ext)))

(defn init-state []
  "Create the initial state hashmap."
  (let [players (init-players gamedata/players)
                ;webgl (dev.reus.rondo.webgl/setup-webgl (count players))]
        drawing-context (canvas2d/init-drawing-context)]
    {:refresh-rate 20
     :start-time (.now js/Date)
     :current-time (.now js/Date)
     :frame-time 0
     :fps-time (.now js/Date)
     :frame 1
     :fps 0
     :drawing-context drawing-context
     :ball {
            :player 3
            :acc nil
            :speed nil
            :position nil
            }
     :players players
     :teams gamedata/teams}))

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
                         :frame-time (* 0.001 dmillis)
                         :frame next-frame)]
    (if (> time-since-fps 1000)
      (do
        (swap! ui/ui-state assoc :fps (int (/ 1000 (/ time-since-fps frame))))
        (assoc new-state :frame 1 :fps-time next-millis))
      new-state)))

(defn turn [x y t]
  (nth ({[0 -1] [[0 -1] [0.7 -0.7] [-0.7 -0.7]]
         [0.7 -0.7] [[0.7 -0.7] [1 0] [0 -1]]
         [1 0] [[1 0] [0.7 0.7] [0.7 -0.7]]
         [0.7 0.7] [[0.7 0.7] [0 1] [1 0]]
         [0 1] [[0 1] [-0.7 0.7] [0.7 0.7]]
         [-0.7 0.7] [[-0.7 0.7] [-1 0] [0 1]]
         [-1 0] [[-1 0] [-0.7 -0.7] [-0.7 0.7]]
         [-0.7 -0.7] [[-0.7 -0.7] [0 -1] [-1 0]]} [x y]) t))

(defn process-selected-player-keys [state]
  "If a player is selected, update it's direction and velocity based
   on the (arrow) keys pressed."
  (if-let [p (:selected-player @ui/ui-state)]
    (let [max-acc (get-in state [:players p :max-acc])
          [x y run t] @ui/keys-pressed
          [old-x old-y] (get-in state [:players p :direction])
          dir (get map-keys-to-direction [x y])
          dt (:frame-trime state)]
      (swap! ui/keys-pressed assoc 3 0)
      (-> state
          (update-in [:players p :direction] #(if (= [0 0] dir) (turn old-x old-y t) dir))
          (assoc-in [:players p :velocity] (mapv #(* run max-acc %) dir))))
    state))

(defn step [state]
  (-> state
      (update-time)
      (process-selected-player-keys)
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
             state)
    :random-position (let [i (:index evt)]
                      (println i)
                      state)
    :reset-position (let [i (:index evt)]
                      (println i)
                      state)
    :print-state (do (println state) state)
    :print-player-state (do (println (get-in state [:players (:selected-player @ui/ui-state)])) state)
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
                           (recur refresh new-state))
              refresh (let [new-state (step s)]
                        (render! new-state)
                        (recur (timeout rate) new-state))
              player-channel (let [new-state v]
                               ;(.postMessage worker v)
                               (recur refresh new-state))))))))

(defn start-game []
  "Start the game.
   * Initialize game state
   * Set up user interface
   * Run the webworker script
   * Start the game loop and binds the webworker code to the player channel."
  (let [state (init-state)
        ui-channel (ui/setup-ui (mapv #(select-keys % [:index :name :team :nr]) (:players state)) (:teams state))
        player-channel (setup-worker)]
    (game-loop state ui-channel player-channel)))

(defn on-js-reload []
  "Figwheel convenience function.")

(defonce game (start-game))
