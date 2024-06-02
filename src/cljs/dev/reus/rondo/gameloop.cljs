(ns dev.reus.rondo.gameloop
  (:require [dev.reus.rondo.canvas2d :as canvas2d]
            [dev.reus.rondo.ui :as ui]
            [dev.reus.rondo.model :as model]
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
    {:refresh-rate 15
     :frame 1
     :time initial-time
     :frame-time 0
     :fps-info {:frame 1
                :time initial-time
                :fps 0}
     :drawing-context drawing-context
     :ball {
            :state :with-player
            :player 1
            :ke 0 ;; kinetic energy
            :velocity nil
            :pos nil
            :shot-start-time nil
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

(defn control-player [{direction :direction {status :status} :goal :as player} forward turn run]
  (if (every? zero? (list forward turn))
    (if (= status :human-controlled)
      (assoc player :goal {:status :decelerate})
      player)
    (assoc player :goal {:status :human-controlled :acc-dir (mapv #(* forward %) direction) :turn turn :run run})))

(defonce ball-states [{:with-player :with-player
                       :shooting :release-shot
                       :release-shot :moving}
                      {:with-player :shot-initiated
                       :shot-initiated :shooting
                       :shooting :shooting}])

(defn control-ball [ball shot]
  (let [new-state (get-in ball-states [shot (:state ball)])]
    (assoc ball :state new-state)))

(defn game-controls [state]
  (let [ui-state @ui/ui-state
        selected-player (get ui-state :selected-player)
        player (get-in state [:players selected-player])
        [forward turn shot run] (get ui-state :keys-pressed)
        ball (get state :ball)
        player-with-ball (:player ball)
        update-ball (fn [state]
                      (if (= player-with-ball selected-player)
                        (assoc state :ball (control-ball ball shot))
                        state))]
    (cond-> state
      selected-player (assoc-in [:players selected-player] (control-player player forward turn run))
      player-with-ball update-ball)))

(defn step [state]
  "take a step in state updates"
  (-> state
      update-time
      game-controls
      model/update-players
      model/update-ball
      model/player-player-collisions
      model/player-ball-collisions
      model/check-ball
      ))

(defn render! [state]
  "Render all game objects to the drawing context."
  (canvas2d/draw-scene! state))

(defn process-ui [state e]
  (case (:type e)
    :reset-position (model/reset-position state (:index e))
    :random-position (model/random-position state (:index e))
    :give-ball (assoc state :ball {:state :with-player
                                   :player (:index e)
                                   :velocity nil
                                   :ke 0
                                   :pos nil})
    :print-state (do
                   (pprint state)
                   state)
    :print-ui-state (do
                      (pprint @ui/ui-state)
                      state)
    :print-player-info (let [player (get-in state [:players (:index e)])]
                         (pprint player)
                         state)
    :print-ball-info (do
                       (pprint (get state :ball))
                       state)
    :click-canvas-move (let [[x y] (:pos e)
                             mouse-selected-player (model/point-in-players? [x y] (:players state))]
                         (if mouse-selected-player
                           (do
                             (swap! ui/ui-state assoc :selected-player mouse-selected-player :selected-team nil)
                             state)
                           (if-let [selected-player (:selected-player @ui/ui-state)]
                             (assoc-in state [:players selected-player :goal] {:status :move-destination :destination [x y] :run (get-in @ui/ui-state [:keys-pressed 3])})
                             state)))
    :goal (update-in state [:players (:index e)] #(assoc % :goal (:goal e)))
    state))

(defn setup-worker-channel [worker]
  "Create a channel through which the webworker can communicate."
  (let [worker-channel (chan)]
    (set! (.-onmessage worker) (fn [msg]
                                 (println "Received a message from worker:" msg.data)
                                 (go
                                   (while (not (get @ui/ui-state :webworker))
                                     (<! (timeout 1000)))
                                   (>! worker-channel msg))))
    worker-channel))

(defn state->array-buffer! [state]
  (let [ar (js/Uint16Array. 8)]
    (aset ar 0 (get state :frame-time))
    (.-buffer ar)))


(defn game-loop [state ui-channel worker worker-channel]
  (let [rate (:refresh-rate state)]
    (render! state)
    (go (loop [refresh (timeout rate)
               ui-state-update (timeout 1000)
               s state]
          (let [[v c] (alts! [refresh
                              ui-state-update
                              ui-channel
                              worker-channel] :priority true)]
            (condp = c
              refresh (let [new-state (step s)]
                        (render! new-state)
                        (recur (timeout rate) ui-state-update new-state))
              ui-state-update (let [new-state (update-fps s)]
                                (ui/update-ui-state! new-state)
                                (recur refresh (timeout 1000) new-state))
              ui-channel (let [new-state (process-ui s v)]
                           (recur refresh ui-state-update new-state))
              worker-channel (let [state-info v
                                   state-array (state->array-buffer! s)
                                   ;obj (.parse js/JSON (.stringify js/JSON state))]
                                   ]
                               (.postMessage worker state-array [state-array])
                               (recur refresh ui-state-update s))))))))
(defn start-game! []
  "Start the game.
   * Initialize game state
   * Set up user interface and receive channel
   * Run the webworker script and receive channel
   * Start the game loop and pass the channels"
  (let [state (init-state)
        ui-channel (ui/setup-ui state)
        worker (js/Worker. "cljs-out/dev_worker-main.js")
        worker-channel (setup-worker-channel worker)]
    (game-loop state ui-channel worker worker-channel)))

(def game (start-game!))


(comment

  (js/alert "hi")

)
