(ns dev.reus.rondo.app
  (:require [dev.reus.rondo.canvas2d :as canvas2d]
            [dev.reus.rondo.ui :as ui]
            [dev.reus.rondo.model :as model]
            [dev.reus.rondo.gamedata :as gamedata]
            [cljs.core.async :as async :refer [<! >! chan timeout]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defn find-color [team]
  "Determine the shirt-color of a team. Use a keyword."
  (loop [i 0]
    (if-let [t (get gamedata/teams i)]
      (if (= (:id t) team)
        (:color t)
        (recur (inc i)))
      [0 0 0])))

(defn init-player [idx player]
  "Initialize player vector. Adds different game related
   properties to each player hashmap."
  (let [team (:team player)
        color (find-color team)
        [r g b] (map #(* 256 %) color)
        color-str (str "rgb(" r "," g "," b ")")]
    (assoc player
           :index idx
           :color color
           :color-str color-str)))

(defn init-state []
  "Create the initial state hashmap."
  (let [players (map-indexed init-player gamedata/players)
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
            :player 1
            :ke 0 ;; kinetic energy
            :velocity nil
            :direction nil
            :pos nil
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
    (if (>= time-since-fps 1000)
      (do
        (swap! ui/ui-state assoc :fps (int (/ 1000 (/ time-since-fps frame))))
        (assoc new-state :frame 1 :fps-time next-millis))
      new-state)))

(defn update-acceleration [player fw run]
  (let [direction (:direction player)
        velocity (:velocity player)
        magn (model/magnitude velocity)
        speed (:speed player)]
    (assoc player :acceleration (mapv #(* % speed fw run) direction))))

(defn update-direction [player xf yf run]
  (let [direction (:direction player)
        turn (:turn player)
        [vx vy] (if (= (:velocity player) [0 0]) direction (:velocity player))
        magn (model/magnitude [vx vy])
        perp (model/normalize (map * [xf yf] [vy vx]))
        acceleration (:acceleration player)]
    ;(println (map #(* magn run %) perp))
    ;(println acceleration)

    (if (> magn 10)
      (let [ca (map #(* magn %) perp)] (assoc player :acceleration (mapv + acceleration ca)))
      (let [ca (map #(* magn run %) perp)] (assoc player :acceleration (mapv + acceleration ca))))))



;    (cond
;      (and (> magn 0) (> run 1)) (let [ca (map #(* % (min run 2) turn) perp)]
;                     (assoc player :acceleration (mapv + acceleration ca)))
;      (and (> magn 0) (= run 1)) (let [ca (map #(* % (* magn (max 3 run))) perp)]
;                   (assoc player :acceleration (mapv + acceleration ca)))
;      (= magn 0) (let [[vx vy] direction
;                       ca (map #(* % %2 run) [xf yf] [vy vx])]
;                   (assoc player :acceleration (mapv + acceleration ca))))))

(defn update-shot [ball shot current-time player]
  (let [dir (:direction player)
        pos (:pos player)
        s (:shot ball)]
    (if shot
      (if-not s
        (assoc ball :shot current-time)
        ball)
      (if s
        (let [ke (* (- current-time s) 100)]
          {:shot false
           :direction dir
           :player nil
           :velocity nil
           :ke ke
           :pos (mapv + pos (map #(* % 10) dir))})
        ball))))

(defn game-controls [state]
  (let [p (:selected-player @ui/ui-state)]
    (if-let [player (get-in state [:players p])]
      (let [[xf yf fw run shot] @ui/keys-pressed
            ball (:ball state)
            pb (:player ball)
            current-time (:current-time state)
            new-player (-> player
                           (update-acceleration fw run)
                           (update-direction xf yf run))]
            (-> state
                (assoc-in [:players p] new-player)
                (assoc :ball (if (= pb p)
                               (update-shot ball shot current-time new-player)
                               ball))))
        state)))

(defn update-players [state]
  (let [p (:selected-player @ui/ui-state)
        update-fn (fn [player]
                    (if (= p (:index player))
                      player
                      (assoc player :acceleration [0 0])))]
    (assoc state :players (mapv update-fn (:players state)))))

(defn step [state]
  (-> state
      update-time
      game-controls
      update-players
      model/move-players
      model/move-ball
      ;;model/distance-to-ball
      model/player-pickup-ball))

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
    :reset-all (init-state)
    :random-position (let [i (:index evt)]
                      state)
    :reset-position (let [i (:index evt)
                          player (get-in state [:players i])
                          pos (get-in gamedata/players [i :pos])
                          reset (assoc player :pos pos :acceleration [0 0] :velocity [0 0])]
                      (assoc-in state [:players i] reset))
    :give-ball (assoc state :ball {:player (:index evt)
                                   :velocity nil
                                   :direction nil
                                   :ke 0
                                   :pos nil})
    :change-selected-player-with-ball (if-let [selected-player (:selected-player @ui/ui-state)]
                                        (let [team (get-in state [:players selected-player :team])]
                                          (let [team-players (filter #(and (= team (:team %))
                                                                           (not= selected-player (:index %)))
                                                                     (:players state))
                                                pb (:player (:ball state))]
                                            (if (and pb
                                                     (some #{pb} (map #(:index %) team-players)))
                                              (do (swap! ui/ui-state assoc :selected-player pb)
                                                  state)
                                              state)))
                                        state)
    :change-selected-player (if-let [selected-player (:selected-player @ui/ui-state)]
                              (let [team (get-in state [:players selected-player :team])
                                    index (get-in state [:players selected-player :index])]
                                (let [team-players (map #(:index %) (filter #(and (= team (:team %))
                                                                                  (not= selected-player (:index %)))
                                                                            (:players state)))]
                                  (do
                                    (if (some #{(inc index)} team-players)
                                      (swap! ui/ui-state assoc :selected-player (inc index))
                                      (swap! ui/ui-state assoc :selected-player (first team-players)))
                                    state)))
                              state)
    :print-state (do (println state) state)
    :print-ball-info (do (println (:ball state)) state)
    :print-test-info (do (println state) state)
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
