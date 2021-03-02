(ns dev.reus.rondo.ui2
  (:require [dev.reus.rondo.utils :refer [by-id]]
            [cljs.core.async :as async :refer [chan put!]]
            [reagent.core :as reagent]
            [reagent.dom :as rdom]))

(defonce ui-state (reagent/atom {:selected-player nil
                                 :selected-team nil
                                 :teams nil
                                 :players nil
                                 :ui-chan nil
                                 :keys-pressed [0 ; forward
                                                0 ; turn left (-1) or right (1)
                                                0 ; shoot
                                                1 ; walk (1) or run (3)
                                               ]
                                 }))

(defn notify-channel! [m]
  (let [c (get @ui-state :ui-chan)]
    (put! c m)))

(defn ui-player [player]
  "Returns reagent ui for handling players."
    [:div
     [:a {:class "back" :on-click (fn [_]
                                    (swap! ui-state assoc :selected-player nil))} "Back"]
     [:div "name: " (:name player)]
     [:div [:input {:type "button"
                    :value "Reset position"
                    :on-click (fn [e]
                                (notify-channel! {:type :reset-position :index (:index player)}))}]]
     [:div [:input {:type "button"
                    :value "Random position"
                    :on-click (fn [e]
                                (notify-channel! {:type :random-position :index (:index player)}))}]]
     [:div [:input {:type "button"
                    :value "Give ball"
                    :on-click (fn [e]
                                (notify-channel! {:type :give-ball
                                                  :index (:index player)}))}]]
     [:div [:input {:type "button"
                    :value "Idle"
                    :on-click (fn [e]
                                (notify-channel! {:type :goal
                                                  :goal {:status :set-idle}
                                                  :index (:index player)}))}]]
     [:div [:input {:type "button"
                    :value \u2191
                    :on-click (fn [e]
                                (notify-channel! {:type :goal
                                                  :goal {:status :move-direction
                                                         :direction [0 -1]}
                                                  :index (:index player)}))}]]
     [:div [:input {:type "button"
                    :value \u2190
                    :on-click (fn [e]
                                (notify-channel! {:type :goal
                                                  :goal {:status :move-direction
                                                         :direction [-1 0]}
                                                  :index (:index player)}))}]
           [:input {:type "button"
                    :value \u2192
                    :on-click (fn [e]
                                (notify-channel! {:type :goal
                                                  :goal {:status :move-direction
                                                         :direction [1 0]}
                                                  :index (:index player)}))}]]
     [:div [:input {:type "button"
                    :value \u2193
                    :on-click (fn [e]
                                (notify-channel! {:type :goal
                                                  :goal {:status :move-direction
                                                         :direction [0 1]}
                                                  :index (:index player)}))}]]
     [:div [:input {:type "checkbox"
                    ;:value "approach ball"
                    :checked (= :approach-ball (get-in player [:goal :status]))
                    :on-change (fn [e]
                                 (let [status (if (.. e -target -checked)
                                                :approach-ball
                                                :set-idle)]
                                       (notify-channel! {:type :goal
                                                         :goal {:status status}
                                                         :index (:index player)})))}] "Approach ball"]
     ])

(defn ui-team []
  "Returns ui for handling teams."
  [:div
   [:a {:class "back" :on-click (fn [e] nil)} "Back"]])

(defn get-ui []
  "Get the Reagent array that forms the user interface."
  (let [state @ui-state
        players (:players state)]
    [:div
     [:div.teams
      (for [team (:teams state)]
        ^{:key team} [:ul [:li {:class (if (= (:id team)
                                              (:selected-team state))
                                         "selected team"
                                         "team")
                                :on-click (fn [_] (swap! ui-state assoc assoc :selected-team (:id team)))}
                           (:name team)]
                      (for [player (filter (comp #{(:id team)} :team) players)]
                        ^{:key player} [:li {:class (if (= (:index player)
                                                           (:selected-player state))
                                                      "selected player"
                                                      "player")
                                             :on-click (fn [_] (swap! ui-state assoc :selected-player (:index player)))}
                                        [:span {:class "nr"} (:nr player)]
                                        [:span {:class "name"} (:name player)]])])]
     [:div.settings
      [:div "FPS: " (str (:fps @ui-state))]
      (if-let [selected-player-index (:selected-player state)]
        [ui-player (get players selected-player-index)]
        (if (:selected-team state)
          [ui-team]
          [:div "General settings"
           [:input {:type "button" :value "Reset all players" :on-click (fn [e] nil)}]]))]]))

(defn process-key [e dir]
  (case dir
    :down (case e.key
            ;; control player
            "ArrowUp" (swap! ui-state assoc-in [:keys-pressed 0] 1)
            "ArrowLeft" (swap! ui-state assoc-in [:keys-pressed 1] -1)
            "ArrowRight" (swap! ui-state assoc-in [:keys-pressed 1] 1)
            ("z" "Z") (swap! ui-state assoc-in [:keys-pressed 2] 1)
            ("Shift") (swap! ui-state assoc-in [:keys-pressed 3] 3)
            ("Control") (swap! ui-state assoc-in [:keys-pressed 3] 0.2)
            ;; print state
            ("s" "S") (notify-channel! {:type :print-state})
            ("u" "U") (notify-channel! {:type :print-ui-state})
            ("b" "B") (notify-channel! {:type :print-ball-info})
            ("p" "P") (if-let [p (get @ui-state :selected-player)]
                        (notify-channel! {:type :print-player-info :index p}))
            :default)
    :up (case e.key
          "ArrowUp" (swap! ui-state assoc-in [:keys-pressed 0] 0)
          "ArrowLeft" (swap! ui-state assoc-in [:keys-pressed 1] 0)
          "ArrowRight" (swap! ui-state assoc-in [:keys-pressed 1] 0)
          ("z" "Z") (swap! ui-state assoc-in [:keys-pressed 2] 0)
          ("Shift") (swap! ui-state assoc-in [:keys-pressed 3] 1)
          ("Control") (swap! ui-state assoc-in [:keys-pressed 3] 1)
          :default)
    :default))

(defn get-key-listener [type]
  (fn [e]
    (process-key e type)
    (case e.key
      ("Space" "Shift" "z" "Z" "Alt" "ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown" "PageDown" "End") (do (.preventDefault e) false)
      true)))

(defn setup-event-handlers! []
  "Add event listeners."
  (let [canvas (by-id "canvas")]
    (.addEventListener canvas "mousedown" (fn [event]
                                            (let [rect (.getBoundingClientRect canvas)
                                                  x (- (.-clientX event) (.-left rect))
                                                  y (- (.-clientY event) (.-top rect))
                                                  button (.-button event)]
                                              (case button
                                                0 (notify-channel! {:type :click-canvas-move :pos [x y]})
                                                2 (notify-channel! {:type :click-canvas-shoot :pos [x y]})
                                                :do-nothing))))
    (.addEventListener js/document "keyup" (get-key-listener :up))
    (.addEventListener js/document "keydown" (get-key-listener :down))))

(defn setup-ui [{players :players teams :teams :as state}]
  "Setup the main user interface rendered by Reagent. Creates a core async channel through
that offers a means to communicate with the game loop. Returns the channel and a reagent atom
with ui state."
  (let [ui-chan (chan)
        sel-players (mapv #(select-keys % [:index :name :team :nr]) players)
        sel-teams (mapv #(select-keys % [:id :name]) teams)]
    (swap! ui-state assoc :players sel-players :teams sel-teams :ui-chan ui-chan)
    (rdom/render [get-ui] (by-id "ui"))
    (setup-event-handlers!)
    ui-chan))

(defn update-ui-state! [{{fps :fps} :fps-info players :players}]
  (swap! ui-state assoc :players (mapv #(select-keys % [:index :name :team :nr :goal]) players))
  (swap! ui-state assoc :fps (.round js/Math fps)))
