(ns dev.reus.rondo.ui
  (:require [dev.reus.rondo.utils :refer [by-id]]
            [dev.reus.rondo.gamedata :as gamedata]
            [dev.reus.rondo.model :refer [point-in-players?
                                          reset-position-player
                                          random-position-player]]
            [cljs.core.async :as async :refer [chan put!]]
            [reagent.core :as reagent]
            [reagent.dom :as rdom]))

(defonce signal (reagent/atom {}))
(defonce ui-state (reagent/atom {:selected-player nil
                                 :selected-team nil
                                 :teams nil
                                 :players nil
                                 :fps nil
                                 }))

;; define keys-pressed, a vector containing in this order:
;; * move left/right
;; * move up/down
;; * run or walk
;; * turn
(defonce keys-pressed (atom [0 0 1 0]))

(defn ui-player []
  "Returns ui for handling players."
  (let [state @ui-state
        player-index (:selected-player state)
        player (get (:players state) player-index)]
    [:div
     [:a {:class "back" :on-click (fn [e] (reset! signal {:type :select-team :team-id nil}))} "Back"]
     [:div "name: " (:name player)]
     [:div [:input {:type "button" :value "Reset position" :on-click (fn [e] (reset! signal {:type :reset-position :index player-index}))}]]
     [:div [:input {:type "button" :value "Random position" :on-click (fn [e] (reset! signal {:type :random-position :index player-index}))}]]
     ]))

(defn ui-team []
  "Returns ui for handling teams."
  [:div
   [:a {:class "back" :on-click (fn [e] (reset! signal {:type :select-team :team-id nil}))} "Back"]])

(defn get-ui []
  "Get the Reagent array that forms the user interface."
  (let [state @ui-state]
    [:div
     [:div.teams
      (for [team (:teams state)]
        ^{:key team} [:ul [:li {:class (if (= (:id team)
                                              (:selected-team state))
                                         "selected team"
                                         "team")
                                :on-click (fn [_] (swap! ui-state assoc assoc :selected-team (:id team)))}
                           (:name team)]
                      (for [player (filter (comp #{(:id team)} :team) (:players state))]
                        ^{:key player} [:li {:class (if (= (:index player)
                                                           (:selected-player state))
                                                      "selected player"
                                                      "player")
                                             :on-click (fn [_] (swap! ui-state assoc :selected-player (:index player)))}
                                        [:span {:class "nr"} (:nr player)]
                                        [:span {:class "name"} (:name player)]])])]
     [:div.settings
      [:div "FPS: " (str (:fps state))]
      (if (:selected-player state)
        [ui-player]
        (if (:selected-team state)
          [ui-team]
          [:div "General settings"
           [:input {:type "button" :value "Reset all players"}]]))]]))

(defn process-key [e dir]
  (case dir
    :down (case e.key
            "ArrowLeft" (swap! keys-pressed assoc 0 -1)
            "ArrowRight" (swap! keys-pressed assoc 0 1)
            "ArrowUp" (swap! keys-pressed assoc 1 -1)
            "ArrowDown" (swap! keys-pressed assoc 1 1)
            "Shift" (swap! keys-pressed assoc 2 3)
            "PageDown" (swap! keys-pressed assoc 3 1)
            "PageUp" (swap! keys-pressed assoc 3 2)
            "Control" (swap! keys-pressed assoc 4 1)
            ("k" "K") (println @keys-pressed)
            ("s" "S") (reset! signal {:type :print-state})
            ("u" "U") (println @ui-state)
            ("p" "P") (reset! signal {:type :print-player-state})
            :default)
    :up (case e.key
          "ArrowLeft" (swap! keys-pressed assoc 0 0)
          "ArrowRight" (swap! keys-pressed assoc 0 0)
          "ArrowUp" (swap! keys-pressed assoc 1 0)
          "ArrowDown" (swap! keys-pressed assoc 1 0)
          "Shift" (swap! keys-pressed assoc 2 1)
          "Control" (swap! keys-pressed assoc 4 0)
          :default)
    :default))

(defn get-key-listener [type]
  (fn [e]
    (process-key e type)
    (case e.key
      ("Shift" "Control" "Alt" "ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown" "PageUp" "PageDown") (do (.preventDefault e) false)
      true)))

(defn setup-event-handlers! []
  "Add event listeners."
  (let [canvas (by-id "canvas")]
    (.addEventListener canvas "mousedown" (fn [e] (reset! signal {:type :click :event e})))
    (.addEventListener js/document "keyup" (get-key-listener :up))
    (.addEventListener js/document "keydown" (get-key-listener :down))))

(defn setup-ui [players teams]
  "Setup the main user interface rendered by Reagent. Creates a core async channel through that offers
   a means to communicate with the game loop. Returns the channel and a reagent atom with ui state."
  (let [ui-chan (chan)
        state-updater (fn []
                        (let [s @signal]
                          (put! ui-chan s)))]
    (swap! ui-state assoc :players players :teams teams)
    (rdom/render [get-ui] (by-id "ui"))
    (setup-event-handlers!)
    (reagent/track! state-updater)
    ui-chan))

(defn update-fps! [{:keys [fps]}]
  "Update the frames per second counter in the ui."
)
