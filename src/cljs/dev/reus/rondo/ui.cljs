(ns dev.reus.rondo.ui
  (:require [dev.reus.rondo.utils :refer [by-id]]
            [dev.reus.rondo.gamedata :as gamedata]
            [dev.reus.rondo.model :refer [init-players
                                          point-in-players?
                                          reset-position-selected-player
                                          random-position-selected-player]]
            [cljs.core.async :as async :refer [chan put!]]
            [reagent.core :as reagent]
            [reagent.dom :as rdom]))

(defonce signal (reagent/atom {}))

(defonce ui-state (reagent/atom {:teams gamedata/teams
                                 :players (map #(select-keys % [:index :nr :name :team]) (init-players gamedata/players))
                                 :selected-team nil
                                 :selected-player nil}))

(defn swap-state! [swap-fn & args]
  "Helper function to update state and send signal to channel."
  (doseq [a (partition 2 args)]
    ((fn [[s v]] (swap! ui-state swap-fn s v)) a))
  (reset! signal {:type :ui-state-changed}))

(defn ui-player []
  "Returns ui for handling players."
  (let [state @ui-state
        player-index (:selected-player state)
        player (get (:players state) player-index)]
    [:div
     [:a {:class "back ":on-click #(swap-state! assoc :selected-player nil)} "Back"]
     [:div "name: " (:name player)]
     [:div [:input {:type "button" :value "Reset position" :on-click #(js/alert "reset position")}]]
     [:div [:input {:type "button" :value "Random position" :on-click #(js/alert "random position")}]]
     ]))

(defn ui-team []
  "Returns ui for handling teams."
  [:div
   [:a {:class "back" :on-click #(swap-state! assoc :selected-team nil)} "Back"]])

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
                                :on-click #(swap-state! assoc :selected-team (:id team) :selected-player nil)}
                           (:name team)]
                      (for [player (filter (comp #{(:id team)} :team) (:players state))]
                        ^{:key player} [:li {:class (if (= (:index player)
                                                           (:selected-player state))
                                                      "selected player"
                                                      "player")
                                             :on-click #(swap-state! assoc :selected-team nil :selected-player (:index player))}
                                        [:span {:class "nr"} (:nr player)]
                                        [:span {:class "name"} (:name player)]])])]
     [:div.settings
        (if (:selected-player state)
          [ui-player]
          (if (:selected-team state)
            [ui-team]
            [:div "General settings"
             [:input {:type "button" :value "Reset all players"}]]))]]))


(defn canvas-clicked [event]
  "Process a mousedown event in the canvas."
  (reset! signal {:type :click :event event}))

(defn process-keydown [event]
  "Process a keydown event in the canvas."
  (.log js/console event))

(defn setup-event-handlers! []
  "Add event listeners."
  (let [canvas (by-id "canvas")]
    (.addEventListener canvas "mousedown" (fn [e] (canvas-clicked e)))
    (.addEventListener js/document "keydown" (fn [e] (process-keydown e)))))

(defn setup-ui []
  "Setup the main user interface rendered by Reagent. Creates a core async channel through that offers
   a means to communicate with the game loop. Returns the channel and a reagent atom with ui state."
  (let [ui-chan (chan)
        state-updater (fn []
                        (let [s @signal]
                          (put! ui-chan s)))]
    (rdom/render [get-ui] (by-id "ui"))
    (setup-event-handlers!)
    (reagent/track! state-updater)
    ui-chan))
