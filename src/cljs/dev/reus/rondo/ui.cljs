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

(defonce ui-state (reagent/atom {}))

(comment
(defn swap-state! [swap-fn & args]
  "Helper function to update state and send signal to channel."
  (doseq [a (partition 2 args)]
    ((fn [[s v]] (swap! ui-state swap-fn s v)) a))
  (reset! signal {:type :ui-state-changed}))
)

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
                                ;:on-click #(swap-state! assoc :selected-team (:id team) :selected-player nil)}
                                :on-click (fn [e] (reset! signal {:type :select-team :team-id (:id team)}))}
                           (:name team)]
                      (for [player (filter (comp #{(:id team)} :team) (:players state))]
                        ^{:key player} [:li {:class (if (= (:index player)
                                                           (:selected-player state))
                                                      "selected player"
                                                      "player")
                                             ;:on-click #(swap-state! assoc :selected-team nil :selected-player (:index player))}
                                             :on-click (fn [e] (reset! signal {:type :select-player :index (:index player)}))}
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

(defn setup-event-handlers! []
  "Add event listeners."
  (let [canvas (by-id "canvas")]
    (.addEventListener canvas "mousedown" (fn [e] (reset! signal {:type :click :event e})))
    (.addEventListener js/document "keyup" (fn [e] (reset! signal {:type :keyup :event e})))
    (.addEventListener js/document "keydown" (fn [e] (reset! signal {:type :keydown :event e})
                                               (case e.key ("ArrowUp" "ArrowDown") (do (.preventDefault e)) nil)))))

(defn setup-ui [state]
  "Setup the main user interface rendered by Reagent. Creates a core async channel through that offers
   a means to communicate with the game loop. Returns the channel and a reagent atom with ui state."
  (let [ui-chan (chan)
        state-updater (fn []
                        (let [s @signal]
                          (put! ui-chan s)))]
    (reset! ui-state state)
    (rdom/render [get-ui] (by-id "ui"))
    (setup-event-handlers!)
    (reagent/track! state-updater)
    ui-chan))

(defn update-fps! [{:keys [fps]}]
  "Update the frames per second counter in the ui."
)
