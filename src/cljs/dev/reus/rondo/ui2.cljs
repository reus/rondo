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
                                 :keys-pressed [0 0]
                                 }))

(defn notify-channel! [m]
  (let [c (get @ui-state :ui-chan)]
    (put! c m)))


(defn ui-player [player]
  "Returns reagent ui for handling players."
    [:div
     [:a {:class "back" :on-click (fn [_] (swap! ui-state assoc :selected-player nil))} "Back"]
     [:div "name: " (:name player)]
     [:div [:input {:type "button"
                    :value "Reset position"
                    :on-click (fn [e] (notify-channel! {:type :reset-position :index (:index player)}))}]]
     [:div [:input {:type "button"
                    :value "Random position"
                    :on-click (fn [e] (notify-channel! {:type :random-position :index (:index player)}))}]]
     [:div [:input {:type "button"
                    :value "Give ball"}]]])

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
            ;; control player with arrow keys
            "ArrowUp" (swap! ui-state assoc-in [:keys-pressed 0] 1)
            ;; print state
            ("s" "S") (notify-channel! {:type :print-state})
            ("u" "U") (notify-channel! {:type :print-ui-state :value @ui-state})
            :default)
    :up (case e.key
          "ArrowUp" (swap! ui-state assoc-in [:keys-pressed 0] 0)
          :default)

    :default))

(defn get-key-listener [type]
  (fn [e]
    (process-key e type)
    (case e.key
      ("Space" "Shift" "z" "Z" "Alt" "ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown" "PageDown" "End") (do (.preventDefault e) false)
      true)))

(defn setup-event-handlers! [update-ui-fn]
  "Add event listeners."
  (let [canvas (by-id "canvas")]
    (.addEventListener canvas "mousedown" (fn [e] (notify-channel! {:type :click-canvas :event e})))
    (.addEventListener js/document "keyup" (get-key-listener :up))
    (.addEventListener js/document "keydown" (get-key-listener :down))))

(defn setup-ui [{players :players teams :teams :as state}]
  "Setup the main user interface rendered by Reagent. Creates a core async channel through
that offers a means to communicate with the game loop. Returns the channel and a reagent atom
with ui state."
  (let [ui-chan (chan)
        update-ui-fn (fn [m]
                    (put! ui-chan m))
        sel-players (mapv #(select-keys % [:index :name :team :nr]) players)]
    (swap! ui-state assoc :players sel-players :teams teams :ui-chan ui-chan)
    (rdom/render [get-ui] (by-id "ui"))
    (setup-event-handlers! update-ui-fn)
    ui-chan))

(defn update-fps! [fps]
  "Update the fps of the game"
  (swap! ui-state assoc :fps (.round js/Math fps)))
