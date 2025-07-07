(ns dev.reus.rondo.app
  (:require [dev.reus.rondo.gameloop :as gameloop]))

(defn ^:export main []
  "Main entry point for the application"
  (gameloop/start-game!))

;; Auto-start the game when the script loads
(main)
