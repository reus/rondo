(ns dev.reus.rondo.core
  (:require [compojure.core :refer (defroutes GET)]
            [compojure.route :as route]
            [ring.util.response :as resp]
            [ring.adapter.jetty :refer (run-jetty)]))

(defonce server (atom nil))

(defroutes routes
  (GET "/" [] (resp/resource-response "index.html" {:root "public"}))
  (route/resources "/")
  (route/not-found "Page not found"))

(defn start-server! []
  (reset! server
          (run-jetty routes {:port 3000 :join? false})))

(defn stop-server! []
  (when @server
    (.stop @server)
    (reset! server nil)))

(def -main start-server!)
