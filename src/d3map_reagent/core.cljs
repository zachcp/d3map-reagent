(ns d3map-reagent.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljsjs.d3]))

;cljs port of http://bl.ocks.org/mbostock/9943478

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(def width 960)
(def height 600)
(defn formatNumber [d]
  ((.format js/d3 ",.01f") d))

(def path (.. js/d3.geo path (projection nil)))
(def radius 
  (.. js/d3.scale 
            sqrt 
            (domain #js [0 1e6])
            (range  #js [0 15] )))

(def svg 
  (.. js/d3 (select "body")
            (append "svg")
            (attr "width" width)
            (attr "height" height)))

(def legend 
  (.. svg (append "g")
          (attr "class" "legend")
          (attr "transform" (str "translate(" (- width 50) "," (- height 20) ")"))
        (selectAll "g")
          (data #js [1e6 5e6 1e7])
          enter
          (append "g")
    ))

(let []
  (.. legend 
    (append "circle")
    (attr "cy" #(-  (radius %))))
  (.. legend
    (append "text")
    (attr "y" #(* -2 (radius %)))
    (attr "dy" "1.3em")
    (text (fn [d] ( (.format js/d3 ".1s") d)))))

(let [usajson "js/us.json"]
  (.json js/d3 usajson
    (fn [error json]
      (.. svg
        (append "path")
        (datum (.feature js/topojson (json (.. json objects nation))))
        (attr "class" "land")
        (attr "d" path))
      
      (.. svg 
        (append "path")
        (datum (.mesh js/topojson (json (.. json objects states) (fn [a b] (not= a b)))))
        (attr "class" "border border-state")
        (attr "d" path))

      (.. svg
        (append "g")
        (attr "class" "bubble")
        (selectAll "circle")
        (data (.. js/topojson 
                  (feature json (.. json objects counties)) 
                  features))
        (sort (fn [a b] (-  (.. b properties population) (.. a properties population))))
        enter
        (append "circle")
        (attr "transform" #(str "translate(" (.. path centroid %) ")"))
        (attr "r" (fn [d] (radius (.. d properties population))))
        (append "title")
        (text (fn [d] (str (.. d properties name) " Population " (formatNumber (.. d properties population))))))
    )))




(defonce app-state (atom {:text "Hello world!"}))

(defn hello-world []
  [:h1 (:text @app-state)])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
