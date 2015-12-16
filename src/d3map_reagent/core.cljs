(ns d3map-reagent.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET POST]]
            [re-frame.core :as re-frame :refer [register-handler
                                   path
                                   register-sub
                                   dispatch
                                   dispatch-sync
                                   subscribe]]
            [cljsjs.d3]
            [cljsjs.topojson]))

;cljs port of http://bl.ocks.org/mbostock/9943478

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

;;;---- Event handlers-----------
(register-handler                 ;; setup initial state
  :initialize                     ;; usage:  (submit [:initialize])
  (fn
    [db _]
    (GET "js/us.json" {:response-format :json
                       :handler         #(re-frame/dispatch [:process-mapdata %1])})
    db))    ;; what it returns becomes the new state

(register-handler                 ;; setup initial state
  :process-mapdata                ;; usage:  (submit [:initialize])
  (fn
    [db [_ resp]]
    (let [data (js->clj resp)]
      {:mapdata #js [resp]})))    ;; what it returns becomes the new state


;;;----Subscription Handlers -----------
(register-sub
  :mapdata
  (fn
    [db _]                       ;; db is the app-db atom
    (reaction (:mapdata @db))))    ;; wrap the computation in a reaction
;;;---------------


(defn d3map-inner []
  (let [width 960
        height 600
        formatNumber (fn [d]  ((.format js/d3 ",.01f") d))
        path (.. js/d3.geo path (projection nil))
        radius (.. js/d3.scale
                   sqrt
                   (domain #js [0 1e6])
                   (range  #js [0 15] ))
        update  (fn [comp]
                  (let [mapclj (reagent/props comp)
                        json (clj->js mapclj)
                        svg  (.. js/d3 (select "svg"))]

                    (.. svg (append "g")
                        (attr "class" "legend")
                        (attr "transform" (str "translate(" (- width 50) "," (- height 20) ")"))
                        (selectAll "g")
                        (data #js [1e6 5e6 1e7])
                        enter
                        (append "g"))

                    (.. svg
                        (append "path")
                        (datum (.feature js/topojson json (.. json -objects -nation)))
                        (attr "class" "land")
                        (attr "d" path))

                    (.. svg
                        (append "g")
                        (attr "class" "bubble")
                        (selectAll "circle")
                        (data (.. js/topojson
                                  (feature json (.. json -objects -counties))
                                  -features))
                        (sort (fn [a b] (-  (.. b -properties -population) (.. a -properties -population))))
                        enter
                        (append "circle")
                        (attr "transform" #(str "translate(" (.centroid path %) ")"))
                        (attr "r" (fn [d] (radius (.. d -properties -population))))
                        (append "title")
                        (text (fn [d] (str (.. d -properties -name) " Population " (formatNumber (.. d -properties -population))))))
                    ))]
    
    (reagent/create-class
      {:reagent-render (fn []
                         [:div
                          [:h2 "Awesome Map"]
                          [:div#map [:svg {:style {:height (str  height "px") :width (str width "px")}}] ]])

       :component-did-mount (fn [comp] (update comp))
       :component-did-update update
       :display-name "d3map-inner"})))

;(defn simplecomponent []
;  (let [mapdata (re-frame/subscribe [:mapdata])]
;    (println @mapdata)
;    [:p (str (first @mapdata))]))

(defn d3map-outer []
  (let [mapdata (re-frame/subscribe [:mapdata])]   ;; obtain the data
    (fn []
      [d3map-inner (first @mapdata)])))

(let []
  (dispatch-sync [:initialize])
  (reagent/render [d3map-outer] (. js/document (getElementById "app")))
  )


