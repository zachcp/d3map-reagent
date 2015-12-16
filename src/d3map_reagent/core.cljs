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
  :initialize-mapdata                     ;; usage:  (submit [:initialize])
  (fn
    [_ _]
    (GET "js/nyc.json" {:response-format :json
                       :handler         #(re-frame/dispatch [:process-mapdata %1])})
    _))

(register-handler                 ;; setup initial state
  :initialize-countdata                     ;; usage:  (submit [:initialize])
  (fn
    [_ _]
    (GET "js/counts.json" {:response-format :json
                        :handler         #(re-frame/dispatch [:process-mapdata %1])})
    _))

(register-handler                 ;; setup initial state
  :process-mapdata                ;; usage:  (submit [:initialize])
  (fn
    [db [_ resp]]
    (merge db :mapdata #js [resp])))    ;; what it returns becomes the new state

(register-handler                 ;; setup initial state
  :process-countdata                ;; usage:  (submit [:initialize])
  (fn
    [db [_ resp]]
    (let [counts (js->clj resp)]
    (merge db :counts counts))))    ;; what it returns becomes the new state


;;;----Subscription Handlers -----------
(register-sub
  :mapdata
  (fn
    [db _]                       ;; db is the app-db atom
    (reaction (:mapdata @db))))    ;; wrap the computation in a reaction

(register-sub
  :countdata
  (fn
    [db _]                       ;; db is the app-db atom
    (reaction (:countdata @db))))    ;; wrap the computation in a reaction

(register-sub
  :top-10
  (fn
    [db _]                       ;; db is the app-db atom
    (reaction (:mapdata @db))))    ;; wrap the computation in a reaction


;;;---------------




(defn NYCmap-inner []
  (let [width 960
        height 500
        projection (.. js/d3.geo
                       mercator
                       (center #js [-73.94 40.7])
                       (scale 50000)
                       (translate #js [(/ width 2) (/ height 2)]))

        formatNumber (fn [d]  ((.format js/d3 ",.01f") d))
        path (.. js/d3.geo path (projection projection))
        radius (.. js/d3.scale
                   sqrt
                   (domain #js [0 1e6])
                   (range  #js [0 15] ))

        update  (fn [comp]
                  (let [{:keys [mapdata countdata]} (reagent/props comp)
                        json (clj->js mapdata)
                        counts (clj->js countdata)
                        svg  (.. js/d3 (select "svg"))]

                    (println json)
                    (println counts)

                    ;; draw nyc map
                    (.. svg (append "g")
                        (attr "id" "boroughs")
                        (selectAll ".state")
                        (data (.-features json))
                        enter
                        (append "path")
                        (attr "class" (fn [d] (str "borough " d.properties.borough)))
                        (attr "d" path)
                        (style "fill" (fn [d] d.colors)))

                    ;; draw circles
                    (.. js/d3
                        (select "svg")
                        (append "g")
                        (attr "id" "circle")
                        (selectAll "circle")
                        (data counts)
                        enter
                        (append "scg:circle")
                        (attr "cx" (fn [d] (first (.-location d))))
                        (attr "cy" (fn [d] (second (.-location d))))
                        (attr "cr" 5)
                        (attr "class" "bubble")
                        )
                    ))]

    (reagent/create-class
      {:reagent-render (fn []
                         [:div
                          [:h2 "Awesome NYC Map"]
                          [:div#map [:svg {:style {:height (str  height "px") :width (str width "px")}}]]])

       :component-did-mount (fn [comp] (update comp))
       :component-did-update update
       :display-name "d3map-inner"})))



;(defn d3map-inner []
;  (let [width 960
;        height 600
;        formatNumber (fn [d]  ((.format js/d3 ",.01f") d))
;        path (.. js/d3.geo path (projection nil))
;        radius (.. js/d3.scale
;                   sqrt
;                   (domain #js [0 1e6])
;                   (range  #js [0 15] ))
;        update  (fn [comp]
;                  (let [mapclj (reagent/props comp)
;                        json (clj->js mapclj)
;                        svg  (.. js/d3 (select "svg"))]
;
;                    (.. svg (append "g")
;                        (attr "class" "legend")
;                        (attr "transform" (str "translate(" (- width 50) "," (- height 20) ")"))
;                        (selectAll "g")
;                        (data #js [1e6 5e6 1e7])
;                        enter
;                        (append "g"))
;
;                    (.. svg
;                        (append "path")
;                        (datum (.feature js/topojson json (.. json -objects -nation)))
;                        (attr "class" "land")
;                        (attr "d" path))
;
;                    (.. svg
;                        (append "g")
;                        (attr "class" "bubble")
;                        (selectAll "circle")
;                        (data (.. js/topojson
;                                  (feature json (.. json -objects -counties))
;                                  -features))
;                        (sort (fn [a b] (-  (.. b -properties -population) (.. a -properties -population))))
;                        enter
;                        (append "circle")
;                        (attr "transform" #(str "translate(" (.centroid path %) ")"))
;                        (attr "r" (fn [d] (radius (.. d -properties -population))))
;                        (append "title")
;                        (text (fn [d] (str (.. d -properties -name) " Population " (formatNumber (.. d -properties -population))))))
;                    ))]
;
;    (reagent/create-class
;      {:reagent-render (fn []
;                         [:div
;                          [:h2 "Awesome Map"]
;                          [:div#map [:svg {:style {:height (str  height "px") :width (str width "px")}}] ]])
;
;       :component-did-mount (fn [comp] (update comp))
;       :component-did-update update
;       :display-name "d3map-inner"})))


;(defn d3-bargraph-inner []
;  (let [width 960
;        height 200
;        formatNumber (fn [d]  ((.format js/d3 ",.01f") d))
;        path (.. js/d3.geo path (projection nil))
;        radius (.. js/d3.scale
;                   sqrt
;                   (domain #js [0 1e6])
;                   (range  #js [0 15] ))
;        update  (fn [comp]
;                  (let [mapclj (reagent/props comp)
;                        json (clj->js mapclj)
;                        svg  (.. js/d3 (select "svg"))]
;
;                    (.. svg (append "g")
;                        (attr "class" "legend")
;                        (attr "transform" (str "translate(" (- width 50) "," (- height 20) ")"))
;                        (selectAll "g")
;                        (data #js [1e6 5e6 1e7])
;                        enter
;                        (append "g"))
;
;                    (.. svg
;                        (append "path")
;                        (datum (.feature js/topojson json (.. json -objects -nation)))
;                        (attr "class" "land")
;                        (attr "d" path))
;
;                    (.. svg
;                        (append "g")
;                        (attr "class" "bubble")
;                        (selectAll "circle")
;                        (data (.. js/topojson
;                                  (feature json (.. json -objects -counties))
;                                  -features))
;                        (sort (fn [a b] (-  (.. b -properties -population) (.. a -properties -population))))
;                        enter
;                        (append "circle")
;                        (attr "transform" #(str "translate(" (.centroid path %) ")"))
;                        (attr "r" (fn [d] (radius (.. d -properties -population))))
;                        (append "title")
;                        (text (fn [d] (str (.. d -properties -name) " Population " (formatNumber (.. d -properties -population))))))
;                    ))]
;
;    (reagent/create-class
;      {:reagent-render (fn []
;                         [:div
;                          [:h2 "Awesome Map"]
;                          [:div#map [:svg {:style {:height (str  height "px") :width (str width "px")}}] ]])
;
;       :component-did-mount (fn [comp] (update comp))
;       :component-did-update update
;       :display-name "d3map-inner"})))

(defn NYCmap-outer []
  (let [mapdata (re-frame/subscribe [:mapdata])
        countdata (re-frame/subscribe [:countdata])]   ;; obtain the data
    (fn []
      [NYCmap-inner {:countdata @countdata :mapdata @mapdata}])))


;(defn simplecomponent []
;  (let [mapdata (re-frame/subscribe [:mapdata])]
;    (println @mapdata)
;    [:p (str (first @mapdata))]))

;(defn d3map-outer []
;  (let [mapdata (re-frame/subscribe [:mapdata])]   ;; obtain the data
;    (fn []
;      [d3map-inner (first @mapdata)])))

;(defn d3-bargraph-outer []
;  (let [mapdata (re-frame/subscribe [:mapdata])]   ;; obtain the data
;    (fn []
;      [d3map-inner (first @mapdata)])))

(let []
  (dispatch-sync [:initialize-mapdata])
  (dispatch-sync [:initialize-countdata])
  (reagent/render [NYCmap-outer] (. js/document (getElementById "app")))
  )


