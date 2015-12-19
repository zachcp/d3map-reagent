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

;;;---- Basic DB ----------------
(def dbstart {:countdata  [{:samplename "DFD_1128.1",
                            :Longitude  -73.9185,
                            :Latitude   40.5879,
                            :totalreads 50095,
                            :countdata  {"A40926"       0,
                                         "A47934"       63,
                                         "A54145"       0,
                                         "actinomycin"  0,
                                         "aculeximycin" 0}
                            :size       5}
                           {:samplename "DFD_1128.1",
                            :Longitude  -73.8185,
                            :Latitude   40.7879,
                            :totalreads 50095,
                            :countdata  {"A40926"       0,
                                         "A47934"       63,
                                         "A54145"       0,
                                         "actinomycin"  0,
                                         "aculeximycin" 0}
                            :size       5}]

                         :molecules ["A40926"
                                     "A47934"
                                     "A54145"
                                     "actinomycin"
                                     "aculeximycin"]

                         :activemolecule nil
                         :normalized true
              })

;;;---- Helper Functions-----------
(defn calculate-countdata [sample molecule]
  "calculate the counts of given molecule"
  (let [molnum (get-in sample [:countdata molecule])
        sampletotal (:totalreads sample)]
    (assoc sample :size (/ molnum sampletotal))))


;;;---- Event handlers-----------
(register-handler                 ;; setup initial state
  :initialize-db                     ;; usage:  (submit [:initialize])
  (fn
    [_ _]
   dbstart))

(register-handler                 ;; setup initial state
  :initialize-countdata                     ;; usage:  (submit [:initialize])
  (fn
    [db _]
    (GET "js/counts.json" {:response-format :json
                           :handler         #(re-frame/dispatch [:process-countdata %1])})
    db))

(register-handler                 ;; setup initial state
  :process-countdata                ;; usage:  (submit [:initialize])
  (fn
    [db [_ resp]]
    (let [counts (js->clj resp :keywordize-keys true)]
    (assoc db :countdata counts))))    ;; what it returns becomes the new state

(register-handler
  :update-active
  (fn
    [db [_ molvalue]]
    (let [countdata (-> db :countdata)
          newvalues (map #(calculate-countdata % molvalue) countdata)]
      (println countdata)
      (println newvalues)
    (-> db (assoc :activemolecule molvalue)
           (assoc :countdata newvalues)))))  ;; what it returns becomes the new state


;;;----Subscription Handlers -----------
(register-sub
  :countdata
  (fn
    [db _]
    (reaction (-> @db :countdata))))


(register-sub
  :molecules
  (fn
    [db _]
    (let [countdata (reaction (-> @db :countdata ))
          molecules (reaction (-> @countdata first :countdata keys))]
      molecules)))

(register-sub
  :activemolecule
  (fn
    [db _]                       ;; db is the app-db atom
    (reaction (:activemolecule @db))))    ;; wrap the computation in a reaction

;;;---------------


(defn load-map [file svg path]
  "use d3.json to load the json file and crete the map in the svg element usin the path"
  (.json js/d3 file
    (fn [error json]
      (.. svg (append "g")
          (attr "id" "boroughs")
          (selectAll ".state")
          (data (.-features json))
          enter
          (append "path")
          (attr "class" (fn [d] (str "borough " (.. d -properties -borough))))
          (attr "d" path)
          (style "fill" (fn [d]  (.-colors d)))))))


(defn molecule-select-box []
  "the select box to choose the active molecules"
  (let [molecules (re-frame/subscribe [:molecules])]
    (fn []
      [:select {:on-change #(re-frame/dispatch [:update-active (-> % .-target .-value)])}
       (for [m @molecules]
         [:option {:value  (str m)} (str m)])])))

(defn NYCmap-inner [countdata]
  (let [width 960
        height 500
        projection (.. js/d3.geo mercator
                       (center #js [-73.94 40.7])
                       (scale 50000)
                       (translate #js [(/ width 2) (/ height 2)]))

        path (.. js/d3.geo path (projection projection))

        getlatlon (fn [datamap] (let [latitude (:Latitude datamap)
                                      longitude (:Longitude datamap)
                                      [lon lat] (js->clj (projection #js [longitude latitude])) ]
                                  (merge datamap {:d3lat lon  :d3lon lat})))]
    (reagent/create-class
      {:reagent-render (fn [countdata]
                         [:div
                          [:h2 "Awesome NYC Map"]
                          [:div#map
                           [molecule-select-box]
                           [:svg {:style {:height (str  height "px") :width (str width "px")}}]]])

       :component-did-mount
                        (fn [this]
                            (let [[_ countdata] (reagent/argv this)
                                  countdata-updated (map getlatlon countdata)
                                  pointdata (clj->js countdata-updated)
                                  radius (.. js/d3.scale sqrt (domain #js [0 5]) (range #js [0 10]))
                                  svg (.. js/d3 (select "svg"))]
                              ;; draw the map (only happens once)
                              (load-map "js/nyc.json" svg path)

                              ;; create and draw initial points
                              (.. svg
                                  (append "g")
                                  (attr "id" "circle")
                                  (selectAll "circle")
                                  (data pointdata)
                                  enter
                                  (append "svg:circle")
                                  (attr "cx" (fn [d] (.-d3lat d)))
                                  (attr "cy" (fn [d] (.-d3lon d)))
                                  (attr "r"  (fn [d] (radius (.-size d))))
                                  (attr "class" "bubble"))

                              ;; create the legend
                              (.. svg
                                  (append "g")
                                  (attr "class" "legend")
                                  (attr "transform" (str "translate(" (- width 50) "," (- height 20) ")")))))

       :component-did-update
                       (fn [this]
                         (let [[_ countdata] (reagent/argv this)
                               pointdata (clj->js countdata)
                               maxradius (first (max (map #(:size %) countdata)))
                               radius (.. js/d3.scale sqrt (domain (clj->js [0 maxradius])) (range #js [0 20]))
                               legend-data (clj->js [0 (/ 10 maxradius) maxradius])
                               legend (.. js/d3
                                          (select ".legend")
                                          (selectAll "g")
                                          (data legend-data)
                                          enter
                                          (append "g"))]

                           ;; redraw based on size
                           (.. js/d3
                               (selectAll "circle")
                               (data pointdata)
                               (attr "r"  (fn [d] (radius (.-size d)) )))


                           ;; update the legend
                           (.. legend
                               (append "circle")
                               (attr "cy" (fn [d] (- (radius d))))
                               (attr "r"  (fn [d] (radius d))))

                           (.. legend
                               (append "text")
                               (attr "y"   (fn [d] (* -2 (radius d))))
                               (attr "dy"  "1.3em")
                               (text (fn [d] ((.format js/d3 "0.01s") d))))
                           ))

       :display-name "d3map-inner"})))


(defn NYCmap-outer []
  (let [countdata (re-frame/subscribe [:countdata])
        activemol (re-frame/subscribe [:activemolecule])]
    (fn []
      [:div
       [NYCmap-inner @countdata ]
       (when @activemol [:h3 (str "Activemol: " @activemol)])])))

(let []
  (dispatch-sync [:initialize-db])
  ;(dispatch-sync [:initialize-countdata])
  (reagent/render-component [NYCmap-outer] (. js/document (getElementById "app"))))
