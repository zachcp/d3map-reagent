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
(def dbstart {:countdata {:data [{:samplename "DFD_1128.1",
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
                                    :size       5}]}

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
    (assoc db :countdata {:data counts}))))    ;; what it returns becomes the new state

(register-handler
  :update-active
  (fn
    [db [_ molvalue]]
    (let [countdata (-> db :countdata :data)
          newvalues (map #(calculate-countdata % molvalue) countdata)]
      (println countdata)
      (println newvalues)
    (-> db (assoc :activemolecule molvalue)
           (assoc :countdata {:data newvalues})))))  ;; what it returns becomes the new state


;;;----Subscription Handlers -----------
(register-sub
  :countdata
  (fn
    [db _]
    (reaction (-> @db :countdata :data))))


(register-sub
  :molecules
  (fn
    [db _]
    (let [countdata (reaction (-> @db :countdata :data))
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

(defn draw-points [svg counts]
  "use d3.json to load the json file and crete the map in the svg element usin the path"
  (.. svg
      (append "g")
      (attr "id" "circle")
      (selectAll "circle")
      (data counts)
      enter
      (append "svg:circle")
      (attr "cx" (fn [d] (.-d3lat d)))
      (attr "cy" (fn [d] (.-d3lon d)))
      (attr "r"  (fn [d] (.-size d)))
      (attr "class" "bubble")
      ))


(defn molecule-select-box []
  ""
  (let [molecules (re-frame/subscribe [:molecules])]
    (fn []
      [:select {:on-change #(re-frame/dispatch [:update-active (-> % .-target .-value)])}
       (for [m @molecules]
         [:option {:value  (str m)} (str m)])])))

(defn updatefn
  ([comp]
   (let [countdata (-> comp reagent/props :test :data)]
      (println countdata)
      (.. js/d3
          (selectAll "circle")
          (data (clj->js countdata))
          (attr "r"  (fn [d] (.-size d))))
      (println "In the update function"))))


(defn NYCmap-inner []
  (let [width 960
        height 500
        projection (.. js/d3.geo
                       mercator
                       (center #js [-73.94 40.7])
                       (scale 50000)
                       (translate #js [(/ width 2) (/ height 2)]))

        path (.. js/d3.geo path (projection projection))

        getlatlon (fn [datamap] (let [latitude (:Latitude datamap)
                                      longitude (:Longitude datamap)
                                      [lon lat] (js->clj (projection #js [longitude latitude])) ]
                                  (merge datamap {:d3lat lon  :d3lon lat})))]
    (reagent/create-class
      {:reagent-render (fn []
                         [:div [:h2 "Awesome NYC Map"]
                               [:div#map [molecule-select-box]
                                [:svg {:style {:height (str  height "px") :width (str width "px")}}]]])

       :component-did-mount (fn [comp]
                              (let [countdata (-> comp reagent/props :test :data)
                                    countdata-updated (map getlatlon countdata)
                                    svg  (.. js/d3 (select "svg"))]
                                (println countdata)
                                (do (load-map "js/nyc.json" svg path))
                                    (draw-points svg (clj->js countdata-updated))))

       :component-did-update updatefn
       :display-name "d3map-inner"})))



(defn NYCmap-outer []
  (let [countdata (re-frame/subscribe [:countdata])
        activemol (re-frame/subscribe [:activemolecule])]
    (fn []
      [:div
       [NYCmap-inner {:test @countdata} ]
       (when @activemol [:h3 (str "Activemol: " @activemol)])])))

(let []
  (dispatch-sync [:initialize-db])
  ;(dispatch-sync [:initialize-countdata])
  (reagent/render-component [NYCmap-outer] (. js/document (getElementById "app"))))
