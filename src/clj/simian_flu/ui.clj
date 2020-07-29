(ns simian-flu.ui
  (:require [cljfx.api :as fx]
            [cljfx.css :as css]))

(def style
  (css/register ::style
    (merge
      {".cell"  {:-fx-background-color "white"}
       ".human" {}
       ".ape"   {:-fx-fill "darkred"}}
      (into {} (for [[state color] '(["sane" "lime"] ["infected" "red"] ["immune" "cyan"])]
                 [(str ".human-" state)
                  {:-fx-fill color}])))))

(def cell-points
  (memoize
    (fn [scale] [0.0 0.0
                 scale 0.0
                 scale scale
                 0.0 scale])))

(def marker
  (memoize
    (fn [scale]
      (let [r (/ scale 2)
            q (/ scale 12)
            y (- r q)
            d (Math/sqrt (- (Math/pow r 2) (Math/pow (* y -1) 2)))]
        [r (* 2 q),
         d (+ r (- y q)),
         (+ r d) (+ r (- y q))]))))

(def angles {0 0
             1 45
             2 90
             3 135
             4 180
             5 -135
             6 -90
             7 -45})

(defn grid-cell [{:keys [cell scale]}]
  (let [{:keys [x y occupant virus]} cell
        children [{:fx/type     :polygon
                   :style-class ["cell"]
                   :style       (str "-fx-fill: rgba(255,0,0," virus ")")
                   :points      (cell-points scale)}]
        children (if-not (and occupant (:alive? occupant))
                   children
                   (conj children
                     {:fx/type     :polygon
                      :rotate      (angles (:dir occupant 0))
                      :style-class [(name (:species occupant "")) (if (= (:species occupant) :human) (str "human-" (if (:immune? occupant) "immune" (if (:contaminated? occupant) "infected" "sane"))) "")]
                      :points      (marker scale)}))]
    {:fx/type  :group
     :layout-x x
     :layout-y y
     :children children}))

#_(defn title-input [{:keys [title]}]
    {:fx/type         :text-field
     :on-text-changed (swap! *state assoc :title %)
     :text            title})

(defn simulation-pane
  [{:keys [cells scale]}]
  {:fx/type  :group
   :children (for [cell cells]
               (grid-cell {:cell cell :scale scale}))})

(defn root [{:keys [title width height] :as state}]
  {:fx/type :stage
   :showing true
   :title   title
   :width   width
   :height  height
   :scene   {:fx/type :scene
             :stylesheets [(::css/url style)]
             :root    (simulation-pane state)}})

#_(defn map-event-handler [event]
    (case (:event/type event)
      ::set-title (swap! *state assoc :title (:fx/event event))))

(defn show [state]
  (fx/mount-renderer
    state
    (fx/create-renderer
      :middleware (fx/wrap-map-desc assoc :fx/type root)
      ;:opts {:fx.opt/map-event-handler map-event-handler}
      )))

(comment
  (show (atom {:title  "Simian Flu Pandemic Simulation"
               :width  800
               :height 600
               }))
  )
