(ns simian-flu.core
  (:require [clojure.math.combinatorics :refer [combinations]])
  (:import [javafx.stage.Stage]
           (java.io PrintWriter)))

;;; TODO
;; [x] Implement turn and move
;; [x] Implement ∪-prob
;; [x] Clean ∪-prob1..6: keep only one when finished playing with it
;; [x] Implement world, lab, humans and apes setup
;; [x] Implement evap agent
;; [x] Implement human/ape behavior in dosync
;; [x] Implement instant death
;; [x] Add JavaFX GUI, see: https://github.com/halgari/fn-fx
;; [/] Keep counter of living humans and stop simulation when all humans are dead
;; [ ] Define viral load decrease with rate (per-cycle) for non-dying humans
;; [ ] Implement slow death instead of instant death
;; [ ] Stop adding TODOs
;; [ ] Add cure and health facilities with "word-of-mouth" trails for directions
;; [ ] Add tombstones with exposure rate
;; [ ] Add radius proportional decay for air exposure rate
;; [ ] Seriously STOP adding TODOs
;; [ ] Add human roles: scientists go to the lab to find cure, cops kill apes
;; [ ] Distribute all the things!

(defn uuid [] (.toString (java.util.UUID/randomUUID)))
;(uuid)

(def logfile "/Users/arnaud/Lab/simian-flu/log/simian-flu.log")

(defn safe-println [& more]
  (with-open [w (clojure.java.io/writer logfile :append true)]
    (.write w (str "[" (.getName (Thread/currentThread)) "]" (clojure.string/join " " more) "\n"))))
;(safe-println "plop")

(defn ∩
  [a & more]
  (reduce * a more))
;(∩ 0.7)
;(∩ 0.7 0.5)

(defn ∪
  "https://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle#In_probability"
  [a & more]
  (let [ps (concat [a] more)
        n (count ps)]
    (loop [k 1 prev 0]
      (if (> k n)
        prev
        (let [mult (Math/pow -1 (+ k 1))
              cs (combinations ps k)
              intersections (map #(* mult (apply ∩ %)) cs)]
          (recur (inc k) (reduce + prev intersections)))))))
;(∪ 0.7)
;(∪ 0.7 0.5)

(defn lab-ranges
  "Returns the randomly generated x and y offsets ranges of the lab given
  the world and lab sizes."
  [world-size lab-size]
  (let [range-size (- world-size lab-size)]
    (take 2 (map #(range % (+ % lab-size))
                 (repeatedly #(rand-int (inc range-size)))))))
;(lab-ranges 100 5)

(defn bound
  "returns n wrapped into range 0-b"
  [b n]
  (let [n (rem n b)]
    (if (neg? n)
      (+ n b)
      n)))
;(bound 5 -21)
;(bound 5 3)
;(bound 4 0)

(def dir-delta {0 [0 -1]
                1 [1 -1]
                2 [1 0]
                3 [1 1]
                4 [0 1]
                5 [-1 1]
                6 [-1 0]
                7 [-1 -1]})

(defn delta-loc
  "Returns the location one step in the given dir. Note the world is a torus."
  [world-size [x y] dir]
  (let [[dx dy] (dir-delta (bound 8 dir))]
    [(bound world-size (+ x dx)) (bound world-size (+ y dy))]))
;(delta-loc 5 [0 0] 4)
;(delta-loc 5 [0 0] 7)
;(delta-loc 5 [4 3] 1)

(defn make-cell []
  (ref {:lab?     false
        :virus    0
        :occupant nil
        }))
;(deref (make-cell))

(defn genesis
  [world-size]
  (let [world-range (range world-size)]
    (mapv (fn [_]
            (mapv (fn [_]
                    (make-cell))
                  world-range))
          world-range)))
;(world 10)

(defn cell
  [world [x y]]
  (-> world (nth x) (nth y)))
;(-> (world 10)
;    (cell [5 5]) deref)

(defn make-ape
  [uuid dir]
  {:id            uuid
   :species       :ape
   :contaminated? true
   :alive?        true
   :dir           dir})
;(make-ape (uuid) 3)

(defn setup-apes
  [lab-ranges]
  (doall
    (for [x (first lab-ranges) y (second lab-ranges)]
      [x y])))
;(setup-apes [[2 3] [2 3]])
;(contains? (set (setup-apes [[2 3] [2 3]])) [2 3])

(defn make-human
  [uuid dir]
  {:id            uuid
   :species       :human
   :contaminated? false
   :alive?        true
   :dir           dir})
;(make-human (uuid) 4)

(defn setup-humans
  [x-gen y-gen apes-coords nhumans]
  (loop [humans #{} nhumans nhumans]
    (if (> nhumans 0)
      (let [x (x-gen)
            y (y-gen)
            coord [x y]]
        (if (or (contains? apes-coords coord)
                (contains? humans coord))
          (recur humans nhumans)
          (recur (conj humans coord) (dec nhumans))))
      humans)))
;(setup-humans #(rand-int 10) #(rand-int 10) #{[2 2] [2 3] [3 3] [3 2]} 5)

(defn setup!
  "Initialize apes and humans onto the 2D world."
  [world lab-ranges nhumans]
  (dosync
    (let [apes (set (setup-apes lab-ranges))
          xy-gen #(rand-int (count world))
          dir-gen #(rand-int 8)
          humans (setup-humans xy-gen xy-gen apes nhumans)]
      ; Setup apes
      (doseq [coord apes]
        (alter (cell world coord)
               assoc
               :lab? true
               :virus 1
               :occupant (make-ape (uuid) (dir-gen))))
      ; Setup humans
      (doseq [coord humans]
        (alter (cell world coord)
               assoc
               :lab? false
               :virus 0
               :occupant (make-human (uuid) (dir-gen))))
      ; Return agents for apes and humans
      {:world  world
       :agents (map agent (concat apes humans))})))

;(let [world-size 10
;      lab-size 2
;      nhumans 5
;      {:keys [world agents]} (setup! world-size lab-size nhumans)]
;  (map (comp #(cell world %)
;             deref)
;       agents))

(defn turn
  "Returns the function that must be applied to a cell's value to turn its
  occupant by the given number of \"ticks\" and return its new state."
  [amount]
  (letfn [(update-occupant [occupant]
            (safe-println (:id occupant) "turned by" amount)
            (update occupant :dir #(bound 8 (+ % amount))))]
    #(update % :occupant update-occupant)))
;((turn 2) {:occupant {:dir 3}})
;((apply comp
;        [(turn 1) (turn 2)])
;  {:occupant {:dir 1}})

(defn rand-turn []
  (turn (if (even? (rand-int 2)) -1 1)))

(def u-turn (turn 4))
;(u-turn {:occupant {:dir 3}})
;((apply comp
;        [u-turn u-turn])
;  {:occupant {:dir 1}})

(defn move!
  "Move the human or ape at coord to the direction it is heading.
  Must be called in a transaction."
  [world coord new-coord indirect-exp-prob]
  (let [old-c (cell world coord)
        occupant (:occupant @old-c)
        new-c (cell world new-coord)]
    (alter old-c dissoc :occupant)
    (alter new-c assoc :occupant occupant)
    ; Add-up to the virus load of the cell if the last occupant was contaminated
    (when (and (not (:lab? @old-c))
               (:contaminated? occupant))
      (alter old-c update :virus + indirect-exp-prob))
    (safe-println (:id occupant) "moved from" coord "to" new-coord)
    new-coord))

(defn get-human-prob
  "Return the probability of direct contamination by another human."
  [neighbour human-prob]
  (if (:contaminated? neighbour)
    human-prob
    0))

(defn get-airborne-prob
  "Return the probability of airborne contamination
  by evaluating the contamination status of the occupants (if any)
  around the given cell inside the air spreading radius."
  [world [x y] radius airborne-prob]
  (let [airborne (for [x (remove #(= x %) (range (- x radius) (+ x radius 1)))
                       y (remove #(= y %) (range (- y radius) (+ y radius 1)))
                       :when (->> [(bound (count world) x) (bound (count world) y)]
                                  (cell world)
                                  deref
                                  :occupant
                                  :contaminated?
                                  )]
                   airborne-prob)]
    (apply ∪ (if (empty? airborne) [0] airborne))))
;(with-redefs-fn {#'cell (fn [_ _]
;                          (ref {:lab? true
;                                :virus 1
;                                :occupant (make-ape (uuid) 7)}))}
;  #(get-airborne-prob nil [0 0] 0 0.5))

(defn get-indirect-prob
  "Return the probability of indirect contamination by examining the
  viral load of the current cell."
  [world coord]
  (->> coord
       (cell world)
       deref
       :virus))
;(with-redefs-fn {#'cell (fn [_ _]
;                          (ref {:lab? true
;                                :virus 1
;                                :occupant (make-ape (uuid) 7)}))}
;  #(get-indirect-prob nil [0 0]))

(defn contaminate
  "Returns the function that must be applied to a cell's value to decide if its
  occupant lives or dies and return its new state."
  [gets-contaminated? dies?]
  (letfn [(contaminate-occupant [occupant]
            (if (gets-contaminated?)
              (if (dies?)
                (do (safe-println (:id occupant) "killing")
                    (assoc occupant :contaminated? true :alive? false))
                (do (safe-println (:id occupant) "immunizing")
                    (assoc occupant :immune? true)))
              occupant))]
    #(update % :occupant contaminate-occupant)))
;((contaminate (constantly true) (constantly true)) {:occupant {:dir 2}})
;((contaminate (constantly true) (constantly false)) {:occupant {:dir 2}})
;((contaminate (constantly false) nil) {:occupant {:dir 2}})
;((contaminate #(< (rand) (∪-prob 0.7 0.5)) #(< (rand) 0.8)) {:occupant {:dir 2}})
;((apply comp
;        [(turn 1) (contaminate (constantly true) (constantly true))])
;  {:occupant {:dir 1}})

(defn human-flee
  [coord {:keys [direct-exp-prob death-ratio]}]
  (let [contaminate (contaminate #(< (rand) direct-exp-prob)
                                 #(< (rand) death-ratio))]
    {:alter-fn!   (apply comp [u-turn contaminate])
     :next-coord coord}))

(defn human-encounter
  [world coord neighbour
   {:keys [human-exp-prob air-spreading-radius air-exp-prob death-ratio]}]
  (let [prob (∪ (get-human-prob neighbour human-exp-prob)
                (get-airborne-prob world coord air-spreading-radius air-exp-prob)
                (get-indirect-prob world coord))]
    {:alter-fn!   (apply comp [(if (:contaminated? neighbour) u-turn (rand-turn))
                               (contaminate #(< (rand) prob)
                                            #(< (rand) death-ratio))
                               ])
     :next-coord coord}))

(defn human-move-on
  [world coord ahead {:keys [air-spreading-radius air-exp-prob death-ratio]}]
  (let [prob (∪ (get-airborne-prob world coord air-spreading-radius air-exp-prob)
                (get-indirect-prob world coord))]
    {:alter-fn!   (contaminate #(< (rand) prob)
                               #(< (rand) death-ratio))
     :next-coord ahead}))

(defn ape-turn
  [coord]
  {:alter-fn!   (rand-turn)
   :next-coord coord})

(defn ape-move-on
  [ahead]
  {:alter-fn!   identity
   :next-coord ahead})

(defn get-action
  [world coord occupant {:keys [world-size] :as config}]
  (safe-println (:id occupant) "isa" (:species occupant) "at" coord)
  (let [ahead (delta-loc world-size coord (:dir occupant))
        {neighbour :occupant lab? :lab?} (deref (cell world ahead))]
    (if (and (= :human (:species occupant)) (not (:immune? occupant)))
      ; Humans wander and 'try' to survive, see TODOs for possible roles
      (cond
        ; The lab or an ape is ahead. Run!
        (or lab? (= :ape (:species neighbour)))
        (human-flee coord config)
        ; A human is ahead
        (and (= :human (:species neighbour)) (:alive? neighbour))
        (human-encounter world coord neighbour config)
        ; No one alive ahead let's move
        :else
        (human-move-on world coord ahead config))
      ; Apes and immune humans just move around and never die... see TODOs
      (if (:alive? neighbour)
        ; The way is shut! Turn.
        (ape-turn coord)
        ; The way is clear, move on.
        (ape-move-on ahead)))))

(defn at-the-edge [coord world-size]
  (let [[x y] coord
        limit (dec world-size)]
    (or (= x 0)
        (= y 0)
        (= x limit)
        (= y limit))))

(defn behaviour
  "Define the behaviour of apes and humans evolving in the (n*n) world.
  Returns a function that will apply the defined behavior to an ape or human agent."
  [world
   {:keys [indirect-exp-prob
           running
           agent-sleep-ms
           ;alive-cnt
           ]
    :as config}]
  (fn behave! [coord]
    (let [place (cell world coord)
          occupant (:occupant @place)
          ]
      (Thread/sleep agent-sleep-ms)
      ; Dead things do not behave
      (if (:alive? occupant)
        ; TODO Handle distribution here
        ;(if (at-the-edge coord world-size)
        ;  (dosync
        ;    (safe-println (:id occupant) (if (= :human (:species occupant)) "human" "ape") "at the edge, farewell")
        ;    (when (= :human (:species occupant))
        ;      (alter alive-cnt dec)))
        (do
          (when @running
            (send-off *agent* behave!))
          (dosync
            (let [{:keys [alter-fn! next-coord]} (get-action world coord occupant config)]
              ; get some state before altering cell
              ;(let [isa-human? (= :human (get-in @place [:occupant :species]))
              ;      was-immune? (get-in @place [:occupant :immune?])]
              ; alter cell
              (alter place alter-fn!)
              ;; update counter with new state TODO use add-watch on ref cells
              ;(let [is-immune? (get-in @place [:occupant :immune?])
              ;      is-dead? (not (get-in @place [:occupant :alive?]))]
              ;  (when (and isa-human? is-dead?)
              ;    (alter alive-cnt dec)) ;TODO commute
              ;  (when (and isa-human? (not was-immune?) is-immune?)
              ;    (alter alive-cnt dec))))
              ; If action led to a coord change then move!
              (when (not (identical? coord next-coord))
                (move! world coord next-coord indirect-exp-prob))
              next-coord)))
        ;)
        ;(dosync
        ;; TODO tombstone?
        ;(alter place assoc :occupant nil)
        (safe-println (:id occupant) "dead")
        ;)
        ))))

(defn evaporation
  [world world-size {:keys [viral-load-dec running evap-sleep-ms]}]
  (fn evaporate! [_]
    (Thread/sleep evap-sleep-ms)
    (when @running
      (send-off *agent* evaporate!))
    (dorun
      (for [x (range world-size) y (range world-size)]
        (dosync
          (let [c (cell world [x y])]
            ; Viral load constant in the lab but decreases elsewhere
            (when-not (:lab? @c)
              (alter c update :virus * viral-load-dec))))))
    nil))

;;; The simulation

(def world-size 20)

; All apes originate from the lab
(def lab-size 1)

; Number of humans to start with
(def nhumans 40)

; Death ratio
(def death-ratio 0.75) ; was 0.9

; Probability of contamination by direct exposure with an ape or the lab
(def direct-exp-prob 0.5) ; was 1

; Probability of contamination by direct contact with a contaminated human
(def human-exp-prob 0.4) ; was 0.7

; Probability of contamination by air exposure inside spreading radius of a contaminated being
(def air-exp-prob 0.3) ; was 0.5

; Air spreading radius
(def air-spreading-radius 2)

; Probability of contamination by indirect exposure
(def indirect-exp-prob 0.2) ; was 0.3

; Decrease rate of virus load
(def viral-load-dec 0.99)

(def running (atom true))

(def agent-sleep-ms 500)

(def evap-sleep-ms 1000)

; Test simulation for 5s
(comment let [world (genesis world-size)
              lab-ranges (lab-ranges world-size lab-size)
              evaporator (agent nil)
              alive-cnt (ref nhumans)]
         (let [{:keys [world agents]} (setup! world lab-ranges nhumans)
               behave! (behaviour world
                                  {:world-size           world-size
                                   :death-ratio          death-ratio
                                   :direct-exp-prob      direct-exp-prob
                                   :indirect-exp-prob    indirect-exp-prob
                                   :human-exp-prob       human-exp-prob
                                   :air-spreading-radius air-spreading-radius
                                   :air-exp-prob         air-exp-prob
                                   :running              running
                                   :agent-sleep-ms       agent-sleep-ms
                                   :alive-cnt            alive-cnt
                                   })
               evaporate! (evaporation world
                                       world-size
                                       {:viral-load-dec viral-load-dec
                                        :running        running
                                        :evap-sleep-ms  evap-sleep-ms})]
           (with-open [w (clojure.java.io/writer logfile)]
             (.write w "Start\n"))
           (reset! running true)
           (dorun (map #(send-off % behave!) agents))
           (send-off evaporator evaporate!)
           ))

(comment do
         (reset! running false)
         (safe-println "Stop\n"))

; UI

(require
  '[fn-fx.fx-dom :as dom]
  '[fn-fx.diff :refer [component defui render should-update?]]
  '[fn-fx.controls :as ui])

;pixels per world cell
(def scale 20.0)

(defui UiCell
       (render [this {:keys [x y occupant virus] :as cell}]
               (let [children [(ui/polygon
                                 :style-class ["cell"]
                                 :style (str "-fx-fill: rgba(255,0,0," virus ")")
                                 :points [0.0 0.0
                                          scale 0.0
                                          scale scale
                                          0.0 scale])]
                     children (if-not (and occupant (:alive? occupant))
                                children
                                (conj children
                                      (ui/polygon
                                        :rotate ({0 0
                                                  1 45
                                                  2 90
                                                  3 135
                                                  4 180
                                                  5 -135
                                                  6 -90
                                                  7 -45}
                                                  (:dir occupant 0))
                                        :style-class [(name (:species occupant "")) (if (= (:species occupant) :human) (str "human-" (if (:immune? occupant) "immune" (if (:contaminated? occupant) "infected" "sane"))) "")]
                                        :points (let [r (/ scale 2)
                                                      q (/ scale 12)
                                                      y (- r q)
                                                      d (Math/sqrt (- (Math/pow r 2) (Math/pow (* y -1) 2)))]
                                                  [r (* 2 q),
                                                   d (+ r (- y q)),
                                                   (+ r d) (+ r (- y q))]))))
                     ]
                 (ui/group
                   :layout-x x
                   :layout-y y
                   :children children))))

(defui Simulation
       (render [this {:keys [cells] :as args}]
               (ui/group
                 :children (for [cell cells]
                             (ui-cell cell)))))

(defui SimianFlu
       (render [this args]
               (ui/stage
                 :title "Simian Flu"
                 :shown true
                 :scene (ui/scene
                          :width (* scale world-size)
                          :height (* scale world-size)
                          :stylesheets ["ui.css"]
                          :root (simulation args)))))

(defn -main []
  (let [w (genesis world-size)
        lab-ranges (lab-ranges world-size lab-size)
        {:keys [world agents]} (setup! w lab-ranges nhumans)
        evaporator (agent nil)
        alive-cnt (ref nhumans)
        behave! (behaviour world
                           {:world-size           world-size
                            :death-ratio          death-ratio
                            :direct-exp-prob      direct-exp-prob
                            :indirect-exp-prob    indirect-exp-prob
                            :human-exp-prob       human-exp-prob
                            :air-spreading-radius air-spreading-radius
                            :air-exp-prob         air-exp-prob
                            :running              running
                            :agent-sleep-ms       agent-sleep-ms
                            :alive-cnt            alive-cnt
                            })
        evaporate! (evaporation world
                                world-size
                                {:viral-load-dec viral-load-dec
                                 :running        running
                                 :evap-sleep-ms  evap-sleep-ms})

        ;; Data State holds the business logic of our app
        data-state (atom nil)
        _ (reset! data-state {:cells (into [] (for [x (range world-size) y (range world-size)]
                                                (let [r (get-in world [x y])]
                                                  (add-watch r :world (fn [_ _ _ n]
                                                                        (swap! data-state assoc-in
                                                                               [:cells (+ (* x world-size) y)]
                                                                               (assoc n :x (* x scale) :y (* y scale)))))
                                                  (assoc @r :x (* x scale) :y (* y scale)))))})

        ;; handler-fn handles events from the ui and updates the data state
        handler-fn (fn [event]
                     (try
                       (comment swap! data-state handle-event event)
                       (catch Throwable ex
                         (safe-println ex))))

        ;; ui-state holds the most recent state of the ui
        ui-state   (agent (dom/app (simian-flu @data-state) handler-fn))]

    (dorun (map #(send-off % behave!) agents))
    (send-off evaporator evaporate!)

    ;; Every time the data-state changes, queue up an update of the UI
    (add-watch data-state :ui (fn [_ _ _ _]
                                (send ui-state
                                      (fn [old-ui]
                                        (try
                                          (dom/update-app old-ui (simian-flu @data-state))
                                          (catch Throwable ex
                                            (safe-println ex)))))))
    ))

(do
  (with-open [w (clojure.java.io/writer logfile)]
    (.write w "Start\n"))
  (reset! running true)
  (-main)
  (Thread/sleep 30000)
  (reset! running false)
  (safe-println "Stop\n"))