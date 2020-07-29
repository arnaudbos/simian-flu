(ns simian-flu.core
  (:require [clojure.math.combinatorics :refer [combinations]]
            [simian-flu.ui :as ui])
  (:import [java.util UUID]))

;;; TODO
;; [x] Implement turn and move
;; [x] Implement ∪-prob
;; [x] Clean ∪-prob1..6: keep only one when finished playing with it
;; [x] Implement world, lab, humans and apes setup
;; [x] Implement evap agent
;; [x] Implement human/ape behavior in dosync
;; [x] Implement instant death
;; [x] Add JavaFX GUI (https://github.com/cljfx/cljfx)
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

(defn uuid [] (.toString (UUID/randomUUID)))
(comment
  (uuid)
  )

(def logfile "/Users/arnaud/Lab/stm/simian-flu/log/simian-flu.log")

(defn safe-println [& more]
  (with-open [w (clojure.java.io/writer logfile :append true)]
    (.write w
      (str
        "[" (.getName (Thread/currentThread)) "]"
        (clojure.string/join " " more)
        "\n"))))
(comment
  (safe-println "plop")
  )

(defn ∩
  [a & more]
  (reduce * a more))
(comment
  (∩ 0.7)

  (∩ 0.7 0.5)
  )

(defn ∪
  "https://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle#In_probability"
  [a & more]
  (let [ps (concat [a] more)
        n  (count ps)]
    (loop [k 1 prev 0]
      (if (> k n)
        prev
        (let [mult          (Math/pow -1 (+ k 1))
              cs            (combinations ps k)
              intersections (map #(* mult (apply ∩ %)) cs)]
          (recur (inc k) (reduce + prev intersections)))))))
(comment
  (∪ 0.7)
  (∪ 0.7 0.5)
  )

(defn lab-ranges
  "Returns the randomly generated x and y offsets ranges of the lab given
  the world and lab sizes."
  [world-size lab-size]
  (let [range-size (- world-size lab-size)]
    (take 2 (map #(range % (+ % lab-size))
              (repeatedly #(rand-int (inc range-size)))))))
(comment
  (lab-ranges 100 5)
  )

(defn bound
  "returns n wrapped into range 0-b"
  [b n]
  (let [n (rem n b)]
    (if (neg? n)
      (+ n b)
      n)))
(comment
  (bound 5 -21)
  (bound 5 3)
  (bound 4 0)
  )

(def dir-delta {0 [0 -1]
                1 [1 -1]
                2 [1 0]
                3 [1 1]
                4 [0 1]
                5 [-1 1]
                6 [-1 0]
                7 [-1 -1]})

(defn delta-loc
  "Returns the location one step in the given dir.
   Note the world is a torus."
  [world-size [x y] dir]
  (let [[dx dy] (dir-delta (bound 8 dir))]
    [(bound world-size (+ x dx)) (bound world-size (+ y dy))]))
(comment
  (delta-loc 5 [0 0] 4)
  (delta-loc 5 [0 0] 7)
  (delta-loc 5 [4 3] 1)
  )

(defn make-cell [lab? occupant]
  {:lab?     lab?
   :virus    (ref (if lab? 1 0))
   :occupant (ref occupant)})
(comment
  (deref (:virus (make-cell true "me")))
  )

(defn world
  [world-size init-cell-fn]
  (let [world-range (range world-size)]
    (mapv (fn [x]
            (mapv (fn [y]
                    (init-cell-fn x y))
              world-range))
      world-range)))
(comment
  (letfn [(dummy-init-cell-fn [x y] (if (= x y) '| '_))]
    (world 10 dummy-init-cell-fn))
  )

(defn cell
  [world [x y]]
  (-> world (nth x) (nth y)))
(comment
  (letfn [(dummy-init-cell-fn [x y] (if (= x y) '| '_))]
    (-> (world 10 dummy-init-cell-fn)
      (cell [5 5])))
  )

(defn make-ape
  [uuid dir]
  {:id            uuid
   :species       :ape
   :contaminated? true
   :alive?        true
   :dir           dir})
(comment
  (make-ape (uuid) 3)
  )

(defn setup-apes
  [lab-ranges]
  (set
    (for [x (first lab-ranges) y (second lab-ranges)]
      [x y])))
(comment
  (setup-apes [[2 3] [2 3]])
  (contains? (set (setup-apes [[2 3] [2 3]])) [2 3])
  )

(defn make-human
  [uuid dir]
  {:id            uuid
   :species       :human
   :contaminated? false
   :alive?        true
   :dir           dir})
(comment
  (make-human (uuid) 4)
  )

(defn setup-humans
  [x-gen y-gen apes-coords nhumans]
  (loop [humans #{} nhumans nhumans]
    (if (> nhumans 0)
      (let [x     (x-gen)
            y     (y-gen)
            coord [x y]]
        (if (or (contains? apes-coords coord)
              (contains? humans coord))
          (recur humans nhumans)
          (recur (conj humans coord) (dec nhumans))))
      humans)))
(comment
  (setup-humans
    #(rand-int 10)
    #(rand-int 10)
    #{[2 2] [2 3] [3 3] [3 2]}
    5)
  )

(defn genesis
  "Initialize apes and humans onto the 2D world."
  [world-size lab-ranges nhumans]
  (let [apes    (set (setup-apes lab-ranges))
        xy-gen  #(rand-int world-size)
        dir-gen #(rand-int 8)
        humans  (setup-humans xy-gen xy-gen apes nhumans)]
    ; Return agents for apes and humans
    {:world  (world world-size
               (fn [x y]
                 (cond
                   (contains? apes [x y])
                   (make-cell true (make-ape (uuid) (dir-gen)))

                   (contains? humans [x y])
                   (make-cell false (make-human (uuid) (dir-gen)))

                   :else
                   (make-cell false nil))))
     :agents (map agent (concat apes humans))}))
(comment
  (let [world-size 10
        lab-size   2
        lab-ranges (lab-ranges world-size lab-size)
        nhumans    5
        {:keys [world agents]} (genesis world-size lab-ranges nhumans)]
    (map (comp #(cell world %)
           deref)
      agents))
  )

(defn turn
  "Returns the function that must be applied to a cell's value to turn its
  occupant by the given number of \"ticks\" and return its new state."
  [amount]
  (letfn [(update-occupant [occupant]
            (safe-println (:id occupant) "turned by" amount)
            (update occupant :dir #(bound 8 (+ % amount))))]
    update-occupant))
(comment
  ((turn 2) {:dir 3})
  ((apply comp
     [(turn 1) (turn 2)])
   {:dir 1})
  )

(defn rand-turn []
  (turn (if (even? (rand-int 2)) -1 1)))

(def u-turn (turn 4))
(comment
  (u-turn {:dir 3})
  ((apply comp
     [u-turn u-turn])
   {:dir 1})
  )

(defn move!
  "Move the human or ape at coord to the direction it is heading.
  Must be called in a transaction."
  [world coord new-coord]
  (let [{:keys [occupant]} (cell world coord)
        new-c (cell world new-coord)]
    (ref-set (:occupant new-c) @occupant)
    (safe-println (:id @occupant) "moved from" coord "to" new-coord)
    (ref-set occupant nil)
    new-coord))
(comment
  (let [world [[{:occupant (ref '|)} {:occupant (ref nil)}]]]
    (dosync
      (move! world
        [0 0]
        [0 1]))
    world)
  )

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
                               :occupant
                               deref
                               :contaminated?)]

                   airborne-prob)]
    (apply ∪ (if (empty? airborne) [0] airborne))))
(comment
  (with-redefs-fn {#'cell (fn [_ _]
                            {:lab?     true
                             :virus    (ref 1)
                             :occupant (ref (make-ape (uuid) 7))})}
    #(get-airborne-prob [[0 0 0 0 0]
                         [0 0 0 0 0]
                         [0 0 0 0 0]
                         [0 0 0 0 0]
                         [0 0 0 0 0]]
       [0 0]
       1
       0.5))
  )

(defn get-indirect-prob
  "Return the probability of indirect contamination by examining the
  viral load of the current cell."
  [world coord]
  (->> coord
    (cell world)
    :virus
    deref))

(comment
  (with-redefs-fn {#'cell (fn [_ _]
                            {:lab?     true
                             :virus    (ref 0.8)
                             :occupant (ref (make-ape (uuid) 7))})}
    #(get-indirect-prob nil [0 0]))
  )

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
    contaminate-occupant))
(comment
  ((contaminate (constantly true) (constantly true)) {:dir 2})
  ((contaminate (constantly true) (constantly false)) {:dir 2})
  ((contaminate (constantly false) nil) {:dir 2})
  ((contaminate #(< (rand) (∪ 0.7 0.5)) #(< (rand) 0.8)) {:dir 2})
  ((apply comp
     [(turn 1) (contaminate (constantly true) (constantly true))])
   {:dir 1})
  )

(defn human-flee
  [coord {:keys [direct-exp-prob death-ratio]}]
  (let [contaminate (contaminate #(< (rand) direct-exp-prob)
                      #(< (rand) death-ratio))]
    {:update-occupant (apply comp [u-turn contaminate])
     :next-coord      coord}))

(defn human-encounter
  [world coord neighbour
   {:keys [human-exp-prob air-spreading-radius air-exp-prob death-ratio]}]
  (let [prob (∪ (get-human-prob neighbour human-exp-prob)
               (get-airborne-prob world coord air-spreading-radius air-exp-prob)
               (get-indirect-prob world coord))]
    {:update-occupant (apply comp [(if (:contaminated? neighbour) u-turn (rand-turn))
                                   (contaminate #(< (rand) prob)
                                     #(< (rand) death-ratio))])

     :next-coord      coord}))

(defn human-move-on
  [world coord ahead {:keys [air-spreading-radius air-exp-prob death-ratio]}]
  (let [prob (∪ (get-airborne-prob world coord air-spreading-radius air-exp-prob)
               (get-indirect-prob world coord))]
    {:update-occupant (contaminate #(< (rand) prob)
                        #(< (rand) death-ratio))
     :next-coord      ahead}))

(defn ape-turn
  [coord]
  {:update-occupant (rand-turn)
   :next-coord      coord})

(defn ape-move-on
  [ahead]
  {:update-occupant identity
   :next-coord      ahead})

(defn get-action
  [world coord occupant {:keys [world-size] :as config}]
  (safe-println (:id occupant) "isa" (:species occupant) "at" coord)
  (let [ahead     (delta-loc world-size coord (:dir occupant))
        {neighbour :occupant lab? :lab?} (cell world ahead)
        neighbour @neighbour]
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

;(defn at-the-edge [coord world-size]
;  (let [[x y] coord
;        limit (dec world-size)]
;    (or (= x 0)
;        (= y 0)
;        (= x limit)
;        (= y limit))))

(defn behaviour
  "Define the behaviour of apes and humans evolving in the (n*n) world.
  Returns a function that will apply the defined behavior to an ape or human agent."
  [world
   {:keys [indirect-exp-prob
           running
           agent-sleep-ms]
    ;alive-cnt

    :as   config}]
  (fn behave! [coord]
    (let [{:keys [lab? virus occupant]} (cell world coord)]
      (Thread/sleep agent-sleep-ms)
      ; Dead things do not behave
      (if (:alive? @occupant)
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
            ; Behave
            (let [{:keys [update-occupant next-coord]} (get-action world coord @occupant config)]
              ; get some state before altering cell
              ;(let [isa-human? (= :human (get-in @place [:occupant :species]))
              ;      was-immune? (get-in @place [:occupant :immune?])]
              ; alter cell
              (alter occupant update-occupant)
              ;; update counter with new state TODO use add-watch on ref cells
              ;(let [is-immune? (get-in @place [:occupant :immune?])
              ;      is-dead? (not (get-in @place [:occupant :alive?]))]
              ;  (when (and isa-human? is-dead?)
              ;    (alter alive-cnt dec)) ;TODO commute
              ;  (when (and isa-human? (not was-immune?) is-immune?)
              ;    (alter alive-cnt dec))))
              ; If action led to a coord change then move!
              (when (not (identical? coord next-coord))
                (move! world coord next-coord)
                ; Add-up to the virus load of the cell if the last occupant was contaminated
                (when (and (not lab?) (:contaminated? (deref (:occupant (cell world next-coord)))))
                  (commute virus + indirect-exp-prob)))
              next-coord)))
        ;)
        ;(dosync
        ;; TODO tombstone?
        ;(alter place assoc :occupant nil)
        (safe-println (:id occupant) "dead")))))
;)


(defn evaporation
  [world {:keys [world-size viral-load-dec running evap-sleep-ms]}]
  (fn evaporate! [_]
    (Thread/sleep evap-sleep-ms)
    (when @running
      (send-off *agent* evaporate!))
    (dorun
      (for [x (range world-size) y (range world-size)]
        (dosync
          (let [{:keys [lab? virus]} (cell world [x y])]
            ; Viral load constant in the lab but decreases elsewhere
            (when-not lab?
              (commute virus * viral-load-dec))))))
    nil))

;;; The simulation

(def world-size 50)

; All apes originate from the lab
(def lab-size 2)

; Number of humans to start with
(def nhumans 200)

; Death ratio
(def death-ratio 0.75)                            ; was 0.9

; Probability of contamination by direct exposure with an ape or the lab
(def direct-exp-prob 0.5)                         ; was 1

; Probability of contamination by direct contact with a contaminated human
(def human-exp-prob 0.4)                          ; was 0.7

; Probability of contamination by air exposure inside spreading radius of a contaminated being
(def air-exp-prob 0.3)                            ; was 0.5

; Air spreading radius
(def air-spreading-radius 2)

; Probability of contamination by indirect exposure
(def indirect-exp-prob 0.2)                       ; was 0.3

; Decrease rate of virus load
(def viral-load-dec 0.99)

(def running (atom true))

(def agent-sleep-ms 200)

(def evap-sleep-ms 500)

; Test simulation for 5s
;(let [lab-ranges (lab-ranges world-size lab-size)
;      evaporator (agent nil)
;      alive-cnt (ref nhumans)
;      {:keys [world agents]} (genesis world-size lab-ranges nhumans)
;      behave! (behaviour world
;                         {:world-size           world-size
;                          :death-ratio          death-ratio
;                          :direct-exp-prob      direct-exp-prob
;                          :indirect-exp-prob    indirect-exp-prob
;                          :human-exp-prob       human-exp-prob
;                          :air-spreading-radius air-spreading-radius
;                          :air-exp-prob         air-exp-prob
;                          :running              running
;                          :agent-sleep-ms       agent-sleep-ms
;                          :alive-cnt            alive-cnt
;                          })
;      evaporate! (evaporation world
;                              {:world-size           world-size
;                               :viral-load-dec viral-load-dec
;                               :running        running
;                               :evap-sleep-ms  evap-sleep-ms})]
;  (with-open [w (clojure.java.io/writer logfile)]
;    (.write w "Start\n"))
;  (reset! running true)
;  (dorun (map #(send-off % behave!) agents))
;  (send-off evaporator evaporate!)
;  )
;
;(do
;  (reset! running false)
;  (safe-println "Stop\n"))

(def ui-throttle-ms 100)

(def ui-throttle-cnt 1)

(defn ui-throttle
  [_]
  (Thread/sleep ui-throttle-ms)
  (when @running
    (send-off *agent* ui-throttle))
  (alter-var-root #'ui-throttle-cnt (constantly 0))
  nil)

(defn simulation []
  (let [lab-ranges   (lab-ranges world-size lab-size)
        {:keys [world agents]} (genesis world-size lab-ranges nhumans)
        evaporator   (agent nil)
        ui-throttler (agent nil)
        alive-cnt    (ref nhumans)
        behave!      (behaviour world
                       {:world-size           world-size
                        :death-ratio          death-ratio
                        :direct-exp-prob      direct-exp-prob
                        :indirect-exp-prob    indirect-exp-prob
                        :human-exp-prob       human-exp-prob
                        :air-spreading-radius air-spreading-radius
                        :air-exp-prob         air-exp-prob
                        :running              running
                        :agent-sleep-ms       agent-sleep-ms
                        :alive-cnt            alive-cnt})

        evaporate!   (evaporation world
                       {:world-size     world-size
                        :viral-load-dec viral-load-dec
                        :running        running
                        :evap-sleep-ms  evap-sleep-ms})

        ;; Data State holds the business logic of our app

        width        (* 15 world-size)
        height       (* 15 world-size)
        scale        (double (/ (min width height) world-size))
        state-proxy  (atom {:width  width
                            :height height
                            :scale  scale
                            :cells  (into []
                                      (for [x (range world-size) y (range world-size)]
                                        (let [{:keys [virus occupant]} (get-in world [x y])]
                                          {:x        (* x scale)
                                           :y        (* y scale)
                                           :occupant @occupant
                                           :virus    @virus})))})
        data-state   (atom @state-proxy)
        ]
    ;; funnel cell states to the proxy state
    (doall
      (for [x (range world-size) y (range world-size)]
        (let [{:keys [virus occupant]} (get-in world [x y])]
          (add-watch occupant :occupant
            (fn [_ _ _ n]
              (swap! state-proxy assoc-in
                [:cells (+ (* x world-size) y) :occupant]
                n)))
          (add-watch virus :virus
            (fn [_ _ _ n]
              (swap! state-proxy assoc-in
                [:cells (+ (* x world-size) y) :virus]
                n))))))

    ;; Every time the data-state changes, queue up an update of the UI
    (add-watch state-proxy :ui (fn [_ _ _ state]
                                 (when (= 0 ui-throttle-cnt)
                                   (reset! data-state state)
                                   (alter-var-root #'ui-throttle-cnt (constantly 1)))))
    (ui/show data-state)
    (dorun (map #(send-off % behave!) agents))
    (send-off evaporator evaporate!)
    (send-off ui-throttler ui-throttle))
  )


(defn start []
  (with-open [w (clojure.java.io/writer logfile)]
    (.write w "Start\n"))
  (reset! running true)
  (Thread/sleep 5)
  (simulation)
  nil)

(defn stop []
  (reset! running false)
  (safe-println "Stop\n"))

(comment
  (start)
  (stop)
  )
