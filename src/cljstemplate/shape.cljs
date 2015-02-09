(ns cljstemplate.shape
  (:use [cljstemplate.logging :only [logger log-when-changes]]
        [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        [cljstemplate.shapeconstance :only [square-pad square-radius square-inner-radius
                                            hex-pad hex-radius hex-inner-radius
                                            tri-pad tri-radius tri-inner-radius
                                            oct-pad oct-radius oct-inner-radius]]))



(def log (logger :shape))




(defn index-of [s v]
  (loop [idx 0 items s]
    (cond
      (empty? items) nil
      (= v (first items)) idx
      :else (recur (inc idx) (rest items)))))


;;

(def level-1 "Pre-created level. EDN?"
  {:shapes   [{:n          6
               :location   [150 150 0]
               :neighbours [nil nil nil 1 nil nil]
               :wiring     [[[1 2 [:on :off]] [1 4]]
                            [[5 3]]
                            [[1 3]]]
               :rotation   {:position 2}}
              {:n          6
               :location   [250 150 0]
               :neighbours [0 nil nil nil nil nil]
               :wiring     [[[1 2] [1 4]]
                            [[5 0]]
                            [[1 3]]]
               :rotation  {:position 0}
               ;{:start 2 :current 2.5 :end 3 :start-time 123456789 :end-time 123457789}
               }
              ]
   :width 500
   :height 500
   :channels [[250 0 0] [0 250 0] [0 0 250]]
   :start    [0 0 0]
   :end      [1 1 1]})


;; Shape

(defn not-rotating? [shape]
  (get-in shape [:rotation :position]))

(def rotating? (complement not-rotating?))



;; check connections
;;  recursive function from start points / 'sweep and mark'

;; connections

;;; clear

(defn clear-wire-connections [[in out _ _]]
  [in out [:off :off]])

(defn clear-channel-connections [channel-wires]
  (mapv clear-wire-connections channel-wires))

(defn clear-shape-connections [shape]
  (update shape :wiring #(mapv clear-channel-connections %)))

(defn clear-connections [level]
  (update level :shapes #(mapv clear-shape-connections %)))

;;; populate

(defn switch-on [shape channel-id wire-id direction-id]
  (assoc-in shape [:wiring channel-id wire-id 2 direction-id] :on))

(defn wire-index-select [target [from onto [backward forward]] index]
  (cond
    (and (= from target) (= forward :off)) [index 1]
    (and (= onto target) (= backward :off)) [index 0]
    :else nil))

(defn find-wires [target wires]
  (filter identity
          (mapv (partial wire-index-select target) wires (range))))

(defn seeds-from [shapes shape-id from-shape-id channel-id]
  ;(log shape-id)
  (let [shape (nth shapes shape-id)
        neighbour-index (index-of (:neighbours shape) from-shape-id)
        channel-wires (nth (:wiring shape) channel-id)
        n (:n shape)
        r (:position (:rotation shape))
        rotated-neighbour-index (mod (+ neighbour-index r) n)]
    (mapv #(concat [shape-id channel-id] %) (find-wires rotated-neighbour-index channel-wires))))

(defn more-seeds [shapes shape-id channel-id wire-id direction-id]
  (let [shape (nth shapes shape-id)]
    (if (not-rotating? shape)
      (let [neighbours (:neighbours shape)
            output (get-in shape [:wiring channel-id wire-id direction-id])
            position (get-in shape [:rotation :position])
            sides (:n shape)
            neighbour-id (nth neighbours (mod (+ output position) sides))]
        (if neighbour-id
          (seeds-from shapes neighbour-id shape-id channel-id))))))

(defn populate-shape-connections [shapes [[shape-id channel-id wire-id direction-id] & seeds]]
  ;; shapes [ [shape-id channel-id wire-id direction-id] ... ]
    (let [new-shapes (update shapes shape-id #(switch-on % channel-id wire-id direction-id))
          new-seeds (concat seeds (more-seeds shapes shape-id channel-id wire-id direction-id))]
      (if (seq new-seeds)
        (recur new-shapes new-seeds)
        new-shapes)))

(defn seed-lights [level shape-id channel-id]
  (let [shapes (:shapes level)
        shape (nth shapes shape-id)
        wiring (:wiring shape)
        channel-wiring (nth wiring channel-id)]
    (concat
      (mapv #(vec [shape-id channel-id % 0]) (range (count channel-wiring)))
      (mapv #(vec [shape-id channel-id % 1]) (range (count channel-wiring))))))

(defn seed-light [level]
  (mapcat seed-lights (repeat level) (:start level) (range)))

(defn populate-connections [level]
  (update level :shapes #(populate-shape-connections % (seed-light level))))

(defn check-connections [level]
  (populate-connections (clear-connections level)))




;(log (str (take 20 (seed-light level-1))))

;(log (str level-1))
;(log (str (check-connections level-1)))




(defn proportionalise "returns e" [[a b c] [d f]]
  (let [dist1 (- c a)
        prog1 (- b a)
        prop (/ prog1 dist1)
        dist2 (- f d)
        prog2 (* prop dist2)
        e (+ d prog2)]

    e))


(defn rotated [timestamp {{start :start end :end start-time :start-time end-time :end-time} :rotation :as shape}]
  (cond
    (get-in shape [:rotation :position]) shape
    (< end-time timestamp) (assoc-in shape [:rotation] {:position end})
    (< timestamp start-time) (assoc-in shape [:rotation :current] start) ; should never happen?
    :else (assoc-in shape [:rotation :current] (proportionalise [start-time timestamp end-time] [start end]))))

(defn do-rotations [timestamp level]
  (let [r-fn (partial rotated timestamp)]
    (update level :shapes #(map r-fn %))))

;; TODO: separate rotations, when we have to set current in clicked anyway?!?
;;  TODO: its all pointing to a separate pass for click-hit detection!


;; map render shapes
;;  detect clicks and alter state if clicked

;; rendering


(defn clicked [shape [_ _ timestamp]]
  ;(log (str "in clicked " (:rotation shape)))
  (if (not-rotating? shape)
    (let [start (:position (:rotation shape))
          end start
          start-time timestamp
          end-time timestamp]
      (merge shape {:rotation {:start start :current start :end end :start-time start-time :end-time end-time}}))
    (let [start (or (:position (:rotation shape)) (:current (:rotation shape)) (:start (:rotation shape)))
          end (inc (:end (:rotation shape)))
          start-time timestamp
          end-time (+ (:end-time (:rotation shape)) 250)]
      (merge shape {:rotation {:start start :current start :end end :start-time start-time :end-time end-time}}))))


;; TODO: these are in the wrong place!
(def shape-fill "rgba(0, 150, 225, 1.0)")
(def shape-stroke "rgb(0, 0, 250)")

(defn rgb-str [[r g b]]
  (str "rgb(" r "," g "," b ")"))
(defn rgba-str [[r g b a]]
  (str "rgba(" r "," g "," b "," a ")"))


(defn render-resting-shape "TODO!!!!!!" [context sf click {[x y r] :location n :n {position :position} :rotation :as shape}]
  (set! (. context -strokeStyle) (str shape-stroke))
  (set! (. context -fillStyle) (str shape-fill))
  (let [theta1 (+ r (* position TAU_6TH))
        theta2 (+ theta1 TAU_6TH)
        theta3 (+ theta2 TAU_6TH)
        theta4 (+ theta3 TAU_6TH)
        theta5 (+ theta4 TAU_6TH)
        theta6 (+ theta5 TAU_6TH)
        radius hex-radius]
    (. context (beginPath))
    (. context (moveTo (+ x (* radius (Math.sin theta1))) (+ y (* radius (Math.cos theta1)))))
    (. context (lineTo (+ x (* radius (Math.sin theta2))) (+ y (* radius (Math.cos theta2)))))
    (. context (lineTo (+ x (* radius (Math.sin theta3))) (+ y (* radius (Math.cos theta3)))))
    (. context (lineTo (+ x (* radius (Math.sin theta4))) (+ y (* radius (Math.cos theta4)))))
    (. context (lineTo (+ x (* radius (Math.sin theta5))) (+ y (* radius (Math.cos theta5)))))
    (. context (lineTo (+ x (* radius (Math.sin theta6))) (+ y (* radius (Math.cos theta6)))))
    (. context (closePath))
    ;(if (.isPointInPath context (:x @pointer-state) (:y @pointer-state))
    ;  (.fill context))
    (. context (fill))
    (. context (stroke))
    )
  ;(log "render-resting-shape")
  (if (if-let [[x y] click]
        (.isPointInPath context x y))
    (clicked shape click)
    shape))


(def alphas {3 TAU_3RD
             4 TAU_4TH
             6 TAU_6TH
             8 TAU_8TH})
(def radii {3 tri-radius
            4 square-radius
            6 hex-radius
            8 oct-radius})
(def inner-radii {3 tri-inner-radius
                  4 square-inner-radius
                  6 hex-inner-radius
                  8 oct-inner-radius})

(defn click-result [shape context click]
  (if (if-let [[x y] click]
        (.isPointInPath context x y))
    (clicked shape click)
    shape))

(defn render-shape [context sf click channels [_ bdr fg] {[x y r] :location n :n rotation :rotation wiring :wiring :as shape}]
  (set! (. context -lineWidth) 1)
  (set! (. context -lineCap) "round")
  (let [alpha (alphas n)
        delta (/ alpha 2)
        radius (radii n)
        inner-radius (inner-radii n)
        beta (+ r delta (* (or (:current rotation) (:position rotation)) alpha))
        gammas (iterate #(+ % alpha) beta)
        epsilons (iterate #(+ % alpha) (- beta delta))]
    (. context (beginPath))
    (. context (moveTo (+ x (* radius (Math.sin beta))) (+ y (* radius (Math.cos beta)))))
    (doseq [gamma (take (dec n) (drop 1 gammas))]
      (. context (lineTo (+ x (* radius (Math.sin gamma))) (+ y (* radius (Math.cos gamma))))))
    (. context (closePath))

    (set! (. context -strokeStyle) (rgb-str bdr))
    (set! (. context -fillStyle) (rgb-str fg))

    (. context (fill))
    (. context (stroke))
    (let [result (click-result shape context click)]

      (doseq [ch (range (count channels))]
        (let [channel (nth channels ch)
              channel-wiring (nth wiring ch)
              ch-pos (* (- ch (/ (dec (count channels)) 2)) 10)]   ;TODO '10' is magic

          ;(log (str "'" channel "' : " channel-wiring))


          (doseq [[from onto switched] channel-wiring]

            ;(log (. context -strokeStyle))
            ;(set! (. context -strokeStyle) (if (some #{:on} switched) "red" channel))
            (. context (beginPath))

            (let [[from-x from-y] [(Math.sin (nth epsilons from)) (Math.cos (nth epsilons from))]
                  [onto-x onto-y] [(Math.sin (nth epsilons onto)) (Math.cos (nth epsilons onto))]
                  [from-x-p from-y-p] [(Math.cos (nth epsilons from)) (- (Math.sin (nth epsilons from)))]
                  [onto-x-p onto-y-p] [(Math.cos (nth epsilons onto)) (- (Math.sin (nth epsilons onto)))]
                  ]

              (. context (moveTo (+ x (* inner-radius from-x) (* ch-pos from-x-p))
                                 (+ y (* inner-radius from-y) (* ch-pos from-y-p))))
              (. context (lineTo (+ x (* 0.7 inner-radius from-x) (* ch-pos from-x-p))
                                 (+ y (* 0.7 inner-radius from-y) (* ch-pos from-y-p))))
              (. context (lineTo (+ x (* 0.7 inner-radius onto-x) (* ch-pos onto-x-p))
                                 (+ y (* 0.7 inner-radius onto-y) (* ch-pos onto-y-p))))
              (. context (lineTo (+ x (* inner-radius onto-x) (* ch-pos onto-x-p))
                                 (+ y (* inner-radius onto-y) (* ch-pos onto-y-p))))
              )
            ;(. context (closePath))

            (if (some #{:on} switched)
              (do
                (set! (. context -strokeStyle) "rgb(0,0,0)")
                (set! (. context -lineWidth) 6)
                (. context (stroke))
                (set! (. context -strokeStyle) (rgb-str channel))
                (set! (. context -lineWidth) 5)
                (. context (stroke)))
              (do
                (set! (. context -strokeStyle) "rgb(0,0,0)")
                (set! (. context -lineWidth) 4)
                (. context (stroke))
                (set! (. context -strokeStyle) (rgba-str (conj channel 0.8)))
                (set! (. context -lineWidth) 3)
                (. context (stroke)))
              )

            ;(set! (. context -strokeStyle) "black")
            ;(set! (. context -strokeStyle) "rgb(0,0,0)")
            ;(set! (. context -lineWidth) 6)
            ;(. context (stroke))
            ;
            ;(set! (. context -strokeStyle) (if (some #{:on} switched)
            ;                                 (rgb-str channel)
            ;                                 (rgba-str (conj channel 0.75))))
            ;(set! (. context -lineWidth) (if (some #{:on} switched)
            ;                               5
            ;                               3))
            ;(. context (stroke))

            )

          ))

      result)))


(defn scale-factor [w h max-w max-h]
  (min (/ max-w w) (/ max-h h)))

(defn render-at-rest [context sf click channels colours shape]
  (if (not-rotating? shape)
    (render-shape context sf click channels colours shape)
    shape))

(defn render-in-motion [context sf click channels colours shape]
  ;(log-when-changes :motion (str "Render in motion: " shape))
  (if (rotating? shape)
    (render-shape context sf click channels colours shape)
    shape))


(defn fill-circle [surface coords [r g b a]]
  (let [[x y d] coords]
    (set! (. surface -fillStyle) (str "rgba(" r "," g "," b "," a ")"))
    (. surface (beginPath))
    (.arc surface x y d 0 (* 2 Math/PI) true)
    (. surface (closePath))
    (. surface (fill))))


(defn render-start [{shapes :shapes [start _ _] :start [ch1 ch2 ch3] :channels :as level} context timestamp]
  (let [{[x y _] :location} (nth shapes start)
        f1 (+ 0.5 (/ (Math/sin (+ (/ timestamp 1000) 0)) 2))
        f2 (+ 0.5 (/ (Math/sin (+ (/ timestamp 1000) 1.5)) 2))
        f3 (+ 0.5 (/ (Math/sin (+ (/ timestamp 1000) 3)) 2))
        ]
    (fill-circle context [x y 21] [0 0 0 1])
    (fill-circle context [x y 20] (conj ch1 f1))
    (fill-circle context [x y 15] (conj ch2 f2))
    (fill-circle context [x y 10] (conj ch3 f3)))
  level)

(defn render-end [{shapes :shapes [end _ _] :end [ch1 ch2 ch3] :channels :as level} context timestamp]
  (let [{[x y _] :location} (nth shapes end)
        f1 (+ 0.5 (/ (Math/sin (- (/ timestamp 1000) 0)) 2))
        f2 (+ 0.5 (/ (Math/sin (- (/ timestamp 1000) 1.5)) 2))
        f3 (+ 0.5 (/ (Math/sin (- (/ timestamp 1000) 3)) 2))
        ]
    (fill-circle context [x y 21] [0 0 0 1])
    (fill-circle context [x y 20] (conj ch1 f1))
    (fill-circle context [x y 15] (conj ch2 f2))
    (fill-circle context [x y 10] (conj ch3 f3)))
  level)

(defn render [[context width height] level click timestamp]
  (let [sf (scale-factor (:width level) (:height level) width height)
        channels (:channels level)
        colours (:colours level)]
    (-> level
        (update :shapes #(doall (map (partial render-at-rest context sf click channels colours) %)))
        (update :shapes #(doall (map (partial render-in-motion context sf click channels colours) %)))
        (render-start context timestamp)
        (render-end context timestamp))
    ;; TODO: transitioning shapes are rendered twice!
    ))

            ;#(map (partial render-in-motion context sf click)
            ;      (map (partial render-at-rest context sf click) %)))))


;;  render
;;;  creates a render function (returns it or adds it to a map/list by z-order [cons/conj? - only two z-orders!]