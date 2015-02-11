(ns cljstemplate.shape
  (:use [cljstemplate.logging :only [logger log-when-changes]]
        [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        [cljstemplate.shapeconstance :only [square-pad square-radius square-inner-radius
                                            hex-pad hex-radius hex-inner-radius
                                            tri-pad tri-radius tri-inner-radius
                                            oct-pad oct-radius oct-inner-radius]])
  (:require [cljs.core.async :refer [put! close!]]))



(def log (logger :shape))

(def debug true)


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
  (let [shape (nth shapes shape-id)]
    (if (not-rotating? shape)
      (let [neighbour-index (index-of (:neighbours shape) from-shape-id)
            channel-wires (nth (:wiring shape) channel-id)
            n (:n shape)
            r (:position (:rotation shape))
            rotated-neighbour-index (mod (+ neighbour-index n (- r)) n)]
        (mapv #(concat [shape-id channel-id] %) (find-wires rotated-neighbour-index channel-wires)))
      [])))

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


(defn clicked [shape [_ _ _ timestamp]]
  ;(log (str "in clicked " (:rotation shape)))
  (if (not-rotating? shape)
    (let [start (:position (:rotation shape))
          end start
          start-time timestamp
          end-time timestamp]
      (merge shape {:rotation {:start start :current start :end end :start-time start-time :end-time end-time}}))
    (let [start (or (:position (:rotation shape)) (:current (:rotation shape)) (:start (:rotation shape)))
          end (dec (:end (:rotation shape)))
          start-time timestamp
          end-time (+ (:end-time (:rotation shape)) 250)]
      (merge shape {:rotation {:start start :current start :end end :start-time start-time :end-time end-time}}))))



(defn rgb-str [[r g b]]
  (str "rgb(" r "," g "," b ")"))
(defn rgba-str [[r g b a]]
  (str "rgba(" r "," g "," b "," a ")"))



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
  (if (if-let [[x y clicked] click]
        (and clicked (.isPointInPath context x y)))
    (clicked shape click)
    shape))

(defn vertices [{n :n [x y r] :location rotation :rotation} sf]
  (let [alpha (alphas n)
        delta (/ alpha 2)
        xs (* x sf)
        ys (* y sf)
        radius (* (radii n) sf)
        beta (+ r delta (* (or (:current rotation) (:position rotation)) alpha))
        gammas (take n (iterate #(+ % alpha) beta))]
    (for [gamma gammas]
      [(+ xs (* radius (Math.sin gamma))) (+ ys (* radius (Math.cos gamma)))])))


(defn trace-path [context [[x1 y1] & rest]]
  (. context (beginPath))
  (. context (moveTo x1 y1))
  (doseq [[xr yr] rest]
    (. context (lineTo xr yr)))
  (. context (lineTo x1 y1)))


(defn render-shape [context sf [mx my :as mouse] channels [_ bdr fg] {[x y r] :location n :n rotation :rotation wiring :wiring :as shape} id]
  (set! (. context -lineWidth) 1)
  (set! (. context -lineJoin) "round")
  (let [alpha (alphas n)
        delta (/ alpha 2)
        radius (* (radii n) sf)
        inner-radius (* (inner-radii n) sf)
        beta (+ r delta (* (or (:current rotation) (:position rotation)) alpha))
        gammas (iterate #(+ % alpha) beta)
        epsilons (iterate #(+ % alpha) (- beta delta))
        channel-width (* 5 sf)
        xs (* x sf)
        ys (* y sf)]
    (. context (beginPath))
    (. context (moveTo (+ xs (* radius (Math.sin beta))) (+ ys (* radius (Math.cos beta)))))
    (doseq [gamma (take (dec n) (drop 1 gammas))]
      (. context (lineTo (+ xs (* radius (Math.sin gamma))) (+ ys (* radius (Math.cos gamma))))))
    (. context (closePath))


    (set! (. context -strokeStyle) (rgb-str bdr))
    (set! (. context -fillStyle) (rgba-str (conj fg (if (.isPointInPath context mx my) 0.6 1))))

    (. context (fill))
    (. context (stroke))

    (let [result (click-result shape context mouse)]

      (doseq [ch (range (count channels))]
        (let [channel (nth channels ch)
              channel-wiring (nth wiring ch)
              ch-pos (* (- ch (/ (dec (count channels)) 2)) channel-width)]
          (doseq [[from onto switched] channel-wiring]
            (. context (beginPath))
            (let [[from-x from-y] [(Math.sin (nth epsilons from)) (Math.cos (nth epsilons from))]
                  [onto-x onto-y] [(Math.sin (nth epsilons onto)) (Math.cos (nth epsilons onto))]
                  [from-x-p from-y-p] [(Math.cos (nth epsilons from)) (- (Math.sin (nth epsilons from)))]
                  [onto-x-p onto-y-p] [(Math.cos (nth epsilons onto)) (- (Math.sin (nth epsilons onto)))]]
              (. context (moveTo (+ xs (* inner-radius from-x)
                                    (* ch-pos from-x-p)
                                    )
                                 (+ ys (* inner-radius from-y)
                                    (* ch-pos from-y-p)
                                    )))
              (. context (lineTo (+ xs (* 0.7 inner-radius from-x) (* ch-pos from-x-p))
                                 (+ ys (* 0.7 inner-radius from-y) (* ch-pos from-y-p))))
              (. context (lineTo (+ xs (* 0.7 inner-radius onto-x) (* ch-pos onto-x-p))
                                 (+ ys (* 0.7 inner-radius onto-y) (* ch-pos onto-y-p))))
              (. context (lineTo (+ xs (* inner-radius onto-x)
                                    (* ch-pos onto-x-p)
                                    )
                                 (+ ys (* inner-radius onto-y)
                                    (* ch-pos onto-y-p)
                                    ))))
            (if (some #{:on} switched)
              (do
                (set! (. context -strokeStyle) "rgb(255,255,255)")
                (set! (. context -lineWidth) (inc channel-width))
                (. context (stroke))
                (set! (. context -strokeStyle) (rgb-str channel))
                (set! (. context -lineWidth) channel-width)
                (. context (stroke))
                (set! (. context -strokeStyle) "rgba(255,255,255, 0.25)")
                (set! (. context -lineWidth) (/ channel-width 2))
                (. context (stroke))
                (set! (. context -strokeStyle) "rgba(255,255,255, 0.15)")
                (set! (. context -lineWidth) (/ channel-width 6))
                (. context (stroke))
                )
              (do
                (set! (. context -strokeStyle) "rgb(0,0,0)")
                (set! (. context -lineWidth) (inc (/ channel-width 2)))
                (. context (stroke))
                (set! (. context -strokeStyle) (rgba-str (conj channel 0.75)))
                (set! (. context -lineWidth) (/ channel-width 2))
                (. context (stroke))))
            )
          ))
      (if debug (do
                  (set! (. context -fillStyle) (rgb-str [250 250 250]))
                  (. context (fillRect (- xs 5) (- ys 10) 15 15))
                  (set! (. context -fillStyle) (rgb-str [0 0 0]))
                  (. context (fillText (str id) xs ys))
                  ))
      result)))


(defn scale-factor [w h max-w max-h]
  (log (str {:w w :h h :mw max-w :mh max-h}))
  (log (min (/ max-w w) (/ max-h h))))

(defn render-at-rest [context sf mouse channels colours shape id]
  (if (not-rotating? shape)
    (render-shape context sf mouse channels colours shape id)
    shape))

(defn render-in-motion [context sf mouse channels colours shape id]
  ;(log-when-changes :motion (str "Render in motion: " shape))
  (if (rotating? shape)
    (render-shape context sf mouse channels colours shape id)
    shape))


(defn fill-circle [surface coords colour]
  (let [[x y d] coords]
    (set! (. surface -fillStyle) (rgba-str colour))
    (. surface (beginPath))
    (.arc surface x y d 0 (* 2 Math/PI) true)
    (. surface (closePath))
    (. surface (fill))))

(defn stroke-circle [surface coords colour]
  (let [[x y d] coords]
    (set! (. surface -strokeStyle) (rgba-str colour))
    (. surface (beginPath))
    (.arc surface x y d 0 (* 2 Math/PI) true)
    (. surface (closePath))
    (. surface (stroke))))


(defn render-start [{shapes :shapes [start _ _] :start channels :channels :as level} context timestamp [_ bdr fg] sf]
  (let [shape (nth shapes start)
        vtxs (vertices shape sf)
        {[x y _] :location n :n} shape
        xs (* x sf)
        ys (* y sf)
        radius (* (radii n) sf)
        many-channels (apply concat (repeat (- 4 (count channels)) channels))
        ;(if (= (count channels) 1)
        ;                (concat channels channels channels)
        ;                (concat channels channels))
        channel-count (count many-channels)]

    (. context (save))

    (trace-path context vtxs)
    (. context (clip))

    (fill-circle context [xs ys radius] [0 0 0 1])

    (doseq [i (range channel-count)]
      (let [f (mod (+ (/ timestamp 100) (* i (/ radius channel-count))) radius)]
        (fill-circle context [xs ys f] (conj (nth many-channels i) (- 1 (/ f radius))))))

    (trace-path context vtxs)
    (set! (. context -strokeStyle) (rgb-str bdr))
    (set! (. context -lineWidth) 1)
    (. context (stroke))
    (. context (restore)))

  level)

(defn render-end [{shapes :shapes [end _ _] :end channels :channels :as level} context timestamp [_ bdr fg] sf done]
  (let [shape (nth shapes end)
        vtxs (vertices shape sf)
        {[x y _] :location n :n wiring :wiring} shape
        xs (* x sf)
        ys (* y sf)
        radius (* (radii n) sf)
        radius_3rd (/ radius 3)
        radius_5th (/ radius 5)
        radius_15th (/ radius 15)
        radius_20th (/ radius 30)
        channel-count (count channels)]

    (set! (. context -lineWidth) 5)
    (. context (save))

    (trace-path context vtxs)
    (. context (clip))

    (fill-circle context [xs ys radius] [0 0 0 1])

    (if (every? identity (for [channel-wiring wiring]
                           (some #{:on} (flatten channel-wiring))))

      (do (doseq [i (range 1 5)]
            (fill-circle context [xs ys (/ radius (- 5 i))] (conj fg (/ 1 i))))
          ;(log (str "DONE FROM SHAPE" wiring))
          (reset! done true)
          (render-attention context xs ys radius timestamp))
      )


    ;(log (str (vec (for [channel-wiring wiring]
    ;                 (some #{:on} (flatten channel-wiring))))))

    ;(doseq [channel-wiring wiring]
    ;  (log (str (flatten channel-wiring))))

    (doseq [i (range channel-count)]
      (let [angle (- (* i (/ TAU channel-count)) (/ timestamp 3000))
            xi (+ xs (* radius_3rd (Math/sin angle)))
            yi (+ ys (* radius_3rd (Math/cos angle)))
            on (some #{:on} (flatten (nth wiring i)))
            ]
        ;(log angle)
        (fill-circle context [xi yi radius_5th] (conj (nth channels i) (if on 1 0.25)))
        (stroke-circle context [xi yi radius_5th] (conj (nth channels i) (if on 0.75 0.25)))
        (if on
          (do
            (fill-circle context [(- xi radius_15th) (- yi radius_15th) radius_15th] [255 255 255 0.15])
            (fill-circle context [(- xi radius_15th) (- yi radius_15th) radius_20th] [255 255 255 0.15])
            ))
        ))

    (trace-path context vtxs)
    (set! (. context -strokeStyle) (rgb-str bdr))
    (set! (. context -lineWidth) 1)
    (. context (stroke))

    (. context (restore)))

  level)



(defn attention-gradient [context x y radius]

  (let [grd (.createRadialGradient context x y 1 x y radius)]
    (.addColorStop grd 0 "rgba(250, 250, 250, 1.0")
    (.addColorStop grd 1 "rgba(250, 250, 250, 0.0")
    grd)

  ;var grd=ctx.createRadialGradient(75,50,5,90,60,100);
  ;grd.addColorStop(0,"red");
  ;grd.addColorStop(1,"white");
  ;
  ;// Fill with gradient
  ;ctx.fillStyle=grd;
  ;ctx.fillRect(10,10,150,100);
  ;
  )


(defn render-attention [context x y radius timestamp]
  (let [gradient (attention-gradient context x y radius)]
    (set! (. context -fillStyle) gradient)
    (doseq [i (range 16)]
      (let [start (- (* i (/ PI 8)) (/ timestamp 3000))
            end (- start 0.2)]
        (. context (beginPath))
        (. context (moveTo x y))
        (.arc context x y radius start end true)
        (. context (closePath))
        (. context fill)))))

(defn render [[context width height] level mouse timestamp done]
  (let [sf (scale-factor (:width level) (:height level) width height)
        channels (:channels level)
        colours (:colours level)]
    (if @done
      (render-attention context width height (/ (min width height) 2) timestamp))
    (-> level
        (update :shapes #(doall (map (partial render-at-rest context sf mouse channels colours) % (range))))
        (update :shapes #(doall (map (partial render-in-motion context sf mouse channels colours) % (range))))
        (render-start context timestamp colours sf)
        (render-end context timestamp colours sf done)
        )
    ;; TODO: transitioning shapes are rendered twice!
    ))

            ;#(map (partial render-in-motion context sf click)
            ;      (map (partial render-at-rest context sf click) %)))))


;;  render
;;;  creates a render function (returns it or adds it to a map/list by z-order [cons/conj? - only two z-orders!]