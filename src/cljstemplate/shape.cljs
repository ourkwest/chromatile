(ns cljstemplate.shape
  (:use [cljstemplate.logging :only [logger log-when-changes]]
        [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        [cljstemplate.shapeconstance :only [shape-side-length
                                            square-pad square-radius square-inner-radius
                                            hex-pad hex-radius hex-inner-radius
                                            tri-pad tri-radius tri-inner-radius
                                            oct-pad oct-radius oct-inner-radius]])
  (:require [cljs.core.async :refer [put! close!]]))



(def log (logger :shape))

(def debug false)                                           ;show numbers on shapes


(defn index-of [s v]
  (loop [idx 0 items s]
    (cond
      (empty? items) nil
      (= v (first items)) idx
      :else (recur (inc idx) (rest items)))))


;; Shape

(defn not-rotating? [shape]
  (get-in shape [:rotation :position]))

(def rotating? (complement not-rotating?))


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
        (if (and neighbour-id (not= output 9))
          (seeds-from shapes neighbour-id shape-id channel-id))))))

(defn populate-shape-connections [shapes [[shape-id channel-id wire-id direction-id] & seeds]]
    (let [new-shapes (update shapes shape-id #(switch-on % channel-id wire-id direction-id))
          new-seeds (concat seeds (more-seeds new-shapes shape-id channel-id wire-id direction-id))]
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


;; rotation


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
(def pads {3 tri-pad
           4 square-pad
           6 hex-pad
           8 oct-pad})

(defn click-result [shape context click]
  (if (if-let [[x y clicked] click]
        (and clicked (.isPointInPath context x y)))
    (clicked shape click)
    shape))

(defn vertices [{n :n [x y r] :location rotation :rotation} xs ys sf]
  (let [alpha (alphas n)
        delta (/ alpha 2)
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


(def path-lengths
  {[4 1] 1.6
   [4 3] 1.6
   [6 1] 1.3
   [6 5] 1.3
   [8 1] 1.0
   [8 2] 1.5
   [8 3] 1.8
   [8 5] 1.8
   [8 6] 1.5
   [8 7] 1.0
   })


(defn render-shape [context sf [x-offset y-offset] [mx my :as mouse] channels [_ bdr fg] {[x y r] :location n :n rotation :rotation wiring :wiring :as shape} id timestamp]


  (let [alpha (alphas n)
        delta (/ alpha 2)
        radius (* (radii n) sf)
        inner-radius (* (inner-radii n) sf)
        pad (* (pads n) sf)
        beta (+ r delta (* (or (:current rotation) (:position rotation)) alpha))
        gammas (iterate #(+ % alpha) beta)
        epsilons (iterate #(+ % alpha) (- beta delta))
        side-length (* shape-side-length sf)
        channel-width (* side-length 0.07)
        xs (+ (* x sf) x-offset)
        ys (+ (* y sf) y-offset)]

    (set! (. context -lineWidth) 1)

    (. context (beginPath))
    (. context (moveTo (+ xs (* radius (Math.sin beta))) (+ ys (* radius (Math.cos beta)))))
    (doseq [gamma (take (dec n) (drop 1 gammas))]
      (. context (lineTo (+ xs (* radius (Math.sin gamma))) (+ ys (* radius (Math.cos gamma))))))
    (. context (closePath))

    (set! (. context -fillStyle) (rgba-str (conj fg (if (.isPointInPath context mx my) 0.6 1))))
    (. context (fill))

    (. context (save))
    (. context (clip))

    (let [result (click-result shape context mouse)]

      (doseq [ch (range (count channels))]
        (let [channel (nth channels ch)
              channel-wiring (nth wiring ch)
              ch-pos (* (- ch (/ (dec (count channels)) 2)) (/ inner-radius n) 2.5)]
          (doseq [[from onto [fw bw :as switched]] channel-wiring]
            (. context (beginPath))
            (let [[from-x from-y] [(Math.sin (nth epsilons from)) (Math.cos (nth epsilons from))]
                  [onto-x onto-y] [(Math.sin (nth epsilons onto)) (Math.cos (nth epsilons onto))]
                  [from-x-p from-y-p] [(Math.cos (nth epsilons from)) (- (Math.sin (nth epsilons from)))]
                  [onto-x-p onto-y-p] [(Math.cos (nth epsilons onto)) (- (Math.sin (nth epsilons onto)))]
                  [xa ya] [(+ xs (* pad from-x))
                           (+ ys (* pad from-y))]
                  [xb yb] [(+ xs (* 0.5 inner-radius from-x) (* ch-pos from-x-p))
                           (+ ys (* 0.5 inner-radius from-y) (* ch-pos from-y-p))]
                  [xc yc] [(+ xs (* 0.5 inner-radius onto-x) (* ch-pos onto-x-p))
                           (+ ys (* 0.5 inner-radius onto-y) (* ch-pos onto-y-p))]
                  [xd yd] [(+ xs (* pad onto-x))
                           (+ ys (* pad onto-y))]]
              (. context (moveTo xa ya))
              (. context (bezierCurveTo xb yb xc yc xd yd))

              (let [length-est (* inner-radius (get path-lengths [n (Math/abs (- from onto))] 2))
                    length-bit (/ length-est (int (/ length-est (* sf 10))))
                    bit-1 (/ channel-width 3)                                 ; (* length-bit (/ 1 3))
                    bit-2 (* length-bit (/ 2 3))
                    bits (+ bit-1 bit-2)
                    offset (mod (* (/ timestamp 1000) bits) bits)
                    lineDash #js [bit-1 bit-2]]

                ; Render background
                (if (some #{:on} switched)
                  (do
                    (set! (. context -strokeStyle) "rgb(255,255,255)")
                    (set! (. context -lineWidth) (inc channel-width))
                    (. context (stroke))
                    (set! (. context -strokeStyle) (rgb-str channel))
                    (set! (. context -lineWidth) channel-width)
                    (. context (stroke))

                    ;(set! (. context -strokeStyle) "rgba(255,255,255, 0.25)")
                    ;(set! (. context -lineWidth) (/ channel-width 2))
                    ;(. context (stroke))

                    )
                  (do
                    (set! (. context -strokeStyle) "rgb(0,0,0)")
                    (set! (. context -lineWidth) (inc (/ channel-width 3)))
                    (. context (stroke))
                    (set! (. context -strokeStyle) (rgb-str channel))
                    ;(set! (. context -strokeStyle) (rgb-str channel))
                    (set! (. context -lineWidth) (/ channel-width 3))
                    (. context (stroke))))

                ; Render blobs
                (doseq [[direction os] [[fw offset] [bw (- offset)]]]
                  (if (= :on direction)
                    (do

                      (. context (setLineDash lineDash))

                      (set! (. context -lineDashOffset) os)
                      (set! (. context -strokeStyle) "black")      ; "rgba(255,255,255, 0.75)"
                      (set! (. context -lineWidth) (/ channel-width 3))
                      (. context (stroke))


                      ;(set! (. context -lineDashOffset) (+ os bit-1))
                      ;(set! (. context -strokeStyle) "rgba(0,0,0, 0.25)")
                      ;(set! (. context -lineWidth) (/ channel-width 2))
                      ;(. context (stroke))

                      ;(log (str "ldoA: " (. context -lineDashOffset))) ; TODO: wtf?


                      )))


                (. context (setLineDash #js []))

                )

              )
            )
          ))
      (if debug (do
                  (set! (. context -fillStyle) (rgb-str [250 250 250]))
                  (. context (fillRect (- xs 5) (- ys 10) 15 15))
                  (set! (. context -fillStyle) (rgb-str [0 0 0]))
                  (. context (fillText (str id) xs ys))
                  ))
      (. context (restore))

      (. context (beginPath))
      (. context (moveTo (+ xs (* radius (Math.sin beta))) (+ ys (* radius (Math.cos beta)))))
      (doseq [gamma (take (dec n) (drop 1 gammas))]
        (. context (lineTo (+ xs (* radius (Math.sin gamma))) (+ ys (* radius (Math.cos gamma))))))
      (. context (closePath))

      (set! (. context -strokeStyle) (rgb-str bdr))
      (. context (stroke))

      result)))


(defn scale-factor [w h max-w max-h]
  ;(log (str {:w w :h h :mw max-w :mh max-h}))
  ;(log (min (/ max-w w) (/ max-h h)))
  (min (/ max-w w) (/ max-h h)))

(defn render-at-rest [context sf offset mouse channels colours ends timestamp shape id]
  (if (and (not-rotating? shape) (not (ends id)))
    (render-shape context sf offset mouse channels colours shape id timestamp)
    shape))

(defn render-in-motion [context sf offset mouse channels colours ends timestamp shape id]
  ;(log-when-changes :motion (str "Render in motion: " shape))
  (if (and (rotating? shape) (not (ends id)))
    (render-shape context sf offset mouse channels colours shape id timestamp)
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


(defn render-start [{shapes :shapes [start _ _] :start channels :channels :as level} context timestamp [_ bdr _] sf [x-offset y-offset]]
  (let [shape (nth shapes start)
        {[x y _] :location n :n} shape
        xs (+ (* x sf) x-offset)
        ys (+ (* y sf) y-offset)
        vtxs (vertices shape xs ys sf)
        radius (* (radii n) sf)
        many-channels (apply concat (repeat (- 4 (count channels)) channels))
        channel-count (count many-channels)]

    (. context (save))

    (trace-path context vtxs)
    (. context (clip))

    (fill-circle context [xs ys radius] [0 0 0 1])

    (doseq [i (range channel-count)]
      (let [f (mod (+ (* sf timestamp 0.01) (* i (/ radius channel-count))) radius)]
        (fill-circle context [xs ys f] (conj (nth many-channels i) (- 1 (/ f radius))))))

    (. context (restore))

    (trace-path context vtxs)
    (set! (. context -strokeStyle) (rgb-str bdr))
    (set! (. context -lineWidth) 1)
    (. context (stroke)))

  level)

(defn render-end [{shapes :shapes [end _ _] :end channels :channels :as level} context timestamp [_ bdr fg] sf [x-offset y-offset] done]
  (let [shape (nth shapes end)
        {[x y _] :location n :n wiring :wiring} shape
        xs (+ (* x sf) x-offset)
        ys (+ (* y sf) y-offset)
        vtxs (vertices shape xs ys sf)
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
          (reset! done true)
          (render-attention :end context xs ys radius timestamp)))

    (doseq [i (range channel-count)]
      (let [angle (- (* i (/ TAU channel-count)) (/ timestamp 3000))
            xi (+ xs (* radius_3rd (Math/sin angle)))
            yi (+ ys (* radius_3rd (Math/cos angle)))
            on (some #{:on} (flatten (nth wiring i)))]
        (fill-circle context [xi yi radius_5th] (conj (nth channels i) (if on 1 0.25)))
        (stroke-circle context [xi yi radius_5th] (conj (nth channels i) (if on 0.75 0.25)))
        (if on
          (do
            (fill-circle context [(- xi radius_15th) (- yi radius_15th) radius_15th] [255 255 255 0.15])
            (fill-circle context [(- xi radius_15th) (- yi radius_15th) radius_20th] [255 255 255 0.15])
            ))))

    (. context (restore))

    (trace-path context vtxs)
    (set! (. context -strokeStyle) (rgb-str bdr))
    (set! (. context -lineWidth) 1)
    (. context (stroke)))
  level)


(def gradients (atom {})) ; TODO: is this caching justified?

(defn mk-gradient [key context x y radius]
  (let [grd (.createRadialGradient context x y 1 x y radius)]
    (.addColorStop grd 0 "rgba(250, 250, 250, 1.0)")
    (.addColorStop grd 1 "rgba(250, 250, 250, 0.0)")
    (swap! gradients assoc key [[context x y radius] grd])
    grd))

(defn attention-gradient [key context x y radius]
  (if-let [[args cached-value] (@gradients key)]
    (if (= args [context x y radius])
      cached-value
      (do
        ;(log "CACHE: Args differ")
        (mk-gradient key context x y radius)))
    (do
      ;(log "CACHE: Cache empty")
      (mk-gradient key context x y radius))))



(defn render-attention [key context x y radius timestamp]
  (let [gradient (attention-gradient key context x y radius)]
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
        offset [(/ (- width (* sf (:width level))) 2) (/ (- height (* sf (:height level))) 2)]
        channels (:channels level)
        colours (:colours level)
        [start] (:start level)
        [end] (:end level)
        ends #{start end}]
    (set! (. context -lineJoin) "round")
    (set! (. context -lineCap) "butt")
    (if @done
      (render-attention :next context width height (/ (min width height) 2) timestamp))
    (-> level
        (update :shapes #(doall (map (partial render-at-rest context sf offset mouse channels colours ends timestamp) % (range))))
        (update :shapes #(doall (map (partial render-in-motion context sf offset mouse channels colours ends timestamp) % (range))))
        (render-start context timestamp colours sf offset)
        (render-end context timestamp colours sf offset done)
        )
    ;; TODO: transitioning shapes are rendered twice!
    ))

