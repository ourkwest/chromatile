(ns cljstemplate.core
  (:use [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        [cljstemplate.shapeconstance :only [square-pad square-radius
                                            hex-pad hex-radius
                                            tri-pad tri-radius]]
        [cljstemplate.logging :only [logger log-when-changes]]
        [cljstemplate.shape :only [level-1 render check-connections do-rotations]])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <!]]))

;; (repl/connect "http://localhost:9000/repl")

;(enable-console-print!)




(def log (logger :core))


(def shapes (atom []))


(defn apply-step [[x y rotation] [angle distance]]
  (let [new-rotation (+ rotation angle)
        new-x (+ x (* distance (Math/sin new-rotation)))
        new-y (+ y (* distance (Math/cos new-rotation)))]
    [new-x new-y new-rotation]))

(defn path [location & steps]
  (if-let [step (first steps)]
    (recur (apply-step location step) (rest steps))
    location))

(def square1 (path [(* 2 square-pad) (* 3 hex-pad) 0]))
(def hex1 (path square1 [TAU_4TH square-pad] [0 hex-pad]))
(def tri1 (path square1 [PI square-pad] [0 tri-pad]))
(def square2 (path tri1 [(- TAU_6TH) tri-pad] [0 square-pad]))
(def square3 (path hex1 [0 hex-pad] [0 square-pad]))
(def square4 (path hex1 [(- TAU_6TH) hex-pad] [0 square-pad]))
(def square5 (path hex1 [(+ TAU_6TH) hex-pad] [0 square-pad]))
(def square6 (path hex1 [(- TAU_3RD) hex-pad] [0 square-pad]))

(defn square-at [[x y r]] {:x x :y y :n 4 :rotation (+ r TAU_8TH)})
(defn hex-at [[x y r]] {:x x :y y :n 6 :rotation (+ r TAU_12TH)})
(defn tri-at [[x y r]] {:x x :y y :n 3 :rotation r})

(swap! shapes conj (square-at square1))
(swap! shapes conj (square-at square2))
(swap! shapes conj (square-at square3))
(swap! shapes conj (square-at square4))
(swap! shapes conj (square-at square5))
(swap! shapes conj (square-at square6))
(swap! shapes conj (hex-at hex1))
(swap! shapes conj (tri-at tri1))



;; SHAPES

;;; sides?
;;; rotatable mappings to sides on other shapes



;(swap! shapes conj {:x 150 :y 150 :n 4 :rotation (/ TAU_4TH 2)})
;(swap! shapes conj {:x (+ 150 square-pad hex-pad) :y 150 :n 6 :rotation 0})
;(swap! shapes conj {:x 150 :y (+ 150 square-pad tri-pad) :n 3 :rotation 0})

(def shape-fill "rgba(0, 150, 225, 1.0)")
(def shape-stroke "rgb(0, 0, 250)")

(defn render-square [[surface] {x :x y :y r :rotation offset :offset} coords]
  (set! (. surface -strokeStyle) (str shape-stroke))
  (set! (. surface -fillStyle) (str shape-fill))
  (let [theta1 (+ r (* (:current offset) TAU_4TH))
        theta2 (+ theta1 TAU_4TH)
        theta3 (+ theta2 TAU_4TH)
        theta4 (+ theta3 TAU_4TH)
        radius square-radius                                ;(/ shape-side-length ROOT_TWO)
        ]
    (. surface (beginPath))
    (. surface (moveTo (+ x (* radius (Math.sin theta1))) (+ y (* radius (Math.cos theta1)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta2))) (+ y (* radius (Math.cos theta2)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta3))) (+ y (* radius (Math.cos theta3)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta4))) (+ y (* radius (Math.cos theta4)))))
    (. surface (closePath))
    ;(if (.isPointInPath surface (:x @pointer-state) (:y @pointer-state))
    ;  (.fill surface))
    (. surface (fill))
    (. surface (stroke))
    (if-let [[x y] coords]
      (.isPointInPath surface x y))))

(defn render-hex [[surface] {x :x y :y r :rotation offset :offset} coords]
  (set! (. surface -strokeStyle) (str shape-stroke))
  (set! (. surface -fillStyle) (str shape-fill))
  (let [theta1 (+ r (* (:current offset) TAU_6TH))
        theta2 (+ theta1 TAU_6TH)
        theta3 (+ theta2 TAU_6TH)
        theta4 (+ theta3 TAU_6TH)
        theta5 (+ theta4 TAU_6TH)
        theta6 (+ theta5 TAU_6TH)
        radius hex-radius]
    (. surface (beginPath))
    (. surface (moveTo (+ x (* radius (Math.sin theta1))) (+ y (* radius (Math.cos theta1)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta2))) (+ y (* radius (Math.cos theta2)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta3))) (+ y (* radius (Math.cos theta3)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta4))) (+ y (* radius (Math.cos theta4)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta5))) (+ y (* radius (Math.cos theta5)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta6))) (+ y (* radius (Math.cos theta6)))))
    (. surface (closePath))
    ;(if (.isPointInPath surface (:x @pointer-state) (:y @pointer-state))
    ;  (.fill surface))
    (. surface (fill))
    (. surface (stroke))
    (if-let [[x y] coords]
      (.isPointInPath surface x y))))

(defn render-tri [[surface] {x :x y :y r :rotation offset :offset} coords]
  (set! (. surface -strokeStyle) (str shape-stroke))
  (set! (. surface -fillStyle) (str shape-fill))
  (let [theta1 (+ r (* (:current offset) TAU_3RD))
        theta2 (+ theta1 TAU_3RD)
        theta3 (+ theta2 TAU_3RD)
        radius tri-radius]
    (. surface (beginPath))
    (. surface (moveTo (+ x (* radius (Math.sin theta1))) (+ y (* radius (Math.cos theta1)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta2))) (+ y (* radius (Math.cos theta2)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta3))) (+ y (* radius (Math.cos theta3)))))
    (. surface (closePath))
    ;(if (.isPointInPath surface (:x @pointer-state) (:y @pointer-state))
    ;  (.fill surface))
    (. surface (fill))
    (. surface (stroke))
    (if-let [[x y] coords]
      (.isPointInPath surface x y))))


;(defn render-shape [surface shape]
;  (cond
;    (= 3 (:n shape)) (render-tri surface shape)
;    (= 4 (:n shape)) (render-square surface shape)
;    (= 6 (:n shape)) (render-hex surface shape)
;    ))


(defn listen [el type]
  (let [out (chan)]
    (events/listen el type
                   (fn [e] (put! out e)))
    out))


;;;;;;;;;;

(def pointer-state (atom nil))

(defn handle-click [event]
  (let [rect (.getBoundingClientRect (dom/getElement "theCanvas"))]
    (reset! pointer-state {:x (- (.-clientX event) (.-left rect))
                           :y (- (.-clientY event) (.-top rect))})))

(let [clicks (listen (dom/getElement "theCanvas") "click")]
  (go (while true
        (handle-click (<! clicks)))))


;;;;;;;;;;




(defn reset-canvas []
  (let [width (. js/window -innerWidth)
        height (. js/window -innerHeight)
        c (dom/getElement "theCanvas")]
    (set! (.-width c) width)
    (set! (.-height c) height)
    (set! (.-width (.-style c)) width)
    (set! (.-height (.-style c)) height)
    (def canvas [(.getContext c "2d") width height])))

(reset-canvas)
;
(let [resizes (listen js/window "resize")]
  (go (while true
        (<! resizes)
        (reset-canvas))))


;;;;;;;;;;



(defn fill-rect [[surface] [x y width height] [r g b]]
  (set! (. surface -fillStyle) (str "rgb(" r "," g "," b ")"))
  (.fillRect surface x y width height))

(defn stroke-rect [[surface] [x y width height] line-width [r g b]]
  (set! (. surface -strokeStyle) (str "rgb(" r "," g "," b ")"))
  (set! (. surface -lineWidth) line-width)
  (.strokeRect surface x y width height))

(defn fill-circle [[surface] coords [r g b]]
  (let [[x y d] coords]
    (set! (. surface -fillStyle) (str "rgb(" r "," g "," b ")"))
    (. surface (beginPath))
    (.arc surface x y d 0 (* 2 Math/PI) true)
    (. surface (closePath))
    (. surface (fill))))

(defn fill-arc [[surface] [x y radius] [start end] [r g b a]]
  (set! (. surface -fillStyle) (str "rgba(" r "," g "," b "," a ")"))
  (. surface (beginPath))
  (. surface (moveTo x y))
  (.arc surface x y radius start end true)
  (. surface (lineTo x y))
  (. surface (closePath))
  (. surface (fill)))

(defn stroke-arc [[surface] [x y radius] [start end] [r g b]]
  (set! (. surface -strokeStyle) (str "rgb(" r "," g "," b ")"))
  (set! (. surface -lineWidth) "5")
  (. surface (beginPath))
  (. surface (moveTo x y))
  (.arc surface x y radius start end true)
  (. surface (lineTo x y))
  (. surface (closePath))
  (. surface (stroke)))

(defn clear [[surface w h] color]
  (fill-rect [surface w h] [0 0 w h] color))


;;;;;;;;;;

(defn log-entry [entry]
  (log (str (first entry) "\t"  (first (rest entry)))))

(defn log-shape [shape]
  (log "--------")
  (doall (map log-entry (:offset shape)))
  shape)


(defn proportionalise "returns e" [[a b c] [d f]]
  (let [dist1 (- c a)
        prog1 (- b a)
        prop (/ prog1 dist1)
        dist2 (- f d)
        prog2 (* prop dist2)
        e (+ d prog2)]
    e))

(defn render-shape [shape timestamp coords]                 ;...
  (cond
    (= 3 (:n shape)) (render-tri canvas shape coords)
    (= 4 (:n shape)) (render-square canvas shape coords)
    (= 6 (:n shape)) (render-hex canvas shape coords)
    ))


(defn clicked [shape timestamp]                                       ;...
  (let [offset (:offset shape)
        current (:current offset)
        new-vals {:start-time timestamp
                  :end-time (+ (max current timestamp) 250)
                  :start current
                  :end (+ (:end offset) 1)}]
    ;(log-shape shape)
    (update-in shape [:offset] merge new-vals)))

(defn rotate [shape timestamp]
  (let [offset (:offset shape)
        start (:start offset)
        end (:end offset)
        start-time (:start-time offset)
        end-time (:end-time offset)
        position (proportionalise [start-time timestamp end-time] [start end])
        ]
    (cond
      (< end-time timestamp) (assoc-in shape [:offset :current] end)
      (< timestamp start-time) (assoc-in shape [:offset :current] start) ; should never happen?
      :else (assoc-in shape [:offset :current] position)
      )
))

(defn process-shape [shape timestamp coords]
  ; timestamp for rendering rotation, coords for mutation following clicks
  (if (render-shape shape timestamp coords)
    (clicked shape timestamp)
    (rotate shape timestamp)))

(defn per-shape-processor [timestamp coords]
  (fn [shape] (process-shape shape timestamp coords)))



(def this-level (atom level-1))


(defn per-frame-processing [timestamp]


  (clear canvas [250 175 0])


  ;(log-when-changes :level-1 (str @this-level))

  (swap! this-level check-connections)

  ;(log-when-changes :level-2 (str @this-level))

  (swap! this-level (partial do-rotations timestamp))

  (log-when-changes :level-3 (str @this-level))

  (let [{x :x y :y} @pointer-state]
    (swap! this-level #(render canvas % [x y timestamp])))

  (log-when-changes :level-4 (str @this-level))

  ;(log (str @this-level))

  (comment
    (let [coords (if @pointer-state [(:x @pointer-state) (:y @pointer-state)] nil)
          new-shapes (doall (map (per-shape-processor timestamp coords) @shapes))]
      (reset! shapes new-shapes)))

  (reset! pointer-state nil)



  )


(defn occassional-debug []
  (log (str @this-level)))


;(js/setInterval
;  occassional-debug 5000)


(defn animate [timestamp]
  (per-frame-processing timestamp)
  (.requestAnimationFrame js/window animate))

(.requestAnimationFrame js/window animate)