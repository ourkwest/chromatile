(ns cljstemplate.core
  (:use [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        [cljstemplate.shapeconstance :only [square-pad square-radius
                                            hex-pad hex-radius
                                            tri-pad tri-radius]]
        [cljstemplate.logging :only [logger]])
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

(def square1 (path [150 150 0]))
(def hex1 (path square1 [TAU_4TH square-pad] [0 hex-pad]))
(def tri1 (path square1 [PI square-pad] [0 tri-pad]))
(def square2 (path tri1 [(- TAU_6TH) tri-pad] [0 square-pad]))

(defn square-at [[x y r]] {:x x :y y :n 4 :rotation (+ r TAU_8TH)})
(defn hex-at [[x y r]] {:x x :y y :n 6 :rotation (+ r TAU_12TH)})
(defn tri-at [[x y r]] {:x x :y y :n 3 :rotation r})

(swap! shapes conj (square-at square1))
(swap! shapes conj (square-at square2))
(swap! shapes conj (hex-at hex1))
(swap! shapes conj (tri-at tri1))

;(swap! shapes conj {:x 150 :y 150 :n 4 :rotation (/ TAU_4TH 2)})
;(swap! shapes conj {:x (+ 150 square-pad hex-pad) :y 150 :n 6 :rotation 0})
;(swap! shapes conj {:x 150 :y (+ 150 square-pad tri-pad) :n 3 :rotation 0})



(defn render-square [[surface] {x :x y :y r :rotation}]
  (set! (. surface -strokeStyle) (str "black"))
  (set! (. surface -fillStyle) (str "rgba(200, 200, 0, 0.5)"))
  (let [theta1 r
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
    (if (.isPointInPath surface (:x @pointer-state) (:y @pointer-state))
      (.fill surface))
    (. surface (stroke))))

(defn render-hex [[surface] {x :x y :y r :rotation}]
  (set! (. surface -strokeStyle) (str "black"))
  (set! (. surface -fillStyle) (str "rgba(200, 200, 0, 0.5)"))
  (let [theta1 r
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
    (if (.isPointInPath surface (:x @pointer-state) (:y @pointer-state))
      (.fill surface))
    (. surface (stroke))))

(defn render-tri [[surface] {x :x y :y r :rotation}]
  (set! (. surface -strokeStyle) (str "black"))
  (set! (. surface -fillStyle) (str "rgba(200, 200, 0, 0.5)"))
  (let [theta1 r
        theta2 (+ theta1 TAU_3RD)
        theta3 (+ theta2 TAU_3RD)
        radius tri-radius]
    (. surface (beginPath))
    (. surface (moveTo (+ x (* radius (Math.sin theta1))) (+ y (* radius (Math.cos theta1)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta2))) (+ y (* radius (Math.cos theta2)))))
    (. surface (lineTo (+ x (* radius (Math.sin theta3))) (+ y (* radius (Math.cos theta3)))))
    (. surface (closePath))
    (if (.isPointInPath surface (:x @pointer-state) (:y @pointer-state))
      (.fill surface))
    (. surface (stroke))))


(defn render-shape [surface shape]
  (cond
    (= 3 (:n shape)) (render-tri surface shape)
    (= 4 (:n shape)) (render-square surface shape)
    (= 6 (:n shape)) (render-hex surface shape)
    ))


(defn listen [el type]
  (let [out (chan)]
    (events/listen el type
                   (fn [e] (put! out e)))
    out))



;;;;;;;;;;

(def pointer-state (atom nil))

(defn handle-click [event]
  (let [rect (.getBoundingClientRect (dom/getElement "myCanvas"))]
    (reset! pointer-state {:x (- (.-clientX event) (.-left rect))
                           :y (- (.-clientY event) (.-top rect))})))

(let [clicks (listen (dom/getElement "myCanvas") "click")]
  (go (while true
        (handle-click (<! clicks)))))

;;;;;;;;;;




(defn reset-canvas []
  (let [width (. js/window -innerWidth)
        height (. js/window -innerHeight)
        c (dom/getElement "myCanvas")]
    (set! (.-width c) width)
    (set! (.-height c) height)
    (set! (.-width (.-style c)) width)
    (set! (.-height (.-style c)) height)
    (def canvas [(.getContext c "2d") width height])))

(reset-canvas)

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

;(def time-state (atom {:last-frame 0}))

(defn render-shape [shape timestamp coords]                 ;...
  )

(defn clicked [shape]                                       ;...
  )

(defn process-shape [shape timestamp coords]
  ;(render-shape canvas shape)
  ; timestamp for rendering rotation, coords for mutation following clicks
  (if (render-shape shape timestamp coords)
    (clicked shape)
    shape))

(defn per-shape-processor [timestamp coords]
  (fn [shape] (process-shape shape timestamp coords)))


(defn per-frame-processing [timestamp]

  ;(swap! time-state assoc :last-frame timestamp)

  ;; if clicked since last frame then check all shapes for having been clicked on

  (clear canvas [250 175 0])

  (let [coords (if @pointer-state [(:x @pointer-state) (:y @pointer-state)] nil)
        new-shapes (doall (map (per-shape-processor timestamp coords) @shapes))]
    (reset! shapes new-shapes))

  (reset! pointer-state nil)

  ;(doall (map #(render-shape canvas %) @shapes))

  ;(fill-arc canvas [(:x @pointer-state) (:y @pointer-state) 5] [0 TAU] [100 0 0 0.5])

  )




(defn animate [timestamp]
  (per-frame-processing timestamp)
  (.requestAnimationFrame js/window animate))

(.requestAnimationFrame js/window animate)