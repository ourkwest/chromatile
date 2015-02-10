(ns cljstemplate.core
  (:use [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        [cljstemplate.shapeconstance :only [square-pad square-radius
                                            hex-pad hex-radius
                                            tri-pad tri-radius]]
        [cljstemplate.logging :only [logger log-when-changes]]
        [cljstemplate.shape :only [level-1 render check-connections do-rotations]]
        [cljstemplate.levels :only [level-2 load-level]]
        )
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <! close!]])
  (:import [goog Uri]))


;; (repl/connect "http://localhost:9000/repl")


(def log (logger :core))

;;;;;;;;;;

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

(defn load-next []
  (log "Load!")
  (set! (.-location js/window) next-location))

(let [clicks (listen (dom/getElement "nextButton") "click")]
  (go (while true
        (<! clicks)
        (load-next))))

;;;;;;;;;

(defn done-fn []
  (set! (.-visibility (.-style (dom/getElement "nextButton"))) "visible")
  (log "DONE!"))

(def done (atom false))

;;;;;;;;;

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

(defn clear [[surface w h] color]
  (fill-rect [surface w h] [0 0 w h] color))

;;;;;;;;;;


(defn per-frame-processing [timestamp]
  (clear canvas (first (:colours @this-level)))
  (swap! this-level check-connections)
  (swap! this-level (partial do-rotations timestamp))
  (let [{x :x y :y} @pointer-state
        was-done @done]
    (swap! this-level #(render canvas % [x y timestamp] timestamp done))
    (if (and (not was-done) @done)
      (done-fn)))
  (reset! pointer-state nil))

(defn animate [timestamp]
  (per-frame-processing timestamp)
  (.requestAnimationFrame js/window animate))

(defn start []
  (let [uri (Uri. (.-location js/window))
        level-str (.getParameterValue uri "level")
        level (if (and level-str (re-matches #"\d+" level-str))
                (js/parseInt level-str)
                1)
        next-uri (.setParameterValue uri "level" (str (inc level)))]
    (def this-level (atom (load-level level)))
    (def next-location next-uri)
    (log (str "Level is: " level))
    (log (str @this-level)))
  (.requestAnimationFrame js/window animate))

(start)
