(ns cljstemplate.core
  (:use [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        [cljstemplate.shapeconstance :only [square-pad square-radius
                                            hex-pad hex-radius
                                            tri-pad tri-radius]]
        [cljstemplate.logging :only [logger log-when-changes]]
        [cljstemplate.shape :only [render check-connections do-rotations]]
        [cljstemplate.levels :only [get-level shuffle-shapes]]
        )
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
  ;[clojure.browser.repl :as repl]

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

;(def msg-bus (chan))

(defn handle-msg [msg]

  ;(if top?
  ;  (do (set! (.-top (.-style (dom/getElement "msgBus"))) "10px")
  ;      (set! (.-bottom (.-style (dom/getElement "msgBus"))) "auto"))
  ;  (do (set! (.-top (.-style (dom/getElement "msgBus"))) "auto")
  ;      (set! (.-bottom (.-style (dom/getElement "msgBus"))) "10px")))

  (set! (.-transition (.-style (dom/getElement "msgBus"))) "bottom 0s")

  (set! (.-bottom (.-style (dom/getElement "msgBus"))) "100%")
  (set! (.-innerHTML (dom/getElement "msgBus")) msg)

  (.setTimeout
    js/window
    (fn []
      (set! (.-transition (.-style (dom/getElement "msgBus"))) "bottom 0.5s")
      (set! (.-transitionTimingFunction (.-style (dom/getElement "msgBus"))) "ease-out") ;cubic-bezier(0.42,0,0.58,1)
      (set! (.-bottom (.-style (dom/getElement "msgBus"))) "10px")) 10)

  )

;(go (while true
;      (handle-msg (<! msg-bus))))

;;;;;;;;;;

(def pointer-state (atom nil))

(defn handle-click [event]
  (let [rect (.getBoundingClientRect (dom/getElement "theCanvas"))]
    (reset! pointer-state {:x (- (.-clientX event) (.-left rect))
                           :y (- (.-clientY event) (.-top rect))
                           :clicked true})))

(let [clicks (listen (dom/getElement "theCanvas") "mousedown")]
  (go (while true
        (handle-click (<! clicks)))))

(defn handle-moves [event]
  (let [rect (.getBoundingClientRect (dom/getElement "theCanvas"))]
    (swap! pointer-state merge {:x (- (.-clientX event) (.-left rect))
                                :y (- (.-clientY event) (.-top rect))})))

(let [moves (listen (dom/getElement "theCanvas") "mousemove")]
  (go (while true
        (handle-moves (<! moves)))))

;;;;;;;;;;

(let [clicks (listen (dom/getElement "nextButton") "click")]
  (go (while true
        (<! clicks)
        (level-up inc))))

;;;;;;;;;

(defn done-fn [msg]
  (set! (.-visibility (.-style (dom/getElement "nextButton"))) "visible")
  (handle-msg msg))

(def this-level-id (atom 0))
(def this-level (atom nil))
(def level-checked (atom false))
(def shuffles-so-far (atom 0))
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
  (let [{x :x y :y clicked :clicked} @pointer-state
        was-done @done
        canvas (if @level-checked canvas [(first canvas) 1 1])]

    ;(log (str [x y clicked timestamp]))

    (swap! this-level #(render canvas % [x y clicked timestamp] timestamp done))

    (cond
      (and @level-checked @done was-done) nil
      (and @level-checked @done (not was-done)) (done-fn (:end-msg @this-level))
      (and (not @level-checked) (not @done)) (reset! level-checked true)
      (and (not @level-checked) @done) (if (< 3 (swap! shuffles-so-far inc))
                                         (level-up identity)
                                         (do
                                           (log "Shuffling")
                                           (swap! this-level shuffle-shapes)
                                           (reset! done false)))
      :else nil))

  (swap! pointer-state dissoc :clicked))

(defn animate [timestamp]
  (per-frame-processing timestamp)

  ;(js/setTimeout
  ;  (fn [] (.requestAnimationFrame js/window animate)) 330)

  (.requestAnimationFrame js/window animate)
  )


(defn level-up [level-fn]
  (set! (.-visibility (.-style (dom/getElement "nextButton"))) "hidden")
  (swap! this-level-id level-fn)
  (log (str "Loading level " @this-level-id))
  (reset! this-level (get-level @this-level-id))
  (reset! level-checked false)
  (reset! done false)
  (reset! shuffles-so-far 0)
  (set! (.-innerHTML (dom/getElement "levelCounter")) (str (inc @this-level-id)))
  (handle-msg (:start-msg @this-level))

  ;(log (str @this-level))
  )


(defn begin []
  (level-up (fn [x] 0))
  (.requestAnimationFrame js/window animate))

(begin)

;(level-up (fn [x] 12))
