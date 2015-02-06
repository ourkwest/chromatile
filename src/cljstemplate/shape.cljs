(ns cljstemplate.shape
  (:use [cljstemplate.logging :only [logger]]))



(def log (logger :shape))





;;

(def level-1 "Pre-created level. EDN?"
  {:shapes   [{:n          6
               :location   [150 150 3.1415]
               :neighbours [nil nil nil 1 nil nil]
               :wiring     [[[1 2 1] [1 4 0]]
                            [[5 6 0]]
                            [[1 3 0]]]
               :state      {:rotation 2}}
              {:n          6
               :location   [250 150 3.1415]
               :neighbours [0 nil nil nil nil nil]
               :wiring     [[[1 2] [1 4]]
                            [[5 6]]
                            [[1 3]]]
               :state      {:rotation {:start 2 :end 3 :start-time 123456789 :end-time 123457789}}}
              ]
   :channels [[250 0 0] [0 250 0] [0 0 250]]
   :start    [0 0 0]
   :end      [1 1 1]})


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

(defn more-seeds [shapes shape-id channel-id wire-id]
  )

(defn populate-shape-connections [shapes [[shape-id channel-id wire-id direction-id] & seeds]]
  ;; shapes [ [shape-id channel-id wire-id] ... ]

    (let [new-shapes (update shapes shape-id #(switch-on % channel-id wire-id direction-id))]
      (if seeds
        (recur new-shapes seeds)
        new-shapes))
  )

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




(log (str (take 20 (seed-light level-1))))

;(log (str level-1))
(log (str (check-connections level-1)))




;; map render shapes
;;  detect clicks and alter state if clicked
;;  render based on neighbours?

;; rendering





;; create
;;; creates data structure for a shape

(defn- crt-shp [n]
  {:n n})

(defn- crt-nchrd-shp [n x y r]
  (merge (crt-shp n) {:x x :y y :r r :mappings [[;; shape (atom), side (integer)
                                                 ]]}))

(defn create-shape [n]
  (atom (crt-shp n)))

(defn create-anchored-shape [n x y r]
  (atom (crt-nchrd-shp n x y r)))


;; neighbour ?
;;; link two shapes
(defn neighbour [anchored shape]
  )


;;  render
;;;  creates a render function (returns it or adds it to a map/list by z-order [cons/conj? - only two z-orders!])