(ns cljstemplate.levels
  (:use [cljstemplate.logging :only [logger log-when-changes]]

        [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        [cljstemplate.shapeconstance :only [square-pad square-radius square-inner-radius
                                            hex-pad hex-radius hex-inner-radius
                                            tri-pad tri-radius tri-inner-radius
                                            oct-pad oct-radius oct-inner-radius]]
        ))



(def log (logger :levels))



(defn apply-step [[x y rotation] [angle distance]]
  (let [new-rotation (+ rotation angle)
        new-x (+ x (* distance (Math/sin new-rotation)))
        new-y (+ y (* distance (Math/cos new-rotation)))]
    [new-x new-y new-rotation]))


(defn path [location & steps]
  (if-let [step (first steps)]
    (recur (apply-step location step) (rest steps))
    location))

(defn path-dbg [location & steps]
  (log (str "Path: " location " -> " (apply path location steps)))
  (apply path location steps))


(def pads {3 tri-pad
           4 square-pad
           6 hex-pad
           8 oct-pad})

(def angles {3 TAU_3RD
             4 TAU_4TH
             6 TAU_6TH
             8 TAU_8TH})


(defn mk-shapes [shapes [x y r] [n neighbours & rest]]
  ;(log (str "Called with " n ", " neighbours (if rest " and more..." "")))
  (let [my-pad (pads n)
        new-shape {:n n :location (path [x y r] [0 my-pad] [PI 0])}
        my-angle (angles n)

        neighbours-pairs (partition 2 neighbours)

        neighbour-count (count neighbours-pairs)
        neighbour-angles (iterate #(+ % my-angle) (+ PI my-angle))

        neighbour-shapes (apply concat (for
                                         [i (range neighbour-count)]
                                         (do
                                           (log (str "i: " i ", a: " (nth neighbour-angles i)))
                                           (mk-shapes [] (path-dbg [x y r] [0 my-pad] [(nth neighbour-angles i) my-pad]) (nth neighbours-pairs i)))))
        more-shapes (if rest (mk-shapes [] [x y r] rest) [])
        ]
    (log (str {:n n :r r :my-angle my-angle :neighbour-angles (take 4 neighbour-angles)}))
    (vec (concat shapes [new-shape] neighbour-shapes more-shapes))))


(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))


(defn round-location [[x y r]]
  [(round2 2 x) (round2 2 y) (round2 4 r)])

(defn round-shapes [shapes]
  (mapv #(update-in % [:location] round-location) shapes))




(defn translate-each [by-x by-y shape]
  (-> shape
      (update-in [:location 0] + by-x)
      (update-in [:location 1] + by-y)))

(defn translate [shapes by-x by-y]
  (mapv (partial translate-each by-x by-y) shapes))

(def padding (* 1.1 oct-radius))

(defn centre [shapes]
  (let [xs (map first (map :location shapes))
        ys (map second (map :location shapes))
        min-x (reduce min xs)
        min-y (reduce min ys)
        max-x (reduce max xs)
        max-y (reduce max ys)
        new-shapes (translate shapes (- padding min-x) (- padding min-y))
        ]
    [new-shapes (- max-x min-x) (- max-y min-y)]))

(defn add-shape-wires [channel-count shape]
  (assoc shape :wiring (vec (for [i (range channel-count)]
                              (vec (for [j (range (rand-int 2))]
                                     (let [a (rand-int (:n shape))
                                           b (rand-int (:n shape))]
                                       (if (= a b)
                                         [(mod (inc a) (:n shape)) b]
                                         [a b]))))
                              ))))

(defn add-wires [shapes channel-count]
  (mapv (partial add-shape-wires channel-count) shapes))



(defn endpoint-wiring [channel-count shape]
  (assoc shape :wiring (vec (for [i (range channel-count)]
                              (vec (for [j (range (:n shape))]
                                     [j (mod (inc j) (:n shape))]))
                              ))))


(defn add-endpoint-wiring [shapes shape-id channel-count]
  (update-in shapes [shape-id] (partial endpoint-wiring channel-count)))



(defn fake [shapes]
  (mapv #(merge % {:neighbours [nil nil nil nil nil nil] :rotation {:position 0}}) shapes))

(defn mk-level [data]
  (let [start-location [0 0 0]
        shapes0 (mk-shapes [] start-location data)
        shapes1 (round-shapes shapes0)
        [shapes10 width height] (centre shapes1)
        colours [[250 175 0] [0 0 250] [0 150 225]]
        channels [[250 175 0] [255 125 50] [200 225 0]]
        start [0 0 0]
        end [3 3 3]
        shapes2 (add-wires shapes10 (count channels))
        shapes3 (add-endpoint-wiring shapes2 0 3)
        shapes4 (add-endpoint-wiring shapes3 3 3)
        shapes5 (fake shapes4)]
    {:shapes   shapes5
     :width    width
     :height   height
     :channels channels
     :colours  colours
     :start    start
     :end      end}))

(def one
  [6 [4 [3 [] 8 []]
      4 [3 []]
      4 [3 [] 6 []]
      4 [3 [] 6 []]
      4 [3 [] 6 []]
      4 [3 []]]])


(def level-2 (mk-level one))
;
;(log (str level-2))



;(log (round2 2 123.456))


;(defn thing []
;  (log (mk-shapes [] [100 100 0] one)))
