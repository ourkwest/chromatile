(ns cljstemplate.levels
  (:use [cljstemplate.logging :only [logger log-when-changes]]

        [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        [cljstemplate.shapeconstance :only [square-pad square-radius square-inner-radius
                                            hex-pad hex-radius hex-inner-radius
                                            tri-pad tri-radius tri-inner-radius
                                            oct-pad oct-radius oct-inner-radius
                                            shape-side-length]]
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
        new-shape (if (= 0 n)
                    []
                    [{:n n :location (path [x y r] [0 my-pad] [PI 0])}])
        my-angle (angles n)

        neighbours-pairs (partition 2 neighbours)

        neighbour-count (count neighbours-pairs)
        neighbour-angles (iterate #(+ % my-angle) (+ PI my-angle))

        neighbour-shapes (apply concat (for
                                         [i (range neighbour-count)]
                                         (do
                                           ;(log (str "i: " i ", a: " (nth neighbour-angles i)))
                                           (mk-shapes [] (path [x y r] [0 my-pad] [(nth neighbour-angles i) my-pad]) (nth neighbours-pairs i)))))
        more-shapes (if rest (mk-shapes [] [x y r] rest) [])
        ]
    ;(log (str {:n n :r r :my-angle my-angle :neighbour-angles (take 4 neighbour-angles)}))
    (vec (concat shapes new-shape neighbour-shapes more-shapes))))


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

(def padding (round2 2 (* 1.1 oct-radius)))

(defn centre [shapes]
  (let [xs (map first (map :location shapes))
        ys (map second (map :location shapes))
        min-x (reduce min xs)
        min-y (reduce min ys)
        max-x (reduce max xs)
        max-y (reduce max ys)
        new-shapes (translate shapes (- padding min-x) (- padding min-y))
        ]
    ;(log (str {:xs xs :min-x min-x :ys ys :min-y min-y :max-x max-x :max-y max-y}))
    [new-shapes (+ (* 2 padding) (- max-x min-x)) (+ (* 2 padding) (- max-y min-y))]))

(defn add-shape-wires [channel-count shape]
  (assoc shape :wiring (vec (for [i (range channel-count)]
                              (vec (for [j (range (rand-int 2))]
                                     (let [a (rand-int (:n shape))
                                           b (rand-int (:n shape))]
                                       (if (= a b)
                                         [(mod (inc a) (:n shape)) b]
                                         [a b]))))
                              ))))
(defn add-blank-wires [channel-count shape]
  (assoc shape :wiring (vec (for [i (range channel-count)]
                              []
                              ))))

(defn add-wires [shapes channel-count]
  (mapv (partial add-shape-wires channel-count) shapes))

(defn blank-wires [shapes channel-count]
  (mapv (partial add-blank-wires channel-count) shapes))



(defn endpoint-wiring [channel-count direction shape]
  (assoc shape :wiring (vec (for [i (range channel-count)]
                              (vec (for [j (range (:n shape))]

                                     (if (= direction 1)
                                       [j 9]
                                       [9 j])

                                     ;[j (mod (inc j) (:n shape))]
                                     ))
                              ))))


(defn add-endpoint-wiring [shapes shape-id channel-count direction]
  (update-in shapes [shape-id] (partial endpoint-wiring channel-count direction)))


(defn get-sides [shape]
  (let [{[x y r] :location n :n} shape
        shape-angle (angles n)
        radius (pads n)]
    (for [side-angle (take n (iterate #(+ % shape-angle) r))]
      (let [side-x (+ x (* radius (Math/sin side-angle)))
            side-y (+ y (* radius (Math/cos side-angle)))]
        [side-x side-y shape]))))

(defn close-enough [[x1 y1] [x2 y2]]
  (let [x-diff (- x2 x1)
        y-diff (- y2 y1)
        h2 (+ (* x-diff x-diff) (* y-diff y-diff))
        limit (/ shape-side-length 3)
        l2 (* limit limit)]
    (< h2 l2)))

(defn find-neighbours [shapes [x y shape]]
  (some identity (for [i (range (count shapes))]
                   (cond
                     (= shape (nth shapes i)) nil
                     (some (partial close-enough [x y]) (get-sides (nth shapes i))) i))))

(defn add-neighbours [shapes]
  (mapv #(assoc % :neighbours (mapv (partial find-neighbours shapes) (get-sides %))) shapes))


(defn wire [level shape-id wiring]
  (assoc-in level [:shapes shape-id :wiring] wiring))

(defn shuffle [shapes]
  (mapv #(merge % {:rotation {:position (rand-int (:n %))}}) shapes))

(defn un-shuffle [shapes]
  (mapv #(merge % {:rotation {:position 0}}) shapes))

(defn shuffle-shapes [level]
  (update level :shapes shuffle))

(defn mk-level [start-location data [start-index end-index] colours channels]
  (let [shapes0 (mk-shapes [] start-location data)
        shapes1 (round-shapes shapes0)
        [shapes2 width height] (centre shapes1)
        start (repeat (count channels) start-index)
        end (repeat (count channels) end-index)
        shapes25 (blank-wires shapes2 (count channels))
        shapes3 (add-endpoint-wiring shapes25 start-index (count channels) 0)
        shapes4 (add-endpoint-wiring shapes3 end-index (count channels) 1)
        shapes5 (add-neighbours shapes4)
        shapes6 (un-shuffle shapes5)]
    {:shapes   shapes6
     :width    width
     :height   height
     :channels channels
     :colours  colours
     :start    start
     :end      end}))

(defn add-msgs [level start-msg end-msg]
  (-> level
      (assoc :start-msg start-msg)
      (assoc :end-msg end-msg)))


(defn add-distance [distance ends {dte :temp-dte neighbours :neighbours :as shape}]
  (if (some ends neighbours)
    (assoc shape :temp-dte (min (or dte 1000000) distance))
    shape))

(defn has-distance [distance ends {dte :temp-dte neighbours :neighbours} id]
  (if (and (some ends neighbours) (= distance dte))
    id
    nil))

(defn add-distance-to-end [shapes distance ends]
  (let [new-shapes (mapv (partial add-distance distance ends) shapes)
        new-ends (into #{} (filter identity (map (partial has-distance distance ends) new-shapes (range))))
        new-distance (inc distance)]
    ;(log (str {:new-ends new-ends :new-distance new-distance} ))
    (if (and (seq ends) (< distance 20))
      (recur new-shapes new-distance new-ends)
      new-shapes)))

(defn index-of [s v]
  (loop [idx 0 items s]
    (cond
      (empty? items) nil
      (= v (first items)) idx
      :else (recur (inc idx) (rest items)))))

(defn add-path-to-end [here-id from-shape-id end-id travelled shapes channel-id]
  (if (= here-id end-id)
    shapes
    (let [shape (shapes here-id)
          neighbour-ids (:neighbours shape)
          available-ids (filter identity (filter #(not= from-shape-id %) neighbour-ids))
          min-dte (reduce min (map (comp :temp-dte shapes) available-ids))
          neighbour-id (if (< min-dte travelled)
                      (rand-nth (filter #(= min-dte (:temp-dte (shapes %))) available-ids))
                      (rand-nth available-ids))
          wire-to (index-of neighbour-ids neighbour-id)
          wire-from (index-of neighbour-ids from-shape-id)
          wire [wire-from wire-to]
          ;_ (log (str "Ids: " {:here here-id
          ;                     :from from-shape-id
          ;                     :n neighbour-ids
          ;                     :a available-ids
          ;                     :N neighbour-id}))
          ;_ (log (str "UPDATE-IN " [here-id :wiring channel-id]))
          new-shapes (update-in shapes [here-id :wiring channel-id] conj wire)]
      (recur neighbour-id here-id end-id (inc travelled) new-shapes channel-id))))


(defn merge-spam [shape [channel from onto]]
  (update-in shape [:wiring channel] conj [from onto]))

(defn mk-filter [collection]
  (fn [[a b]] (not (and (< a b) (some #{[b a]} collection)))))

(defn de-dupe-channel-spam [channel-wiring]
  ;{:pre  [(not= 1 (log (str "PRE: " channel-wiring)))]
  ; :post [(not= 1 (log (str "POST: " %)))]}
  (filterv (mk-filter channel-wiring) (into [] (into #{} channel-wiring))))

(defn de-dupe-spam [shape]
  (update-in shape [:wiring] #(mapv de-dupe-channel-spam %)))

(defn spam-each [{wiring :wiring n :n :as shape}]
  (let [total-wires (/ (count (flatten wiring)) 2)
        target-wires (+ 1 (rand-int (- n 1)))
        channel-count (count wiring)]
    ;(log (str "Spamming: " n ", " total-wires " / " target-wires "(" wiring ")"))
    (de-dupe-spam
      (reduce merge-spam
              shape
              (for [i (range total-wires target-wires)]
                (let [a (rand-int n)
                      b (rand-int n)]
                  (if (= a b)
                    [(rand-int channel-count) (mod (inc a) n) b]
                    [(rand-int channel-count) a b])))))))

(defn spam [shapes]
  (mapv spam-each shapes))

(defn wire-paths [{[start] :start [end] :end shapes :shapes channels :channels :as level}]
  (let [shapes0 (add-distance-to-end shapes 0 #{end})
        first-id (some identity (:neighbours (shapes start)))
        shapes1 (reduce (partial add-path-to-end first-id start end 0) shapes0 (range (count channels)))
        shapes2 (spam shapes1)]
    (assoc level :shapes shapes2)))


(def blue-on-orange [[250 175 0] [0 0 250] [0 150 225]])
(def orange-yellow-red-channels [[250 175 0] [200 250 0] [250 100 0]])
;(def orange-blue-2 (butlast orange-yellow-red-channels))
;(def orange-blue-1 (butlast orange-blue-2))

(def green-on-pink [[255 150 200] [255 255 255] [100 0 50]])
(def red-yellow-blue-channels [[200 200 0] [250 50 50] [150 150 255]])

(def red-white [[250 50 50] [150 0 0] [250 250 250]])
(def red-white-3 [[200 50 150] [255 0 0] [255 125 125]])

(def black-on-lightgrey [[200 200 200] [255 255 255] [0 0 0]])
(def cyan-magenta-yellow-channels [[250 250 0] [250 0 250] [0 250 250]])

(def grey-on-black [[0 0 0] [250 250 250] [125 125 125]])
(def red-purple-lilac-channels [[250 0 0] [200 0 200] [175 175 255]])

(def all-colours
  [
   [blue-on-orange orange-yellow-red-channels]
   [green-on-pink red-yellow-blue-channels]
   ;[red-white red-white-3] can't make this look good!
   [black-on-lightgrey cyan-magenta-yellow-channels]
   [grey-on-black red-purple-lilac-channels]
   ])


(def tutorial-levels
  [(-> (mk-level
         [0 0 PI]
         [4 [4 [0 []
                4 [0 []
                   4 [0 []
                      4 [0 []
                         4 [0 []
                            4 []]]]]]]]
         [0 6]
         blue-on-orange
         (butlast (butlast orange-yellow-red-channels)))
       (wire 1 [[[0 2]]])
       (wire 2 [[[0 2]]])
       (wire 3 [[[1 3]]])
       (wire 4 [[[0 2]]])
       (wire 5 [[[0 2]]])
       (add-msgs "Click on shapes to rotate them. Complete the path." "Awesome! You did it.")
       )
   (-> (mk-level
         [0 0 PI]
         [4 [4 [0 []
                4 [4 [0 []
                      0 []
                      4 []]
                   4 [0 []
                      4 [4 [0 []
                            0 []
                            4 [0 []
                               4 []]]]]]]]]
         [0 9]
         blue-on-orange
         (butlast orange-yellow-red-channels))
       (wire 1 [[[0 2]] [[0 2]]])
       (wire 2 [[[1 0]] [[0 2]]])
       (wire 3 [[[0 3]] []])
       (wire 4 [[[1 3]] []])
       (wire 5 [[]      [[1 3]]])
       (wire 6 [[] [[1 2]]])
       (wire 7 [[[3 1]] [[0 3]]])
       (wire 8 [[[0 2]] [[0 2]]])
       (add-msgs "Sometimes you need to create more than one path." "Well done!")
       )
   (-> (mk-level
         [0 0 PI]
         [4 [4 [0 []
                4 [4 [0 []
                      4 [0 []
                         0 []
                         4 []]]
                   4 [4 []
                      4 [4 [0 []
                            4 [0 []
                               0 []
                               4 [0 []
                                  4 []]]]]]]]]]
         [0 12]
         blue-on-orange
         orange-yellow-red-channels)
       (wire 1 [[[0 2]] [[0 2]] [[0 2]]])
       (wire 2 [[[0 1]] [[0 1]] [[0 2]]])
       (wire 3 [[[0 2]] [[0 3]] [     ]])
       (wire 4 [[[2 1]] [     ] [     ]])
       (wire 5 [[[1 3]] [     ] [     ]])
       (wire 6 [[     ] [     ] [[1 3]]])
       (wire 7 [[     ] [[0 2]] [     ]])
       (wire 8 [[     ] [     ] [[1 2]]])
       (wire 9 [[     ] [[3 0]] [[0 2]]])
       (wire 10 [[[1 3]] [[0 3]] [[0 3]]])
       (wire 11 [[[1 3]] [[1 3]] [[1 3]]])
       (add-msgs "There may be as many as three paths." "You're getting the hang of this!")
       )
   (-> (mk-level
         [0 0 (/ PI 2)]
         [4 [4 [0 []
                4 []
                4 [4 []]]
             4 [0 []
                4 [4 [0 []
                      4 [0 []
                         0 []
                         4 []]]
                   4 [4 []
                      4 [4 [0 []
                            4 [0 []
                               0 []
                               4 [0 []
                                  4 [0 []
                                     0 []
                                     4 [0 []
                                        4 []
                                        4 [4 []]]]]]]]]]]]]
         [0 16]
         blue-on-orange
         orange-yellow-red-channels)
       (wire 5  [[[0 2]] [[0 2]] [[0 2]]])
       (wire 6  [[[0 1]] [[0 1]] [[0 2]]])
       (wire 7  [[[0 2]] [[0 3]] [     ]])
       (wire 8  [[[0 3]] [     ] [     ]])
       (wire 9  [[[0 2]] [     ] [     ]])
       (wire 10 [[     ] [     ] [[0 2]]])
       (wire 11 [[     ] [[1 3]] [     ]])
       (wire 12 [[     ] [     ] [[0 1]]])
       (wire 13 [[     ] [[1 2]] [[0 2]]])
       (wire 14 [[[1 3]] [[0 3]] [[0 3]]])
       (wire 15 [[[0 2]] [[0 2]] [[0 2]]])
       (wire 2  [[[0 2]] [[0 2]] [[0 2]]])
       (wire 18 [[[0 1]] [[0 2]] [[0 3]]])
       (add-msgs "You don't always have to use all the pieces." "Wow! Onto the real levels...")
       )])


(def unfinished-levels
    [
     (partial mk-level
              [0 0 PI]
              [4 [4 [0 []
                     4 [0 []
                        0 []
                        4 [0 []
                           4 []]]
                     4 [0 []
                        4 []]]
                  4 [0 []
                     4 []]]]
              [8 2])
     (partial mk-level
              [0 0 PI]
              [6 [6 []
                  6 [0 []
                     6 []
                     6 [0 []
                        6 []]
                     6 []]
                  6 []
                  6 []]]
              [8 5])
     (partial mk-level
              [0 0 PI]
              [6 [3 []
                  3 [6 [0 []
                        0 []
                        3 [0 []
                           6 [0 []
                              0 []
                              6 []]]
                        3 []]]
                  6 [3 []
                     3 [6 [0 []
                           0 []
                           3 [0 []
                              6 []]
                           3 []]]]
                  6 []]]
              [15 6])
     (partial mk-level
              [0 0 PI]
              [6 [4 [0 []
                     6 [0 []
                        0 []
                        0 []
                        4 [0 []
                           6 [0 []
                              0 []
                              0 []
                              4 []]]]]
                  4 [0 []
                     6 [4 [3 []]
                        4 [3 []]
                        4 [3 []]
                        4 [3 []]
                        4 [3 []]]
                     3 []]
                  4 [0 []
                     6 [0 []
                        4 [0 []
                           6 [0 []
                              4 [0 []
                                 6 []]]]]]]]
              [20 4])
     (partial mk-level
              [0 0 PI]
              [8 [8 []
                  4 [0 []
                     8 [0 []
                        4 []
                        8 []
                        4 []
                        8 []
                        4 []]]
                  8 []
                  4 []
                  8 []
                  4 []
                  8 []
                  4 []]]
              [11 5])
     ])


(def start-messages
  [
   "Good luck!"
   "You're doing really well!"
   "You're really good at this game!"
   "May the hugs of a thousand kittens be with you."
   "<img src=\"https://placekitten.com/g/300/300\" width=\"50px\" height=\"50px\" />"
   ])

(def end-messages
  [
   "Wow!"
   "Awesome!"
   "You did it!"
   "You totally nailed that level!"
   "You're really good at this game."
   "You must be, like, a genius or something."
   ])

(defn add-random-msg [level]
  (add-msgs level (rand-nth start-messages) (rand-nth end-messages)))

(defn pick-channels [number channels]
  (take number (drop (rand-int 3) (cycle channels))))

(defn finish [level channel-count [colours channels]]
  (-> (level colours (pick-channels channel-count channels))
      wire-paths
      add-random-msg))

(defn finish-level [n]
  (let [index (mod (int (/ n 3)) (count unfinished-levels))
        channel-count (inc (mod n 3))
        level (unfinished-levels index)
        colours (rand-nth all-colours)]
    (finish level channel-count colours)))

(defn get-level [n]
  (let [t (count tutorial-levels)]
    (if (< n t)
      (tutorial-levels n)
      (finish-level (- n t)))))

