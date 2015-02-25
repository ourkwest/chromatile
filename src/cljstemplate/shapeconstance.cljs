(ns cljstemplate.shapeconstance
  (:use [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        ))


(def shape-side-length 50)
(defn shape-pad [inner-radius]
  (* inner-radius 1.05))

(def square-side shape-side-length)
(def square-inner-radius (/ square-side 2))
(def square-radius (* square-inner-radius ROOT_TWO))
(def square-pad (shape-pad square-inner-radius))

(def hex-side shape-side-length)
(def hex-radius hex-side)
(def hex-inner-radius (* hex-side (/ ROOT_THREE 2)))
(def hex-pad (shape-pad hex-inner-radius))

(def oct-side shape-side-length)
(def oct-radius (Math.sqrt (+ (* (/ oct-side 2) (/ oct-side 2))
                              (* (+ (/ oct-side 2) (/ oct-side ROOT_TWO)) (+ (/ oct-side 2) (/ oct-side ROOT_TWO))))))
(def oct-inner-radius (* oct-side (/ (+ 1 ROOT_TWO) 2)))
(def oct-pad (shape-pad oct-inner-radius))

(def tri-side shape-side-length)
(def half-tri-side (/ tri-side 2))
(def tri-radius (/ half-tri-side (/ ROOT_THREE 2)))
(def tri-inner-radius (/ tri-radius 2))
(def tri-pad (shape-pad tri-inner-radius))

;;;;;

(def pad-diff (- hex-pad hex-inner-radius))

(def tri-adjust (/ (- tri-pad pad-diff) tri-inner-radius))
(def tri-radius (* tri-radius tri-adjust))
(def tri-inner-radius (* tri-inner-radius tri-adjust))

(def square-adjust (/ (- square-pad pad-diff) square-inner-radius))
(def square-radius (* square-radius square-adjust))
(def square-inner-radius (* square-inner-radius square-adjust))

(def oct-adjust (/ (- oct-pad pad-diff) oct-inner-radius))
(def oct-radius (* oct-radius oct-adjust))
(def oct-inner-radius (* oct-inner-radius oct-adjust))

