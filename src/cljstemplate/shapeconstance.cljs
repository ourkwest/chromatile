(ns cljstemplate.shapeconstance
  (:use [cljstemplate.constance :only [PI TAU TAU_3RD TAU_4TH TAU_6TH TAU_8TH TAU_12TH
                                       ROOT_TWO ROOT_THREE]]
        ))


(def shape-side-length 50)
(defn shape-pad [inner-radius]
  (* inner-radius 1.05))

(def square-side shape-side-length)
(def sqaure-inner-radius (/ square-side 2))
(def square-radius (* sqaure-inner-radius ROOT_TWO))
(def square-pad (shape-pad sqaure-inner-radius))

(def hex-side shape-side-length)
(def hex-radius hex-side)
(def hex-inner-radius (* hex-side (/ ROOT_THREE 2)))
(def hex-pad (shape-pad hex-inner-radius))

(def tri-side shape-side-length)
(def half-tri-side (/ tri-side 2))
(def tri-radius (/ half-tri-side (/ ROOT_THREE 2)))
(def tri-inner-radius (/ tri-radius 2))
(def tri-pad (shape-pad tri-inner-radius))