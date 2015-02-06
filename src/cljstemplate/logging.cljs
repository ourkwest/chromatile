(ns cljstemplate.logging)


(def config {:core false
             :shape true})

(defn logger [log-name]
  (if (config log-name)
    (fn [x] (.log js/console (str "<" (name log-name) ">") x) x)
    identity))