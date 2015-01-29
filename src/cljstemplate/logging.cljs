(ns cljstemplate.logging)


(def config {:core true})

(defn logger [log-name]
  (if (config log-name)
    (fn [x] (.log js/console (name log-name) x) x)
    identity))