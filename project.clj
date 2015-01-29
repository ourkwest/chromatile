(defproject cljstemplate "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2719"]
                 [org.clojure/core.async "0.1.256.0-1bf8cf-alpha"]]

  :node-dependencies [[source-map-support "0.2.8"]]

  :plugins [[lein-cljsbuild "1.0.4"]
            [lein-npm "0.4.0"]]

  :source-paths ["src" "target/classes"]

  :clean-targets ["out/cljstemplate" "cljstemplate.js" "cljstemplate.min.js"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src"]
              :compiler {
                :output-to "out/cljstemplate.js"
                :output-dir "out"
                :optimizations :none
                :cache-analysis true
                :source-map true}}
             ;{:id "release"
             ; :source-paths ["src"]
             ; :compiler {
             ;   :output-to "out-adv/cljstemplate.min.js"
             ;   :output-dir "out-adv"
             ;   :optimizations :advanced
             ;   :pretty-print false}}
             ]})
