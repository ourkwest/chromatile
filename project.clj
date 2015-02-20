(defproject cljstemplate "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2719"]
                 [org.clojure/core.async "0.1.256.0-1bf8cf-alpha"]]

  :node-dependencies [[source-map-support "0.2.8"]]

  :plugins [[lein-cljsbuild "1.0.6-SNAPSHOT"]
            [lein-npm "0.4.0"]]

  :source-paths ["src" "target/classes"]

  :clean-targets ["release/scripts" "release/scripts-adv" "cljstemplate.js" "cljstemplate.min.js"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src"]
              :compiler {
                :output-to "release/scripts/cljstemplate.js"
                :output-dir "release/scripts"
                :optimizations :none
                :cache-analysis true
                :source-map true}}
             {:id "release"
             :source-paths ["src"]
             :compiler {
                :output-to "release/scripts-adv/cljstemplate.min.js"
                :output-dir "release/scripts-adv"
                :optimizations :advanced
                :pretty-print false}}
             ]})
