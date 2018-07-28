(defproject simian-flu "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [halgari/fn-fx "0.4.0"]
                 [garden "1.3.5"]]
  :source-paths ["src/clj"]
  :plugins [[lein-garden "0.3.0"]]
  ;; Plugins
  :garden {:builds [{:id "ui"
                     :source-paths ["src/garden"]
                     :stylesheet simian-flu.core/ui
                     :compiler {:output-to "resources/ui.css"
                                :pretty-print? false}}]})
