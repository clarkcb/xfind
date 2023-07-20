(defproject cljfind "0.1.0-SNAPSHOT"
  :description "cljfind - the clojure version of the xfind command-line-based find utility"
  :url "https://github.com/clarkcb/xfind"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                  [org.clojure/clojure "1.11.1"]
                  [org.clojure/data.json "2.4.0"]
                  [clojure.java-time "1.2.0"]
                ]
  :main ^:skip-aot cljfind.cljfind
  :target-path "target/%s"
  :plugins [[jonase/eastwood "1.4.0"]]
  :profiles {:uberjar {:aot :all}})
