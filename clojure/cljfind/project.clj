(defproject cljfind "0.1.0-SNAPSHOT"
  :description "cljfind - the clojure version of the xfind command-line-based find utility"
  :url "https://github.com/clarkcb/xfind"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                  [org.clojure/clojure "1.10.1"]
                  [org.clojure/data.json "1.0.0"]
                ]
  :main ^:skip-aot cljfind.cljfind
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
