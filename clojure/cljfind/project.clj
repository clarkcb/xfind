(defproject cljfind "0.1.0-SNAPSHOT"
  :description "cljfind - the clojure version of the xfind command-line-based find utility"
  :url "https://github.com/clarkcb/xfind"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                  [org.clojure/clojure "1.11.2"]
                  [org.clojure/data.json "2.5.0"]
                  [clojure.java-time "1.4.2"]
                  [org.opf_labs/libmagic-jna-wrapper "0.0.2-SNAPSHOT"]
                ]
;  :main ^:skip-aot cljfind.cljfind
  :main cljfind.cljfind
  :aot [cljfind.cljfind]
  :target-path "target/%s"
  :plugins [[jonase/eastwood "1.4.2"]]
  :profiles {:uberjar {:aot :all}})
