(defproject cljfind "0.1.0-SNAPSHOT"
  :description "cljfind - the clojure version of the xfind command-line-based find utility"
  :url "https://github.com/clarkcb/xfind"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                  [org.clojure/clojure "1.12.4"]
                  [org.clojure/data.json "2.5.2"]
                  [org.clojure/java.jdbc "0.7.12"]
                  [clojure.java-time "1.4.3"]
                  ;; TODO: switch to next.jdbc
                  ;;[com.github.seancorfield/next.jdbc "1.3.939"]
                  [org.xerial/sqlite-jdbc "3.46.0.1"]
                  [org.slf4j/slf4j-nop "1.7.36"]
                ]
;  :main ^:skip-aot cljfind.cljfind
  :main cljfind.cljfind
  :aot [cljfind.cljfind]
  :target-path "target/%s"
  :plugins [[jonase/eastwood "1.4.3"]]
  :profiles {:uberjar {:aot :all}})
