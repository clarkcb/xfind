(ns cljfind.config
  #^{:author "Cary Clark",
     :doc "Configuration values"}
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.string :only (join)]))

(def ^:const ^String XFINDPATH
  (or
    (System/getenv "XFIND_PATH")
    (clojure.string/join java.io.File/separator [(System/getenv "HOME") "src" "xfind"])))

(def ^:const ^String SHAREDPATH
  (clojure.string/join java.io.File/separator [XFINDPATH "shared"]))

(def ^:const ^String FILETYPESPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "filetypes.json"]))

(def ^:const ^String FINDOPTIONSPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "findoptions.json"]))
