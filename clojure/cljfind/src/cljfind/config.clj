(ns cljfind.config
  #^{:author "Cary Clark",
     :doc "Configuration values"}
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.string :only (join)]))

(def XFINDPATH
  (or
    (System/getenv "XFIND_PATH")
    (clojure.string/join java.io.File/separator [(System/getenv "HOME") "src" "xfind"])))

(def SHAREDPATH
  (clojure.string/join java.io.File/separator [XFINDPATH "shared"]))

(def FILETYPESPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "filetypes.json"]))

(def FINDOPTIONSPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "findoptions.json"]))
