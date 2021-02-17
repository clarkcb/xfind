(ns cljfind.config
  #^{:author "Cary Clark",
     :doc "Configuration values"}
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.string :only (join)]))

(def XFINDPATH
    (let [configjson (slurp (io/resource "config.json"))
          config (json/read-str configjson :key-fn keyword)]
      (config :xfindpath)))

(def SHAREDPATH
  (clojure.string/join java.io.File/separator [XFINDPATH "shared"]))

(def FILETYPESPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "filetypes.xml"]))

(def FINDOPTIONSPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "findoptions.xml"]))
