(ns cljfind.findconfig
  #^{:author "Cary Clark",
     :doc "FindConfig record"}
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.string :only (join)]))

(def ^:const ^String DEFAULT-XFIND-CONFIG-DIR
  (clojure.string/join java.io.File/separator [(System/getenv "HOME") ".config" "xfind"]))

(defn get-xfind-config-dir ^String []
  (or
   (System/getenv "XFIND_CONFIG_DIR")
   DEFAULT-XFIND-CONFIG-DIR))

(def ^:const ^String DEFAULT-XFIND-PATH
  (clojure.string/join java.io.File/separator [(System/getenv "HOME") "src" "xfind"]))

(defn get-xfind-path ^String []
  (or
   (System/getenv "XFIND_PATH")
   DEFAULT-XFIND-PATH))

(defn get-shared-path ^String []
  (clojure.string/join java.io.File/separator [(get-xfind-path) "shared"]))

(def ^:const ^String FILE-TYPES-NAME
  "filetypes.json")

(defn get-file-types-path ^String []
  (clojure.string/join java.io.File/separator [(get-shared-path) FILE-TYPES-NAME]))

(def ^:const ^String FIND-OPTIONS-NAME
  "findoptions.json")

(defn get-find-options-path ^String []
  (clojure.string/join java.io.File/separator [(get-shared-path) FIND-OPTIONS-NAME]))

(defn get-default-find-settings-path []
  (let [xfind-config-dir (get-xfind-config-dir)]
    (clojure.string/join java.io.File/separator [xfind-config-dir "settings.json"])))

(defrecord FindConfig [file-types-path find-options-path default-find-settings-path])

(defn default-find-config []
  (->FindConfig (get-file-types-path) (get-find-options-path) (get-default-find-settings-path)))
