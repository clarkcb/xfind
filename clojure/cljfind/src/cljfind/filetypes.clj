;;; ############################################################################
;;;
;;; filetypes.clj
;;;
;;; Utility functions for getting file extension and determining file type
;;;
;;; ############################################################################

(ns cljfind.filetypes
  #^{:author "Cary Clark",
     :doc "Module to provide file-related utility functions"}
  (:import (java.io File))
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.set :only (union)]
        [clojure.string :only (split lower-case)]
        [cljfind.fileutil :only (expand-path get-ext)]))

(def ARCHIVE "archive")
(def BINARY "binary")
(def CODE "code")
(def TEXT "text")
(def XML "xml")

(defn get-filetypemap-from-json []
  (let [contents (slurp (io/resource "filetypes.json"))
        filetypes-objs (:filetypes (json/read-str contents :key-fn keyword))
        typenames (map :type filetypes-objs)
        extension-sets (map #(set %) (map :extensions filetypes-objs))
        filetypemap (zipmap typenames extension-sets)
        textmap (hash-map "all-text"
                  (union (get filetypemap TEXT)
                         (get filetypemap CODE)
                         (get filetypemap XML)))
        fullmap (merge filetypemap textmap)
       ]
    fullmap))

(def FILETYPEMAP (get-filetypemap-from-json))

(defn archive-ext? [^String ext]
  (contains? (get FILETYPEMAP ARCHIVE) ext))

(defn archive-file? [f]
  (archive-ext? (get-ext f)))

(defn binary-ext? [^String ext]
  (contains? (get FILETYPEMAP BINARY) ext))

(defn binary-file? [f]
  (contains? (get FILETYPEMAP BINARY) (get-ext f)))

(defn code-ext? [^String ext]
  (contains? (get FILETYPEMAP CODE) ext))

(defn code-file? [f]
  (contains? (get FILETYPEMAP CODE) (get-ext f)))

(defn text-ext? [^String ext]
  (contains? (get FILETYPEMAP TEXT) ext))

(defn text-file? [f]
  (contains? (get FILETYPEMAP TEXT) (get-ext f)))

(defn xml-ext? [^String ext]
  (contains? (get FILETYPEMAP XML) ext))

(defn xml-file? [f]
  (contains? (get FILETYPEMAP XML) (get-ext f)))

(defn get-filetype [f]
  (let [ext (get-ext f)]
    (cond
      (binary-ext? ext) :binary
      (code-ext? ext) :code
      (xml-ext? ext) :xml
      (text-ext? ext) :text
      (archive-ext? ext) :archive
      :else :unknown)))

(defn unknown-file? [f]
  (= :unknown (get-filetype f)))

(defn from-name [^String name]
  (let [lname (lower-case name)]
    (cond
      (= TEXT lname) :text
      (= BINARY lname) :binary
      (= CODE lname) :code
      (= XML lname) :xml
      (= ARCHIVE lname) :archive
      :else :unknown)))
