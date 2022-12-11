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
        [cljfind.fileutil :only (expand-path get-ext get-name)]))

(def ARCHIVE "archive")
(def BINARY "binary")
(def CODE "code")
(def TEXT "text")
(def XML "xml")

(defn get-filetype-maps-from-json []
  (let [contents (slurp (io/resource "filetypes.json"))
        filetypes-objs (:filetypes (json/read-str contents :key-fn keyword))
        typenames (map :type filetypes-objs)
        extension-sets (map #(set %) (map :extensions filetypes-objs))
        filetype-ext-map (zipmap typenames extension-sets)
        text-ext-map (hash-map "all-text"
                       (union (get filetype-ext-map TEXT)
                              (get filetype-ext-map CODE)
                              (get filetype-ext-map XML)))
        full-ext-map (merge filetype-ext-map text-ext-map)
        name-sets (map #(set %) (map :names filetypes-objs))
        filetype-name-map (zipmap typenames name-sets)
        text-name-map (hash-map "all-text"
                        (union (get filetype-name-map TEXT)
                               (get filetype-name-map CODE)
                               (get filetype-name-map XML)))
        full-name-map (merge filetype-name-map text-name-map)
       ]
    [full-ext-map full-name-map]))

(def FILETYPEMAPS (get-filetype-maps-from-json))
(def FILETYPEEXTMAP (first FILETYPEMAPS))
(def FILETYPENAMEMAP (last FILETYPEMAPS))

(defn archive-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP ARCHIVE) ext))

(defn archive-file? [f]
  (or
   (contains? (get FILETYPENAMEMAP ARCHIVE) (get-name f))
   (contains? (get FILETYPEEXTMAP ARCHIVE) (get-ext f))))

(defn binary-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP BINARY) ext))

(defn binary-file? [f]
  (or
   (contains? (get FILETYPENAMEMAP BINARY) (get-name f))
   (contains? (get FILETYPEEXTMAP BINARY) (get-ext f))))

(defn code-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP CODE) ext))

(defn code-file? [f]
  (or
   (contains? (get FILETYPENAMEMAP CODE) (get-name f))
   (contains? (get FILETYPEEXTMAP CODE) (get-ext f))))

(defn text-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP TEXT) ext))

(defn text-file? [f]
  (or
   (contains? (get FILETYPENAMEMAP TEXT) (get-name f))
   (contains? (get FILETYPEEXTMAP TEXT) (get-ext f))))

(defn xml-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP XML) ext))

(defn xml-file? [f]
  (or
   (contains? (get FILETYPENAMEMAP XML) (get-name f))
   (contains? (get FILETYPEEXTMAP XML) (get-ext f))))

(defn get-filetype [f]
  (cond
    (binary-file? f) :binary
    (code-file? f) :code
    (xml-file? f) :xml
    (text-file? f) :text
    (archive-file? f) :archive
    :else :unknown))

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

(defn to-name [ft]
  (cond
    (= :text ft) TEXT
    (= :binary ft) BINARY
    (= :code ft) CODE
    (= :xml ft) XML
    (= :archive ft) ARCHIVE
    :else "unknown"))
