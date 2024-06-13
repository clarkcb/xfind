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
        [clojure.string :only (lower-case)]
        [cljfind.fileutil :only (get-ext get-name)]))

(def ^:const ^String ARCHIVE "archive")
(def ^:const ^String AUDIO "audio")
(def ^:const ^String BINARY "binary")
(def ^:const ^String CODE "code")
(def ^:const ^String FONT "font")
(def ^:const ^String IMAGE "image")
(def ^:const ^String TEXT "text")
(def ^:const ^String VIDEO "video")
(def ^:const ^String XML "xml")

(defn get-file-type-maps-from-json []
  (let [contents (slurp (io/resource "filetypes.json"))
        file-types-objs (:filetypes (json/read-str contents :key-fn keyword))
        typenames (map :type file-types-objs)
        extension-sets (map #(set %) (map :extensions file-types-objs))
        file-type-ext-map (zipmap typenames extension-sets)
        text-ext-map (hash-map "all-text"
                       (union (get file-type-ext-map TEXT)
                              (get file-type-ext-map CODE)
                              (get file-type-ext-map XML)))
        full-ext-map (merge file-type-ext-map text-ext-map)
        name-sets (map #(set %) (map :names file-types-objs))
        file-type-name-map (zipmap typenames name-sets)
        text-name-map (hash-map "all-text"
                        (union (get file-type-name-map TEXT)
                               (get file-type-name-map CODE)
                               (get file-type-name-map XML)))
        full-name-map (merge file-type-name-map text-name-map)
       ]
    [full-ext-map full-name-map]))

(def FILETYPEMAPS (get-file-type-maps-from-json))
(def FILETYPEEXTMAP (nth FILETYPEMAPS 0))
(def FILETYPENAMEMAP (nth FILETYPEMAPS 1))

(defn archive-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP ARCHIVE) ext))

(defn archive-file? [^File f]
  (or
   (contains? (get FILETYPENAMEMAP ARCHIVE) (get-name f))
   (contains? (get FILETYPEEXTMAP ARCHIVE) (get-ext f))))

(defn audio-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP AUDIO) ext))

(defn audio-file? [^File f]
  (or
   (contains? (get FILETYPENAMEMAP AUDIO) (get-name f))
   (contains? (get FILETYPEEXTMAP AUDIO) (get-ext f))))

(defn binary-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP BINARY) ext))

(defn binary-file? [^File f]
  (or
   (contains? (get FILETYPENAMEMAP BINARY) (get-name f))
   (contains? (get FILETYPEEXTMAP BINARY) (get-ext f))))

(defn code-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP CODE) ext))

(defn code-file? [^File f]
  (or
   (contains? (get FILETYPENAMEMAP CODE) (get-name f))
   (contains? (get FILETYPEEXTMAP CODE) (get-ext f))))

(defn font-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP FONT) ext))

(defn font-file? [^File f]
  (or
   (contains? (get FILETYPENAMEMAP FONT) (get-name f))
   (contains? (get FILETYPEEXTMAP FONT) (get-ext f))))

(defn image-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP IMAGE) ext))

(defn image-file? [^File f]
  (or
   (contains? (get FILETYPENAMEMAP IMAGE) (get-name f))
   (contains? (get FILETYPEEXTMAP IMAGE) (get-ext f))))

(defn text-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP TEXT) ext))

(defn text-file? [^File f]
  (or
   (contains? (get FILETYPENAMEMAP TEXT) (get-name f))
   (contains? (get FILETYPEEXTMAP TEXT) (get-ext f))))

(defn video-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP VIDEO) ext))

(defn video-file? [^File f]
  (or
   (contains? (get FILETYPENAMEMAP VIDEO) (get-name f))
   (contains? (get FILETYPEEXTMAP VIDEO) (get-ext f))))

(defn xml-ext? [^String ext]
  (contains? (get FILETYPEEXTMAP XML) ext))

(defn xml-file? [^File f]
  (or
   (contains? (get FILETYPENAMEMAP XML) (get-name f))
   (contains? (get FILETYPEEXTMAP XML) (get-ext f))))

(defn get-file-type [^File f]
  (cond
    ;; most specific first
    (code-file? f) :code
    (archive-file? f) :archive
    (audio-file? f) :audio
    (font-file? f) :font
    (image-file? f) :image
    (video-file? f) :video
    ;; most general last
    (xml-file? f) :xml
    (text-file? f) :text
    (binary-file? f) :binary
    :else :unknown))

(defn unknown-file? [^File f]
  (= :unknown (get-file-type f)))

(defn from-name [^String name]
  (let [lname (lower-case name)]
    (cond
      (= ARCHIVE lname) :archive
      (= AUDIO lname) :audio
      (= BINARY lname) :binary
      (= CODE lname) :code
      (= FONT lname) :font
      (= IMAGE lname) :image
      (= TEXT lname) :text
      (= VIDEO lname) :video
      (= XML lname) :xml
      :else :unknown)))

(defn to-name [ft]
  (cond
    (= :archive ft) ARCHIVE
    (= :audio ft) AUDIO
    (= :binary ft) BINARY
    (= :code ft) CODE
    (= :font ft) FONT
    (= :image ft) IMAGE
    (= :text ft) TEXT
    (= :video ft) VIDEO
    (= :xml ft) XML
    :else "unknown"))
