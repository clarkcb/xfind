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
  (:require [clojure.java.jdbc :as jdbc])
  (:require [clojure.data.json :as json])
  (:use [clojure.set :only (union)]
        [clojure.string :only (lower-case)]
        [cljfind.config :only (XFINDDB)]
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

(def ^:const FILE-TYPES
  [:unknown :archive :audio :binary :code :font :image :text :video :xml])

(def ^:const db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     XFINDDB})

(def ^:const ^String FILE-NAME-QUERY "SELECT file_type_id FROM file_name WHERE name = ?")
(def ^:const ^String FILE-EXT-QUERY "SELECT file_type_id FROM file_extension WHERE extension = ?")

(defn get-file-type-for-file-name [^String file-name]
  (let [file-type-id (jdbc/query db [FILE-NAME-QUERY file-name] {:row-fn :file_type_id})]
    (if (or (nil? file-type-id) (empty? file-type-id))
      :unknown
      (nth FILE-TYPES (- (first file-type-id) 1)))))

(defn get-file-type-for-extension [^String file-ext]
  (let [file-type-id (jdbc/query db [FILE-EXT-QUERY file-ext] {:row-fn :file_type_id})]
    (if (or (nil? file-type-id) (empty? file-type-id))
      :unknown
      (nth FILE-TYPES (- (first file-type-id) 1)))))

(defn get-file-type [f]
  (let [file-name (get-name f)
        file-type-for-file-name (get-file-type-for-file-name file-name)]
    (if (not= :unknown file-type-for-file-name)
      file-type-for-file-name
      (get-file-type-for-extension (get-ext f)))))

(defn archive-file? [f]
  (= (get-file-type f) :archive))

(defn audio-file? [f]
  (= (get-file-type f) :audio))

(defn binary-file? [f]
  (= (get-file-type f) :binary))

(defn code-file? [f]
  (= (get-file-type f) :code))

(defn font-file? [f]
  (= (get-file-type f) :font))

(defn image-file? [f]
  (= (get-file-type f) :image))

(defn text-file? [f]
  (let [file-type (get-file-type f)]
    (or
     (= file-type :text)
     (= file-type :code)
     (= file-type :xml))))

(defn video-file? [f]
  (= (get-file-type f) :video))

(defn xml-file? [f]
  (= (get-file-type f) :xml))

(defn unknown-file? [f]
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

(defn to-name ^String [ft]
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
