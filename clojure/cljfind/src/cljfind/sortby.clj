;;; ############################################################################
;;;
;;; sortby.clj
;;;
;;; Defines the sort-by options
;;;
;;; ############################################################################

(ns cljfind.sortby
  #^{:author "Cary Clark",
     :doc "Defines the sort-by options"}
  (:use [clojure.string :as str :only (lower-case)]))

;; sort-by keyword names
(def ^:const SORTBY_NAMES [:filepath :filename :filesize :filetype :lastmod])

;; sort-by string names
(def ^:const ^String FILEPATH "filepath")
(def ^:const ^String FILENAME "filename")
(def ^:const ^String NAME "name")
(def ^:const ^String FILESIZE "filesize")
(def ^:const ^String SIZE "size")
(def ^:const ^String FILETYPE "filetype")
(def ^:const ^String TYPE "type")
(def ^:const ^String LASTMOD "lastmod")

(defn get-sort-by-name [s]
  (cond
    (= :filename s) FILENAME
    (= :filesize s) FILESIZE
    (= :filetype s) FILETYPE
    (= :lastmod s) LASTMOD
    :else FILEPATH))

(defn sort-by-from-name [^String name]
  (let [lname (lower-case name)]
    (cond
      (or
       (= FILENAME lname)
       (= NAME lname)) :filename
      (or
       (= FILESIZE lname)
       (= SIZE lname)) :filesize
      (or
       (= FILETYPE lname)
       (= TYPE lname)) :filetype
      (= LASTMOD lname) :lastmod
      :else :filepath)))
