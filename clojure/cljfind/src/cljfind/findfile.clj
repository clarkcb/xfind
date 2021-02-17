;;; ############################################################################
;;;
;;; findfile.clj
;;;
;;; Encapsulates a file to be found
;;;
;;; ############################################################################

(ns cljfind.findfile
  #^{:author "Cary Clark",
     :doc "Encapsulates a file to be found"}
  (:use [clojure.string :as str :only (join trim trim-newline)]))

; record to hold a find-file (file is a File object)
(defrecord FindFile [containers file filetype])

(defn new-find-file
  ([^java.io.File file filetype]
   (new-find-file [] file filetype))
  ([containers file filetype]
   (->FindFile containers file filetype)))

(defn find-file-path [^FindFile sf]
  (str (if (empty? (:containers sf)) "" (str (str/join "!" (:containers sf)) "!"))
       (.getPath (:file sf))))
