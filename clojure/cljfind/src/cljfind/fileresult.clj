;;; ############################################################################
;;;
;;; fileresult.clj
;;;
;;; Encapsulates a file result
;;;
;;; ############################################################################

(ns cljfind.fileresult
  #^{:author "Cary Clark",
     :doc "Encapsulates a file to be found"}
  (:use [clojure.string :as str :only (join trim trim-newline)]
        )
)

; record to hold a file result (file is a File object)
(defrecord FileResult [containers, ^java.io.File file, filetype])

(defn new-file-result
  ([^java.io.File file, filetype]
   (->FileResult [] file filetype))
  ([containers, ^java.io.File file, filetype]
   (->FileResult containers file filetype)))

(defn file-result-path [^FileResult sf]
  (str (if (empty? (:containers sf)) "" (str (str/join "!" (:containers sf)) "!"))
       (.getPath (:file sf))))
