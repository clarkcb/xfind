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
        [cljfind.filetypes :only (to-name)]))

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

(defn comp-by-path
  "compares file results by filepath"
  [fr1 fr2]
  (if (= 0 (compare (.getPath (:file fr1)) (.getPath (:file fr2))))
    (compare (.getName (:file fr1)) (.getName (:file fr2)))
    (compare (.getPath (:file fr1)) (.getPath (:file fr2)))))

(defn comp-by-name
  "compares file results by filename"
  [fr1 fr2]
  (if (= 0 (compare (.getName (:file fr1)) (.getName (:file fr2))))
    (compare (.getPath (:file fr1)) (.getPath (:file fr2)))
    (compare (.getName (:file fr1)) (.getName (:file fr2)))))

(defn comp-by-type
  "compares file results by filetype"
  [fr1 fr2]
  (if (= 0 (compare (to-name (:filetype fr1)) (to-name (:filetype fr2))))
    (comp-by-path fr1 fr2)
    (compare (to-name (:filetype fr1)) (to-name (:filetype fr2)))))
