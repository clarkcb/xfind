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
(defrecord FileResult [containers, ^java.io.File file, filetype,
                       ^java.nio.file.attribute.BasicFileAttributes stat])

(defn new-file-result
  ([^java.io.File file, filetype stat]
   (->FileResult [] file filetype stat))
  ([containers, ^java.io.File file, filetype stat]
   (->FileResult containers file filetype stat)))

(defn file-result-path [^FileResult sf]
  (str (if (empty? (:containers sf)) "" (str (str/join "!" (:containers sf)) "!"))
       (.getPath (:file sf))))

(defn comp-strings
  "compares two strings case-sensitively or case-insensitively"
  [str1 str2 case-insensitive]
  (let [s1 (if (nil? str1) "" (if case-insensitive (.toLowerCase str1) str1))
        s2 (if (nil? str2) "" (if case-insensitive (.toLowerCase str2) str2))]
    (compare s1 s2)))

(defn get-comp-by-path
  "get a filepath comparator"
  [settings]
  (fn [fr1 fr2]
    (let [p1 (.getParent (:file fr1))
          p2 (.getParent (:file fr2))
          res (comp-strings p1 p2 (:case-insensitive settings))]
      (if (= 0 res)
        (comp-strings (.getName (:file fr1)) (.getName (:file fr2)) (:case-insensitive settings))
        res))))

(defn get-comp-by-name
  "get a filename comparator"
  [settings]
  (fn [fr1 fr2]
    (let [n1 (.getName (:file fr1))
          n2 (.getName (:file fr2))
          res (comp-strings n1 n2 (:case-insensitive settings))]
      (if (= 0 res)
        (comp-strings (.getParent (:file fr1)) (.getParent (:file fr2)) (:case-insensitive settings))
        res))))

(defn get-comp-by-size
  "get a filesize comparator"
  [settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [fr1 fr2]
      (let [res (compare (.size (:stat fr1)) (.size (:stat fr2)))]
        (if (= 0 res)
          (comp-by-path fr1 fr2)
          res)))))

(defn get-comp-by-type
  "get a filetype comparator"
  [settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [fr1 fr2]
      (let [res (compare (to-name (:filetype fr1)) (to-name (:filetype fr2)))]
        (if (= 0 res)
          (comp-by-path fr1 fr2)
          res)))))

(defn get-comp-by-lastmod
  "get a lastmod comparator"
  [settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [fr1 fr2]
      (let [res (compare (.toMillis (.lastModifiedTime (:stat fr1))) (.toMillis (.lastModifiedTime (:stat fr2))))]
        (if (= 0 res)
          (comp-by-path fr1 fr2)
          res)))))

(defn sort-results
  "sorts file results according to settings"
  [results settings]
  (let [sorted-results (cond
                         (= (:sort-by settings) :filename)
                         (sort (get-comp-by-name settings) results)
                         (= (:sort-by settings) :filesize)
                         (sort (get-comp-by-size settings) results)
                         (= (:sort-by settings) :filetype)
                         (sort (get-comp-by-type settings) results)
                         (= (:sort-by settings) :lastmod)
                         (sort (get-comp-by-lastmod settings) results)
                         :else
                         (sort (get-comp-by-path settings) results))]
    (if (:sort-descending settings)
      (reverse sorted-results)
      sorted-results))
)