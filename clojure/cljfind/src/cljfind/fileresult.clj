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
  (:use [clojure.string :as string :only (join lower-case)]
        [cljfind.filetypes :only (to-name)]
        [cljfind.fileutil :only (get-name)]
        ))

; record to hold a file result (file is a File object)
(defrecord FileResult [containers, ^java.io.File file, file-type,
                       ^java.nio.file.attribute.BasicFileAttributes stat])

(defn new-file-result
  ([^java.io.File file, file-type]
   (->FileResult [] file file-type nil))
  ([^java.io.File file, file-type, stat]
   (->FileResult [] file file-type stat))
  ([containers, ^java.io.File file, file-type stat]
   (->FileResult containers file file-type stat)))

(defn file-result-path [^FileResult fr]
  (str (if (empty? (:containers fr)) "" (str (string/join "!" (:containers fr)) "!"))
       (.getPath (:file fr))))

(defn cmp-strings [str1 str2 settings]
  (let [s1 (if (nil? str1) "" str1)
        s2 (if (nil? str2) "" str2)
        case-insensitive (:sort-case-insensitive settings)]
    (if case-insensitive
      (compare (lower-case s1) (lower-case s2))
      (compare s1 s2))))

(defn get-comp-by-path
  "get a filepath comparator"
  [settings]
  (let [case-insensitive (:sort-case-insensitive settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [pathcmp (cmp-strings (.getParent (:file fr1)) (.getParent (:file fr2)) settings)]
        (if (= 0 pathcmp)
          (cmp-strings (get-name (:file fr1)) (get-name (:file fr2)) settings)
          pathcmp)))))

(defn get-comp-by-name
  "get a filename comparator"
  [settings]
  (let [case-insensitive (:sort-case-insensitive settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [namecmp (cmp-strings (get-name (:file fr1)) (get-name (:file fr2)) settings)]
        (if (= 0 namecmp)
          (cmp-strings (.getParent (:file fr1)) (.getParent (:file fr2)) settings)
          namecmp)))))

(defn get-comp-by-size
  "get a filesize comparator"
  [settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [sizecmp (compare (.size (:stat fr1)) (.size (:stat fr2)))]
        (if (= 0 sizecmp)
          (comp-by-path fr1 fr2)
          sizecmp)))))

(defn get-comp-by-type
  "get a file-type comparator"
  [settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [typecmp (compare (to-name (:file-type fr1)) (to-name (:file-type fr2)))]
        (if (= 0 typecmp)
          (comp-by-path fr1 fr2)
          typecmp)))))

(defn get-comp-by-lastmod
  "get a lastmod comparator"
  [settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [res (compare (.toMillis (.lastModifiedTime (:stat fr1))) (.toMillis (.lastModifiedTime (:stat fr2))))]
        (if (= 0 res)
          (comp-by-path fr1 fr2)
          res)))))

(defn sort-results
  "sorts file results according to settings"
  [results settings]
  (let [sort-fn (cond
                  (= (:sort-by settings) :filename)
                  (get-comp-by-name settings)
                  (= (:sort-by settings) :filesize)
                  (get-comp-by-size settings)
                  (= (:sort-by settings) :filetype)
                  (get-comp-by-type settings)
                  (= (:sort-by settings) :lastmod)
                  (get-comp-by-lastmod settings)
                  :else
                  (get-comp-by-path settings))
        sorted-results (sort sort-fn results)]
    (if (:sort-descending settings)
      (reverse sorted-results)
      sorted-results)))
