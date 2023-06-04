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

(defn get-comp-by-path
  "get a filepath comparator"
  [settings]
  (let [case-insensitive (:sort-case-insensitive settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [_p1 (.getParent (:file fr1))
            p1 (if (nil? _p1) "" (if case-insensitive (string/lower-case _p1) _p1))
            _p2 (.getParent (:file fr2))
            p2 (if (nil? _p2) "" (if case-insensitive (string/lower-case _p2) _p2))
            res (compare p1 p2)]
        (if (= 0 res)
          (let [_n1 (get-name (:file fr1))
                n1 (if case-insensitive (string/lower-case _n1) _n1)
                _n2 (get-name (:file fr2))
                n2 (if case-insensitive (string/lower-case _n2) _n2)]
            (compare n1 n2))
          res)))))

(defn get-comp-by-name
  "get a filename comparator"
  [settings]
  (let [case-insensitive (:sort-case-insensitive settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [_n1 (get-name (:file fr1))
            n1 (if case-insensitive (string/lower-case _n1) _n1)
            _n2 (get-name (:file fr2))
            n2 (if case-insensitive (string/lower-case _n2) _n2)
            res (compare n1 n2)]
        (if (= 0 res)
          (let [_p1 (.getParent (:file fr1))
                p1 (if (nil? _p1) "" (if case-insensitive (string/lower-case _p1) _p1))
                _p2 (.getParent (:file fr2))
                p2 (if (nil? _p2) "" (if case-insensitive (string/lower-case _p2) _p2))]
            (compare p1 p2))
          res)))))

(defn get-comp-by-size
  "get a filesize comparator"
  [settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [res (compare (.size (:stat fr1)) (.size (:stat fr2)))]
        (if (= 0 res)
          (comp-by-path fr1 fr2)
          res)))))

(defn get-comp-by-type
  "get a file-type comparator"
  [settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [res (compare (to-name (:file-type fr1)) (to-name (:file-type fr2)))]
        (if (= 0 res)
          (comp-by-path fr1 fr2)
          res)))))

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
