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
  (:require [cljfind.findsettings])
  (:import (cljfind.findsettings FindSettings))
  (:use [clojure.string :as string :only (join lower-case)]
        [cljfind.filetypes :only (to-name)]
        [cljfind.fileutil :only (get-name)]
        ))

; record to hold a file result (file is a File object)
(defrecord FileResult [containers, ^java.io.File file, file-type,
                       file-size last-mod])

(defn new-file-result
  ([^java.io.File file, file-type]
   (->FileResult [] file file-type 0 nil))
  ([^java.io.File file, file-type, file-size last-mod]
   (->FileResult [] file file-type file-size last-mod))
  ([containers, ^java.io.File file, file-type file-size last-mod]
   (->FileResult containers file file-type file-size last-mod)))

(defn file-result-path ^String [^FileResult fr]
  (str (if (empty? (:containers fr)) "" (str (string/join "!" (:containers fr)) "!"))
       (.getPath (:file fr))))

(defn cmp-strings [^String str1 ^String str2 ^FindSettings settings]
  (let [s1 (if (nil? str1) "" str1)
        s2 (if (nil? str2) "" str2)]
    (if (:sort-case-insensitive settings)
      (compare (lower-case s1) (lower-case s2))
      (compare s1 s2))))

(defn get-comp-by-path
  "get a filepath comparator"
  [^FindSettings settings]
  (fn [^FileResult fr1 ^FileResult fr2]
    (let [pathcmp (cmp-strings (.getParent (:file fr1)) (.getParent (:file fr2)) settings)]
      (if (= 0 pathcmp)
        (cmp-strings (get-name (:file fr1)) (get-name (:file fr2)) settings)
        pathcmp))))

(defn get-comp-by-name
  "get a filename comparator"
  [^FindSettings settings]
  (fn [^FileResult fr1 ^FileResult fr2]
    (let [namecmp (cmp-strings (get-name (:file fr1)) (get-name (:file fr2)) settings)]
      (if (= 0 namecmp)
        (cmp-strings (.getParent (:file fr1)) (.getParent (:file fr2)) settings)
        namecmp))))

(defn get-comp-by-size
  "get a filesize comparator"
  [^FindSettings settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [sizecmp (compare (:file-size fr1) (:file-size fr2))]
        (if (= 0 sizecmp)
          (comp-by-path fr1 fr2)
          sizecmp)))))

(defn get-comp-by-type
  "get a file-type comparator"
  [^FindSettings settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [typecmp (compare (to-name (:file-type fr1)) (to-name (:file-type fr2)))]
        (if (= 0 typecmp)
          (comp-by-path fr1 fr2)
          typecmp)))))

(defn get-comp-by-lastmod
  "get a lastmod comparator"
  [^FindSettings settings]
  (let [comp-by-path (get-comp-by-path settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [res (compare (.toMillis (:last-mod fr1)) (.toMillis (:last-mod fr2)))]
        (if (= 0 res)
          (comp-by-path fr1 fr2)
          res)))))

(defn get-comparator
  "get a comparator for sorting"
  [^FindSettings settings]
  (let [sort-by (:sort-by settings)]
    (cond
      (= sort-by :filename)
      (get-comp-by-name settings)
      (= sort-by :filesize)
      (get-comp-by-size settings)
      (= sort-by :filetype)
      (get-comp-by-type settings)
      (= sort-by :lastmod)
      (get-comp-by-lastmod settings)
      :else
      (get-comp-by-path settings))))

(defn sort-results
  "sorts file results according to settings"
  [results ^FindSettings settings]
  (let [sort-fn (get-comparator settings)
        sorted-results (sort sort-fn results)]
    (if (:sort-descending settings)
      (reverse sorted-results)
      sorted-results)))
