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
  (:import (cljfind.findsettings FindSettings)
           (java.nio.file Paths Path Files))
  (:use [clojure.string :as string :only (join lower-case)]
        [cljfind.color :only (GREEN RESET)]
        [cljfind.filetypes :only (to-name)]
        [cljfind.fileutil :only (get-path-name get-parent-name)]
        ))

; record to hold a file result (path is a Path object)
(defrecord FileResult [containers, ^Path path,
                       file-type, file-size last-mod])

(defn new-file-result
  ([^Path path, file-type]
   (->FileResult [] path file-type 0 nil))
  ([^Path path, file-type, file-size last-mod]
   (->FileResult [] path file-type file-size last-mod))
  ([containers, ^Path path, file-type file-size last-mod]
   (->FileResult containers path file-type file-size last-mod)))

(defn file-result-path ^String [^FileResult fr]
  (str (if (empty? (:containers fr)) "" (str (string/join "!" (:containers fr)) "!"))
       (.toString (:path fr))))

(defn get-cmp-elems [^FindSettings settings]
  "get an elem comparator"
  [^FindSettings settings]
  (let [rev-factor (if (:sort-descending settings) -1 1)]
    (fn [elem1 elem2]
      (* rev-factor (compare elem1 elem2)))))

(defn get-cmp-strings [^FindSettings settings]
  "get a filepath comparator"
  [^FindSettings settings]
  (let [to-case (if (:sort-case-insensitive settings) lower-case identity)
        cmp-elems (get-cmp-elems settings)]
    (fn [^String str1 ^String str2]
      (let [s1 (if (nil? str1) "" str1)
            s2 (if (nil? str2) "" str2)]
        (cmp-elems (to-case s1) (to-case s2))))))

(defn get-comp-by-path
  "get a filepath comparator"
  [^FindSettings settings]
  (let [cmp-strings (get-cmp-strings settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [pathcmp (cmp-strings (get-parent-name (:path fr1)) (get-parent-name (:path fr2)))]
        (if (= 0 pathcmp)
          (cmp-strings (get-path-name (:path fr1)) (get-path-name (:path fr2)))
          pathcmp)))))

(defn get-comp-by-name
  "get a filename comparator"
  [^FindSettings settings]
  (let [cmp-strings (get-cmp-strings settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [namecmp (cmp-strings (get-path-name (:path fr1)) (get-path-name (:path fr2)))]
        (if (= 0 namecmp)
          (cmp-strings (get-parent-name (:path fr1)) (get-parent-name (:path fr2)))
          namecmp)))))

(defn get-comp-by-size
  "get a filesize comparator"
  [^FindSettings settings]
  (let [comp-by-path (get-comp-by-path settings)
        cmp-elems (get-cmp-elems settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [sizecmp (cmp-elems (:file-size fr1) (:file-size fr2))]
        (if (= 0 sizecmp)
          (comp-by-path fr1 fr2)
          sizecmp)))))

(defn get-comp-by-type
  "get a file-type comparator"
  [^FindSettings settings]
  (let [comp-by-path (get-comp-by-path settings)
        cmp-elems (get-cmp-elems settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [typecmp (cmp-elems (to-name (:file-type fr1)) (to-name (:file-type fr2)))]
        (if (= 0 typecmp)
          (comp-by-path fr1 fr2)
          typecmp)))))

(defn get-comp-by-lastmod
  "get a lastmod comparator"
  [^FindSettings settings]
  (let [comp-by-path (get-comp-by-path settings)
        cmp-elems (get-cmp-elems settings)]
    (fn [^FileResult fr1 ^FileResult fr2]
      (let [res (cmp-elems (.toMillis (:last-mod fr1)) (.toMillis (:last-mod fr2)))]
        (if (= 0 res)
          (comp-by-path fr1 fr2)
          res)))))

(defn get-file-result-comparator
  "get a comparator for sorting file results"
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

(defn sort-file-results
  "sorts file results according to settings"
  [results ^FindSettings settings]
  (let [file-result-comparator (get-file-result-comparator settings)]
    (sort file-result-comparator results)))

(defn colorize-string ^String [^String s start-index end-index]
  (let [prefix (if (> start-index 0) (subs s 0 start-index) "")
        colorized (str GREEN (subs s start-index end-index) RESET)
        suffix (if (< end-index (.length s)) (subs s end-index) "")]
    (str prefix colorized suffix)))

(defn colorize-dir-path ^String [^Path p ^FindSettings settings]
  (let [dir-path (if (nil? p) "." (.toString p))
        matching-dir-patterns (take 1 (filter #(re-find % dir-path) (:in-dir-patterns settings)))
        dir-matcher (if (empty? matching-dir-patterns) nil (re-matcher (first matching-dir-patterns) dir-path))
        color-dir-path (if (nil? dir-matcher) dir-path (do (.find dir-matcher 0) (colorize-string dir-path (.start dir-matcher) (.end dir-matcher))))]
    color-dir-path))

(defn get-dir-path-formatter [^FindSettings settings]
(if
  (and
    (:colorize settings)
    (not (empty? (:in-dir-patterns settings))))
  (fn [^Path p]
    (colorize-dir-path p settings))
  (fn [^Path p]
    (.toString p))))

(defn colorize-file-name ^String [^String filename ^FindSettings settings]
  (let [matching-file-patterns (take 1 (filter #(re-find % filename) (:in-file-patterns settings)))
        file-matcher (if (empty? matching-file-patterns) nil (re-matcher (first matching-file-patterns) filename))
        color-filename (if (nil? file-matcher) filename (do (.find file-matcher 0) (colorize-string filename (.start file-matcher) (.end file-matcher))))]
    (if (empty? (:in-extensions settings))
      color-filename
      (let [idx (.lastIndexOf color-filename ".")
            filename-len (.length color-filename)]
        (if (and (> idx 0) (< idx (- filename-len 1)))
          (colorize-string color-filename (+ idx 1) filename-len)
          color-filename)))))

(defn get-file-name-formatter [^FindSettings settings]
  (if
    (and
     (:colorize settings)
     (or
      (not (empty? (:in-etensions settings)))
      (not (empty? (:in-file-patterns settings)))))
    (fn [^String filename]
      (colorize-file-name filename settings))
    (fn [^String filename]
      filename)))

(defn get-file-path-formatter [^FindSettings settings]
  (let [format-dir-path (get-dir-path-formatter settings)
        format-file-name (get-file-name-formatter settings)]
    (fn [^Path p]
      (let [parent (if (nil? (.getParent p)) "." (format-dir-path (.getParent p)))
            filename (format-file-name (.toString (.getFileName p)))]
        (str parent java.io.File/separator filename)))))

(defn get-file-result-formatter [^FindSettings settings]
  (let [format-file-path (get-file-path-formatter settings)]
    (fn [^FileResult r]
      (let [path (format-file-path (:path r))
            containers (if (empty? (:containers r)) "" (str (string/join "!" (:containers r)) "!"))]
        (str containers path)))))
