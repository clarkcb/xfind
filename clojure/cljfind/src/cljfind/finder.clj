;;; ############################################################################
;;;
;;; finder.clj
;;;
;;; The main functions to find files matching settings
;;;
;;; ############################################################################

(ns cljfind.finder
  #^{:author "Cary Clark",
     :doc "Recursive file find utility"}
  (:require [cljfind.fileresult]
            [cljfind.findsettings])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute BasicFileAttributes)
           (java.util.jar JarFile)
           (java.util.zip ZipFile)
           (cljfind.fileresult FileResult)
           (cljfind.findsettings FindSettings))
  (:use [clojure.java.io :only (file reader)]
        [cljfind.common :only (log-msg)]
        [cljfind.fileresult :only
         (new-file-result file-result-path sort-results)]
        [cljfind.filetypes :only (get-file-type)]
        [cljfind.fileutil :only
          (get-ext get-name hidden-dir? hidden-file?
            is-dot-dir? sep-count)]
        [cljfind.findsettings :only (need-last-mod need-size)])
  (:require [java-time.api :as jt]))

(defn matches-any-pattern? [^String s pp]
  (some #(re-find % s) pp))

(defn any-matches-any-pattern? [ss pp]
  (some #(not (= % nil)) (map #(matches-any-pattern? % pp) ss)))

(defn validate-settings [^FindSettings settings]
  (let [paths (:paths settings)
        tests [(fn [ss] (if (empty? paths) "Startpath not defined" nil))
               (fn [ss] (if (some #(not (.exists (file %))) paths) "Startpath not found" nil))
               (fn [ss]
                 (if
                   (and
                     (> (:max-depth ss) -1)
                     (> (:min-depth ss) (:max-depth ss))
                     ) "Invalid range for mindepth and maxdepth" nil))
               (fn [ss]
                 (if
                   (and
                     (not (nil? (:max-last-mod ss)))
                     (not (nil? (:min-last-mod ss)))
                     (.before (:max-last-mod ss) (:min-last-mod ss))
                     ) "Invalid range for minlastmod and maxlastmod" nil))
               (fn [ss]
                 (if
                   (and
                     (> (:max-size ss) 0)
                     (> (:min-size ss) (:max-size ss))
                     ) "Invalid range for minsize and maxsize" nil))
              ]
       ]
    (take 1 (filter #(not (nil? %)) (map #(% settings) tests)))))

(defn is-matching-dir? [^File d ^FindSettings settings]
  (or
    (nil? d)
    (is-dot-dir? (get-name d))
    (and
      (or
        (:include-hidden settings)
        (not (hidden-dir? d)))
      (or
        (empty? (:in-dir-patterns settings))
        (some #(re-find % (.getPath d)) (:in-dir-patterns settings)))
      (or
        (empty? (:out-dir-patterns settings))
        (not-any? #(re-find % (.getPath d)) (:out-dir-patterns settings))))))

(defn is-matching-ext?
  ([^String ext ^FindSettings settings]
    (is-matching-ext? ext (:in-extensions settings) (:out-extensions settings)))
  ([^String ext in-extensions out-extensions]
   (and
    (or
     (empty? in-extensions)
     (some #(= % ext) in-extensions))
    (or
     (empty? out-extensions)
     (not-any? #(= % ext) out-extensions)))))

(defn is-matching-archive-ext?
  ([^String ext ^FindSettings settings]
    (is-matching-ext? ext (:in-archive-extensions settings) (:out-archive-extensions settings)))
  ([^String ext in-extensions out-extensions]
    (is-matching-ext? ext in-extensions out-extensions)))

(defn has-matching-archive-ext?
  ([^FileResult fr ^FindSettings settings]
    (has-matching-archive-ext? fr (:in-archive-extensions settings) (:out-archive-extensions settings)))
  ([^FileResult fr in-extensions out-extensions]
    (or
      (and
        (empty? in-extensions)
        (empty? out-extensions))
      (is-matching-ext? (get-ext (:file fr)) in-extensions out-extensions))))

(defn has-matching-ext?
  ([^FileResult fr ^FindSettings settings]
    (has-matching-ext? fr (:in-extensions settings) (:out-extensions settings)))
  ([^FileResult fr in-extensions out-extensions]
   (or
     (and
       (empty? in-extensions)
       (empty? out-extensions))
     (is-matching-ext? (get-ext (:file fr)) in-extensions out-extensions))))

(defn is-matching-file-name?
  ([^String file-name ^FindSettings settings]
    (is-matching-file-name? file-name (:in-file-patterns settings) (:out-file-patterns settings)))
  ([^String file-name in-file-patterns out-file-patterns]
    (and
      (or
        (empty? in-file-patterns)
        (some #(re-find % file-name) in-file-patterns))
      (or
        (empty? out-file-patterns)
        (not-any? #(re-find % file-name) out-file-patterns)))))

(defn is-matching-archive-file-name?
  ([^String file-name ^FindSettings settings]
    (is-matching-file-name? file-name (:in-archive-file-patterns settings) (:out-archive-file-patterns settings)))
  ([^String file-name in-file-patterns out-file-patterns]
    (is-matching-file-name? file-name in-file-patterns out-file-patterns)))

(defn is-matching-file-type?
  ([file-type ^FindSettings settings]
    (is-matching-file-type? file-type (:in-file-types settings) (:out-file-types settings)))
  ([file-type in-file-types out-file-types]
    (and
      (or
        (empty? in-file-types)
        (contains? in-file-types file-type))
      (or
        (empty? out-file-types)
        (not (contains? out-file-types file-type))))))

(defn format-date [^java.util.Date dt]
  (if (nil? dt)
    ""
    (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") dt)))

(defn is-matching-file-size?
  ([^long file-size ^FindSettings settings]
    (is-matching-file-size? file-size (:min-size settings) (:max-size settings)))
  ([^long file-size ^long min-size ^long max-size]
    (and
      (or
        (= 0 max-size)
        (<= 0 file-size max-size))
      (or
        (= 0 min-size)
        (>= file-size min-size)))))

(defn is-matching-last-mod?
  ([^java.nio.file.attribute.FileTime last-mod ^FindSettings settings]
    (is-matching-last-mod? last-mod (:min-last-mod settings) (:max-last-mod settings)))
  ([^java.nio.file.attribute.FileTime last-mod
    ^java.util.Date min-last-mod
    ^java.util.Date max-last-mod]
    (and
      (or
       (nil? max-last-mod)
       (<= (.toMillis last-mod) (.getTime max-last-mod)))
      (or
        (nil? min-last-mod)
        (>= (.toMillis last-mod) (.getTime min-last-mod))))))

(defn is-matching-archive-file-result? [^FileResult fr ^FindSettings settings]
  (and
    (is-matching-dir? (.getParentFile (:file fr)) settings)
    (has-matching-archive-ext? fr settings)
    (is-matching-archive-file-name? (.getName (:file fr)) settings)))

(defn is-matching-file-result? [^FileResult fr ^FindSettings settings]
  (and
    (is-matching-dir? (.getParentFile (:file fr)) settings)
    (has-matching-ext? fr settings)
    (is-matching-file-name? (.getName (:file fr)) settings)
    (is-matching-file-type? (:file-type fr) settings)
    (is-matching-file-size? (:file-size fr) settings)
    (is-matching-last-mod? (:last-mod fr) settings)))

(defn get-stat [^File f]
  (Files/readAttributes (.toPath f) BasicFileAttributes (into-array java.nio.file.LinkOption [])))

(defn filter-to-file-result [^File f ^FindSettings settings]
  (if
    (and
      (not (:include-hidden settings))
      (hidden-file? f))
    nil
    (let [file-type (get-file-type f)
          stat (if (or (need-last-mod settings) (need-size settings)) (get-stat f) nil)
          file-size (if (need-size settings) (.size stat) 0)
          last-mod (if (need-last-mod settings) (.lastModifiedTime stat) nil)
          fr (new-file-result f file-type file-size last-mod)]
      (if
        (= :archive file-type)
        (if
          (or
            (not (:include-archives settings))
            (not (is-matching-archive-file-result? fr settings)))
          nil
          fr)
        (if
          (or
           (:archives-only settings)
           (not (is-matching-file-result? fr settings)))
          nil
          fr)))))

(defn filter-file-by-depth [^File f path-sep-count ^FindSettings settings]
  (let [file-sep-count (sep-count (.toString f))
        depth (- file-sep-count path-sep-count)]
    (and
      (>= depth (:min-depth settings))
      (or
        (< (:max-depth settings) 1)
        (<= depth (:max-depth settings))))))

(defn get-file-results-under-path
  [^FindSettings settings, ^File path-file ^Integer min-depth ^Integer max-depth ^Integer current-depth]
    (if (or (and (> min-depth -1) (< current-depth min-depth)) (and (> max-depth -1) (> current-depth max-depth)))
      []
      (let [path-elems (.listFiles path-file)
            path-dirs (filter #(is-matching-dir? % settings) (filter #(.isDirectory %) path-elems))
            path-files (filter #(.isFile %) path-elems)
            path-results (filter #(not (nil? %)) (map #(filter-to-file-result % settings) path-files))
            next-depth (inc current-depth)]
        (if (empty? path-dirs)
          path-results
          (concat path-results
                  (mapcat #(get-file-results-under-path settings % min-depth max-depth next-depth) path-dirs))))))

(defn get-file-results-for-path [^FindSettings settings, ^String path]
  (let [path-file (file path)]
    (if (.isFile path-file)
      (if
        (< (:min-depth settings) 1)
        (let [path-file-result (filter-to-file-result path-file settings)]
          (if (not (nil? path-file-result))
            [path-file-result]
            []))
        [])
      (if (= (:max-depth settings) 0)
        []
        (if (:recursive settings)
          (get-file-results-under-path settings path-file (:min-depth settings) (:max-depth settings) 1)
          (get-file-results-under-path settings path-file (:min-depth settings) 1 1))))))

(defn get-file-results [^FindSettings settings]
  (sort-results (mapcat #(get-file-results-for-path settings %) (:paths settings)) settings))

(defn find-files [^FindSettings settings]
  (let [errs (validate-settings settings)]
    (if (empty? errs)
      [(into [] (get-file-results settings)) []]
      [[] errs])))

(defn get-matching-dirs [file-results]
  (sort (distinct (map #(.getParent (:file %)) file-results))))

(defn print-matching-dirs [file-results]
  (let [dirs (get-matching-dirs file-results)]
    (if (empty? dirs)
      (log-msg "\nMatching directories: 0")
      (do
        (log-msg (format "\nMatching directories (%d):" (count dirs)))
        (doseq [d dirs] (log-msg d))))))

(defn get-matching-files [file-results]
  (map #(file-result-path %) file-results))

(defn print-matching-files [file-results]
  (let [files (get-matching-files file-results)]
    (if (empty? files)
      (log-msg "\nMatching files: 0")
      (do
        (log-msg (format "\nMatching files (%d):" (count files)))
        (doseq [f files] (log-msg f))))))
