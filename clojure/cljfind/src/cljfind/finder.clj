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
        [cljfind.findsettings :only (need-stat)])
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

(defn has-matching-dir? [^File f ^FindSettings settings]
  (let [d (.getParentFile f)]
    (or
      (nil? d)
      (is-matching-dir? d settings))))

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

(defn has-matching-ext?
  ([^File f ^FindSettings settings]
    (has-matching-ext? f (:in-extensions settings) (:out-extensions settings)))
  ([^File f in-extensions out-extensions]
    (if
      (or
        (not (empty? in-extensions))
        (not (empty? out-extensions)))
      (is-matching-ext? (get-ext f) in-extensions out-extensions)
      true)))

(defn is-matching-file-pattern?
  ([^String filename ^FindSettings settings]
    (is-matching-file-pattern? filename (:in-file-patterns settings) (:out-file-patterns settings)))
  ([filename in-file-patterns out-file-patterns]
    (and
      (or
        (empty? in-file-patterns)
        (some #(re-find % filename) in-file-patterns))
      (or
        (empty? out-file-patterns)
        (not-any? #(re-find % filename) out-file-patterns)))))

(defn has-matching-file-pattern?
  ([^File f ^FindSettings settings]
    (has-matching-file-pattern? f (:in-file-patterns settings) (:out-file-patterns settings)))
  ([^File f in-file-patterns out-file-patterns]
    (if
      (or
        (not (empty? in-file-patterns))
        (not (empty? out-file-patterns)))
      (is-matching-file-pattern? (.getName f) in-file-patterns out-file-patterns)
      true)))

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

(defn has-matching-file-type?
  ([^File f ^FindSettings settings]
    (has-matching-file-type? f (:in-file-types settings) (:out-file-types settings)))
  ([^File f in-file-types out-file-types]
    (if
      (or
        (not (empty? in-file-types))
        (not (empty? out-file-types)))
      (is-matching-file-type? (get-file-type f) in-file-types out-file-types)
      true)))

(defn format-date [dt]
  (if (nil? dt)
    ""
    (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") dt)))

(defn is-matching-stat? [stat ^FindSettings settings]
  (or
    (nil? stat)
    (and
      (or
        (nil? (:max-last-mod settings))
        (<= (.toMillis (.lastModifiedTime stat)) (.getTime (:max-last-mod settings))))
      (or
        (nil? (:min-last-mod settings))
        (>= (.toMillis (.lastModifiedTime stat)) (.getTime (:min-last-mod settings))))
      (or
        (= 0 (:max-size settings))
        (<= 0 (.size stat) (:max-size settings)))
      (or
        (= 0 (:min-size settings))
        (>= (.size stat) (:min-size settings))))))

(defn has-matching-stat? [^FileResult fr ^FindSettings settings]
  (is-matching-stat? (:stat fr) settings))

(defn is-matching-archive-file-result? [^FileResult fr ^FindSettings settings]
  (if
    (or
      (not (has-matching-dir? (:file fr) settings))
      (not (has-matching-ext? (:file fr) (:in-archive-extensions settings) (:out-archive-extensions settings)))
      (not (has-matching-file-pattern? (:file fr) (:in-archive-file-patterns settings) (:out-archive-patterns settings)))
      (not (has-matching-stat? fr settings)))
    false
    true))

(defn is-matching-file-result? [^FileResult fr ^FindSettings settings]
  (if
    (or
     (not (has-matching-dir? (:file fr) settings))
     (not (has-matching-ext? (:file fr) (:in-extensions settings) (:out-extensions settings)))
     (not (has-matching-file-pattern? (:file fr) (:in-file-patterns settings) (:out-file-patterns settings)))
     (not (is-matching-file-type? (:file-type fr) (:in-file-types settings) (:out-file-types settings)))
     (not (has-matching-stat? fr settings)))
    false
    true))

(defn get-stat [^File f]
  (Files/readAttributes (.toPath f) BasicFileAttributes (into-array java.nio.file.LinkOption [])))

(defn filter-to-file-result [^File f ^FindSettings settings]
  (if
    (and
      (not (:include-hidden settings))
      (hidden-file? f))
    nil
    (let [file-type (get-file-type f)
          stat (if (need-stat settings) (get-stat f) nil)
          fr (new-file-result f file-type stat)]
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

(defn get-file-results-under-path [^FindSettings settings, ^File path-file]
  (if (= (:max-depth settings) 0)
    []
    (let [check-depth (or (> (:max-depth settings) 0) (> (:min-depth settings) 0))
          path-files (if (:recursive settings) (file-seq path-file) (.listFiles path-file))
          path-sep-count (if check-depth (sep-count (.toString path-file)) 0)
          filter-by-depth (if check-depth filter-file-by-depth (fn [^File f c settings] true))]
      (->> path-files
           (filter #(.isFile %))
           (filter #(filter-by-depth % path-sep-count settings))
           (map #(filter-to-file-result % settings))
           (filter #(not (nil? %)))))))

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
      (get-file-results-under-path settings path-file))))

(defn get-file-results
  ([^FindSettings settings]
    (let [paths (into [] (:paths settings))]
      (get-file-results settings paths [])))
  ([^FindSettings settings paths file-results]
   (if (empty? paths)
      (sort-results file-results settings)
      (get-file-results settings (rest paths) (concat file-results (get-file-results-for-path settings (nth paths 0)))))))

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
