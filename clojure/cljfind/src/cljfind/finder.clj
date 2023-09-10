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
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute BasicFileAttributes)
           (java.util.jar JarFile)
           (java.util.zip ZipFile))
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

(defn validate-settings [settings]
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
    (take 1 (filter #(not (= % nil)) (map #(% settings) tests)))))

(defn is-matching-dir? [^File d settings]
  (or
    (is-dot-dir? (get-name d))
    (and
      (or
        (not (:exclude-hidden settings))
        (not (hidden-dir? d)))
      (or
        (empty? (:in-dir-patterns settings))
        (some #(re-find % (.getPath d)) (:in-dir-patterns settings)))
      (or
        (empty? (:out-dir-patterns settings))
        (not-any? #(re-find % (.getPath d)) (:out-dir-patterns settings))))))

(defn has-matching-dir? [^File f settings]
  (let [d (.getParentFile f)]
    (or
     (nil? d)
     (is-matching-dir? d settings))))

(defn is-matching-ext?
  ([ext settings]
   (is-matching-ext? ext (:in-extensions settings) (:out-extensions settings)))
  ([ext in-extensions out-extensions]
   (and
    (or
     (empty? in-extensions)
     (some #(= % ext) in-extensions))
    (or
     (empty? out-extensions)
     (not-any? #(= % ext) out-extensions)))))

(defn has-matching-ext?
  ([^File f settings]
   (has-matching-ext? f (:in-extensions settings) (:out-extensions settings)))
  ([^File f in-extensions out-extensions]
   (if
     (or
      (not (empty? in-extensions))
      (not (empty? out-extensions)))
     (is-matching-ext? (get-ext f) in-extensions out-extensions)
     true)))

(defn is-matching-file-pattern?
  ([filename settings]
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
  ([^File f settings]
   (has-matching-file-pattern? f (:in-file-patterns settings) (:out-file-patterns settings)))
  ([^File f in-file-patterns out-file-patterns]
  (if
    (or
     (not (empty? in-file-patterns))
     (not (empty? out-file-patterns)))
    (is-matching-file-pattern? (.getName f) in-file-patterns out-file-patterns)
    true)))

(defn is-matching-file-type?
  ([file-type settings]
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
  ([^File f settings]
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

(defn is-matching-stat? [stat settings]
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

(defn has-matching-stat? [fr settings]
  (is-matching-stat? (:stat fr) settings))

(defn is-matching-archive-file-result? [fr settings]
  (if
    (or
     (not (has-matching-dir? (:file fr) settings))
     (not (has-matching-ext? (:file fr) (:in-archive-extensions settings) (:out-archive-extensions settings)))
     (not (has-matching-file-pattern? (:file fr) (:in-archive-file-patterns settings) (:out-archive-patterns settings)))
     (not (has-matching-stat? fr settings)))
    false
    true))

(defn is-matching-file-result? [fr settings]
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

(defn filter-to-file-result [^File f settings]
  (if
    (and
      (hidden-file? f)
      (:exclude-hidden settings))
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

(defn filter-dir-by-depth [d path-sep-count settings]
  (if (nil? d)
    true
    (let [dir-sep-count (sep-count (.toString d))
          depth (- dir-sep-count path-sep-count)]
      (or
       (< (:max-depth settings) 1)
       (<= depth (:max-depth settings))))))

(defn filter-file-by-depth [f path-sep-count settings]
  (let [file-sep-count (sep-count (.toString f))
        depth (- file-sep-count path-sep-count)]
    (and
     (>= depth (:min-depth settings))
     (or
      (< (:max-depth settings) 1)
      (<= depth (:max-depth settings))))))

(defn get-file-results-for-path [settings, ^String path]
  (let [pathfile (file path)]
    (if (.isFile pathfile)
      (if (< (:min-depth settings) 1)
        (vec
         (filter
          #(not (nil? %))
          (map #(filter-to-file-result % settings) [pathfile])))
        [])
      (if (not (= (:max-depth settings) 0))
        (if (:recursive settings)
          (let [path-sep-count (sep-count path)]
            (vec
             (filter
              #(not (nil? %))
              (map #(filter-to-file-result % settings)
                   (filter #(filter-file-by-depth % path-sep-count settings)
                           (filter #(filter-dir-by-depth (.getParent %) path-sep-count settings)
                                   (filter #(.isFile %) (file-seq pathfile))))))))
          (vec
           (filter
            #(not (nil? %))
            (map #(filter-to-file-result % settings) (filter #(.isFile %) (.listFiles pathfile))))))
        []))))

(defn get-file-results
  ([settings]
    (get-file-results settings (:paths settings) []))
  ([settings paths file-results]
   (if (empty? paths)
      (sort-results file-results settings)
      (get-file-results settings (rest paths) (concat file-results (get-file-results-for-path settings (first paths)))))))

(defn find-files [settings]
  (let [errs (validate-settings settings)]
    (if (empty? errs)
      [(get-file-results settings) []]
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
