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
           (java.nio.file Files Path Paths)
           (java.nio.file.attribute BasicFileAttributes)
           (java.util.jar JarFile)
           (java.util.stream Collectors)
           (java.util.zip ZipFile)
           (cljfind.fileresult FileResult)
           (cljfind.findsettings FindSettings))
  (:use [cljfind.common :only (log-msg)]
        [cljfind.fileresult :only
         (new-file-result file-result-path sort-results)]
        [cljfind.filetypes :only (get-file-type)]
        [cljfind.fileutil :only
          (exists-path? expand-path get-path-ext get-path-name get-parent-name hidden-dir-path? hidden-file-path?
           is-dir-path? is-dot-dir? is-file-path? is-symlink-path? path-str readable-path?)]
        [cljfind.findsettings :only (need-last-mod need-size)])
  (:require [java-time.api :as jt]))

(defn matches-any-pattern? [^String s pp]
  (some #(re-find % s) pp))

(defn any-matches-any-pattern? [ss pp]
  (some #(not (= % nil)) (map #(matches-any-pattern? % pp) ss)))

(defn validate-settings [^FindSettings settings]
  (let [paths (:paths settings)
        tests [(fn [ss] (if (empty? paths) "Startpath not defined" nil))
               (fn [ss]
                 (if
                   (some #(not (exists-path? %)) paths)
                   (if
                     (some #(not (exists-path? (expand-path %))) paths)
                     "Startpath not found"
                     nil)
                   nil))
               (fn [ss]
                 (if
                   (some #(not (readable-path? %)) paths)
                   (if
                     (some #(not (readable-path? (expand-path %))) paths)
                     "Startpath not readable"
                     nil)
                   nil))
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

(defn is-matching-dir? [^Path d ^FindSettings settings]
  (or
    (nil? d)
    (is-dot-dir? (get-path-name d))
    (and
      (or
        (:include-hidden settings)
        (not (hidden-dir-path? d)))
      (or
        (empty? (:in-dir-patterns settings))
        (some #(re-find % (path-str d)) (:in-dir-patterns settings)))
      (or
        (empty? (:out-dir-patterns settings))
        (not-any? #(re-find % (path-str d)) (:out-dir-patterns settings))))))

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
      (is-matching-ext? (get-path-ext (:path fr)) in-extensions out-extensions))))

(defn has-matching-ext?
  ([^FileResult fr ^FindSettings settings]
    (has-matching-ext? fr (:in-extensions settings) (:out-extensions settings)))
  ([^FileResult fr in-extensions out-extensions]
   (or
     (and
       (empty? in-extensions)
       (empty? out-extensions))
     (is-matching-ext? (get-path-ext (:path fr)) in-extensions out-extensions))))

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
    (is-matching-dir? (.getParent (:path fr)) settings)
    (has-matching-archive-ext? fr settings)
    (is-matching-archive-file-name? (get-path-name (:path fr)) settings)))

(defn is-matching-file-result? [^FileResult fr ^FindSettings settings]
  (and
    (is-matching-dir? (.getParent (:path fr)) settings)
    (has-matching-ext? fr settings)
    (is-matching-file-name? (get-path-name (:path fr)) settings)
    (is-matching-file-type? (:file-type fr) settings)
    (is-matching-file-size? (:file-size fr) settings)
    (is-matching-last-mod? (:last-mod fr) settings)))

(defn get-stat ^BasicFileAttributes [^Path p]
  (Files/readAttributes p BasicFileAttributes (into-array java.nio.file.LinkOption [])))

(defn filter-to-file-result [^Path p ^FindSettings settings]
  (if
    (and
      (not (:include-hidden settings))
      (hidden-file-path? p))
    nil
    (let [file-type (get-file-type p)]
      (if (and (= :archive file-type) (not (:include-archives settings)) (not (:archives-only settings)))
        nil
        (let [stat (if (or (need-last-mod settings) (need-size settings)) (get-stat p) nil)
              file-size (if (need-size settings) (.size stat) 0)
              last-mod (if (need-last-mod settings) (.lastModifiedTime stat) nil)
              fr (new-file-result p file-type file-size last-mod)]
          (if
            (= :archive file-type)
            (if
              (is-matching-archive-file-result? fr settings)
              fr
              nil)
            (if
              (and
                (not (:archives-only settings))
                (is-matching-file-result? fr settings))
              fr
              nil)))))))

(defn list-paths-under-dir-path [^Path dir-path]
  (let [stream (Files/list dir-path)]
    (try
      ;; because a java Stream is not the same as a stream in clojure, we need to convert it
      (vec (iterator-seq (.iterator stream)))
      (finally
        (.close stream)))))

(defn get-file-results-under-path
  [^FindSettings settings, ^Path path ^Integer min-depth ^Integer max-depth ^Integer current-depth]
  (if
    (and
      (> max-depth -1)
      (> current-depth max-depth))
    []
    (let [path-elems (filter #(or (not (is-symlink-path? %)) (:follow-symlinks settings)) (list-paths-under-dir-path path))
          recurse (or (= max-depth -1) (< current-depth max-depth))
          path-dirs (if recurse (filter #(is-matching-dir? % settings) (filter #(is-dir-path? %) path-elems)) [])
          path-files (if (and (> min-depth -1) (< current-depth min-depth)) [] (filter #(is-file-path? %) path-elems))
          path-results (filter #(not (nil? %)) (map #(filter-to-file-result % settings) path-files))
          next-depth (inc current-depth)]
      (if (empty? path-dirs)
        path-results
        (concat path-results
                (mapcat #(get-file-results-under-path settings % min-depth max-depth next-depth) path-dirs))))))

(defn get-file-results-for-path [^FindSettings settings, ^Path path]
  (let [expanded-path (expand-path path)]
    (if (is-file-path? expanded-path)
      (if
        (< (:min-depth settings) 1)
        (let [path-file-result (filter-to-file-result expanded-path settings)]
          (if (not (nil? path-file-result))
            [path-file-result]
            []))
        [])
      (if (= (:max-depth settings) 0)
        []
        (if (:recursive settings)
          (get-file-results-under-path settings expanded-path (:min-depth settings) (:max-depth settings) 1)
          (get-file-results-under-path settings expanded-path (:min-depth settings) 1 1))))))


(defn get-file-results [^FindSettings settings]
  (sort-results (mapcat #(get-file-results-for-path settings %) (:paths settings)) settings))

(defn find-files [^FindSettings settings]
  (let [errs (validate-settings settings)]
    (if (empty? errs)
      [(into [] (get-file-results settings)) []]
      [[] errs])))

(defn get-matching-dirs [file-results]
  (sort (distinct (map #(get-parent-name (:path %)) file-results))))

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
