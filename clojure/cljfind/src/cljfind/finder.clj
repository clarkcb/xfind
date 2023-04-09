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
        [cljfind.filetypes :only (get-filetype)]
        [cljfind.fileutil :only
          (get-ext get-name hidden-dir? hidden-file?
            is-dot-dir?)]
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
              ]
       ]
    (take 1 (filter #(not (= % nil)) (map #(% settings) tests)))))

(defn is-matching-dir? [^File d settings]
  (or
    (is-dot-dir? (get-name d))
    (and
      (or
        (not (:excludehidden settings))
        (not (hidden-dir? d)))
      (or
        (empty? (:in-dirpatterns settings))
        (some #(re-find % (.getPath d)) (:in-dirpatterns settings)))
      (or
        (empty? (:out-dirpatterns settings))
        (not-any? #(re-find % (.getPath d)) (:out-dirpatterns settings))))))

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

(defn is-matching-filepattern?
  ([filename settings]
   (is-matching-filepattern? filename (:in-filepatterns settings) (:out-filepatterns settings)))
  ([filename in-filepatterns out-filepatterns]
   (and
    (or
     (empty? in-filepatterns)
     (some #(re-find % filename) in-filepatterns))
    (or
     (empty? out-filepatterns)
     (not-any? #(re-find % filename) out-filepatterns)))))

(defn has-matching-filepattern?
  ([^File f settings]
   (has-matching-filepattern? f (:in-filepatterns settings) (:out-filepatterns settings)))
  ([^File f in-filepatterns out-filepatterns]
  (if
    (or
     (not (empty? in-filepatterns))
     (not (empty? out-filepatterns)))
    (is-matching-filepattern? (.getName f) in-filepatterns out-filepatterns)
    true)))

(defn is-matching-filetype?
  ([filetype settings]
   (is-matching-filetype? filetype (:in-filetypes settings) (:out-filetypes settings)))
  ([filetype in-filetypes out-filetypes]
   (and
    (or
     (empty? in-filetypes)
     (contains? in-filetypes filetype))
    (or
     (empty? out-filetypes)
     (not (contains? out-filetypes filetype))))))

(defn has-matching-filetype?
  ([^File f settings]
   (has-matching-filetype? f (:in-filetypes settings) (:out-filetypes settings)))
  ([^File f in-filetypes out-filetypes]
  (if
    (or
     (not (empty? in-filetypes))
     (not (empty? out-filetypes)))
    (is-matching-filetype? (get-filetype f) in-filetypes out-filetypes)
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
     (nil? (:maxlastmod settings))
     (<= (.toMillis (.lastModifiedTime stat)) (.getTime (:maxlastmod settings))))
    (or
     (nil? (:minlastmod settings))
     (>= (.toMillis (.lastModifiedTime stat)) (.getTime (:minlastmod settings))))
    (or
     (= 0 (:maxsize settings))
     (<= 0 (.size stat) (:maxsize settings)))
    (or
     (= 0 (:minsize settings))
     (>= (.size stat) (:minsize settings))))))

(defn has-matching-stat? [fr settings]
  (is-matching-stat? (:stat fr) settings))

(defn is-matching-archive-file-result? [fr settings]
  (if
    (or
     (not (has-matching-dir? (:file fr) settings))
     (not (has-matching-ext? (:file fr) (:in-archiveextensions settings) (:out-archiveextensions settings)))
     (not (has-matching-filepattern? (:file fr) (:in-archivefilepatterns settings) (:out-archivefilepatterns settings)))
     (not (has-matching-stat? fr settings)))
    false
    true))

(defn is-matching-file-result? [fr settings]
  (if
    (or
     (not (has-matching-dir? (:file fr) settings))
     (not (has-matching-ext? (:file fr) (:in-extensions settings) (:out-extensions settings)))
     (not (has-matching-filepattern? (:file fr) (:in-filepatterns settings) (:out-filepatterns settings)))
     (not (is-matching-filetype? (:filetype fr) (:in-filetypes settings) (:out-filetypes settings)))
     (not (has-matching-stat? fr settings)))
    false
    true))

(defn get-stat [^File f]
  (Files/readAttributes (.toPath f) BasicFileAttributes (into-array java.nio.file.LinkOption [])))

(defn filter-to-file-result [^File f settings]
  (if
    (and
      (hidden-file? f)
      (:excludehidden settings))
    nil
    (let [filetype (get-filetype f)
          stat (if (need-stat settings) (get-stat f) nil)
          fr (new-file-result f filetype stat)]
      (if
        (= :archive filetype)
        (if
          (or
            (not (:includearchives settings))
            (not (is-matching-archive-file-result? fr settings)))
          nil
          fr)
        (if
          (or
           (:archivesonly settings)
           (not (is-matching-file-result? fr settings)))
          nil
          fr)))))

(defn get-file-results-for-path [settings, ^String path]
  (let [pathfile (file path)]
    (if (.isFile pathfile)
      (vec
       (filter
        #(not (nil? %))
        (map #(filter-to-file-result % settings) [pathfile])))
      (if (:recursive settings)
        (vec
         (filter
          #(not (nil? %))
          (map #(filter-to-file-result % settings) (filter #(.isFile %) (file-seq pathfile)))))
        (vec
         (filter
          #(not (nil? %))
          (map #(filter-to-file-result % settings) (filter #(.isFile %) (.listFiles pathfile)))))))))

(defn get-file-results
  ([settings]
    (get-file-results settings (:paths settings) []))
  ([settings paths fileresults]
    (if (empty? paths)
      (sort-results fileresults settings)
      (get-file-results settings (rest paths) (concat fileresults (get-file-results-for-path settings (first paths)))))))

(defn find-files [settings]
  (let [errs (validate-settings settings)]
    (if (empty? errs)
      [(get-file-results settings) []]
      [[] errs])))

(defn get-matching-dirs [fileresults]
  (sort (distinct (map #(.getParent (:file %)) fileresults))))

(defn print-matching-dirs [fileresults]
  (let [dirs (get-matching-dirs fileresults)]
    (if (empty? dirs)
      (log-msg "\nMatching directories: 0")
      (do
        (log-msg (format "\nMatching directories (%d):" (count dirs)))
        (doseq [d dirs] (log-msg d))))))

(defn get-matching-files [fileresults]
  (map #(file-result-path %) fileresults))

(defn print-matching-files [fileresults]
  (let [files (get-matching-files fileresults)]
    (if (empty? files)
      (log-msg "\nMatching files: 0")
      (do
        (log-msg (format "\nMatching files (%d):" (count files)))
        (doseq [f files] (log-msg f))))))
