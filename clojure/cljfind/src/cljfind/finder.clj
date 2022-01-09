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
           (java.util.jar JarFile)
           (java.util.zip ZipFile))
  (:use [clojure.java.io :only (file reader)]
        [clojure.string :as str :only (join trim upper-case)]
        [cljfind.common :only (log-msg)]
        [cljfind.fileresult :only (new-file-result file-result-path)]
        [cljfind.filetypes :only (archive-file? get-filetype)]
        [cljfind.fileutil :only
          (get-ext get-files-in-directory get-name hidden-dir? hidden-file?
            is-dot-dir?)]))

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

(defn is-find-dir? [^File d settings]
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

(defn is-matching-archive-file? [^File f settings]
  (and
    (or
      (nil? (.getParentFile f))
      (is-find-dir? (.getParentFile f) settings))
    (or
      (empty? (:in-archiveextensions settings))
      (some #(= % (get-ext f)) (:in-archiveextensions settings)))
    (or
      (empty? (:out-archiveextensions settings))
      (not-any? #(= % (get-ext f)) (:out-archiveextensions settings)))
    (or
      (empty? (:in-archivefilepatterns settings))
      (some #(re-find % (.getName f)) (:in-archivefilepatterns settings)))
    (or
      (empty? (:out-archivefilepatterns settings))
      (not-any? #(re-find % (.getName f)) (:out-archivefilepatterns settings)))))

(defn is-matching-file? [^File f settings]
  (and
    (or
      (nil? (.getParentFile f))
      (is-find-dir? (.getParentFile f) settings))
    (or
      (empty? (:in-extensions settings))
      (some #(= % (get-ext f)) (:in-extensions settings)))
    (or
      (empty? (:out-extensions settings))
      (not-any? #(= % (get-ext f)) (:out-extensions settings)))
    (or
      (empty? (:in-filepatterns settings))
      (some #(re-find % (.getName f)) (:in-filepatterns settings)))
    (or
      (empty? (:out-filepatterns settings))
      (not-any? #(re-find % (.getName f)) (:out-filepatterns settings)))
    (or
      (empty? (:in-filetypes settings))
      (contains? (:in-filetypes settings) (get-filetype f)))
    (or
      (empty? (:out-filetypes settings))
      (not (contains? (:out-filetypes settings) (get-filetype f))))))

(defn filter-file? [^File f settings]
  (and
    (or
      (not (hidden-file? f))
      (not (:excludehidden settings)))
    (if (archive-file? f)
      (and
        (:includearchives settings)
        (is-matching-archive-file? f settings))
      (and
        (not (:archivesonly settings))
        (is-matching-file? f settings)))))

(defn get-file-results-for-path [settings, ^String path]
  (let [pathfile (file path)]
    (if (.isFile pathfile)
      (vec
       (map
        #(new-file-result % (get-filetype %))
        (filter #(filter-file? % settings) [pathfile])))
      (if (:recursive settings)
        (vec
         (map
          #(new-file-result % (get-filetype %))
          (filter #(filter-file? % settings) (filter #(.isFile %) (file-seq pathfile)))))
        (vec
         (map
          #(new-file-result % (get-filetype %))
          (filter #(filter-file? % settings) (filter #(.isFile %) (.listFiles pathfile)))))))))

(defn get-file-results
  ([settings]
    (get-file-results settings (:paths settings) []))
  ([settings paths fileresults]
    (if (empty? paths)
      fileresults
      (let [nextfileresults (get-file-results-for-path settings (first paths))]
        (get-file-results settings (rest paths) (concat fileresults nextfileresults))))))

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
  (sort (distinct (map #(file-result-path %) fileresults))))

(defn print-matching-files [fileresults]
  (let [files (get-matching-files fileresults)]
    (if (empty? files)
      (log-msg "\nMatching files: 0")
      (do
        (log-msg (format "\nMatching files (%d):" (count files)))
        (doseq [f files] (log-msg f))))))
