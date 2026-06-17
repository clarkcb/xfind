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
           (java.nio.file.attribute BasicFileAttributes FileTime)
           (java.util.jar JarFile)
           (java.util.stream Collectors)
           (java.util.zip ZipFile)
           (cljfind.fileresult FileResult)
           (cljfind.findsettings FindSettings))
  (:use [cljfind.common :only (log-msg)]
        [cljfind.fileresult :only
         (new-file-result file-result-path get-dir-path-formatter get-file-result-formatter sort-file-results)]
        [cljfind.filetypes :only (get-file-type)]
        [cljfind.fileutil :only
         (exists-path? expand-path get-ext get-path-ext get-path-name get-parent-name hidden? hidden-dir-path?
                       hidden-file-path? dir-path? dot-dir? regular-file-path? symlink-path? path-str readable-path?)]
        [cljfind.findsettings :only (need-last-mod need-size)])
  (:require [java-time.api :as jt]))


(defn matches-any-pattern? [^String s patterns]
  (some #(re-find % s) patterns))

(defn any-matches-any-pattern? [ss patterns]
  (some #(not (= % nil)) (map #(matches-any-pattern? % patterns) ss)))

(defn empty-or-matches-any-pattern? [^String s patterns]
  (or (empty? patterns) (some #(re-find % s) patterns)))

(defn empty-or-not-matches-any-pattern? [^String s patterns]
  (or (empty? patterns) (nil? (some #(re-find % s) patterns))))

(defn empty-or-any-matches-any-pattern? [ss patterns]
  (or (empty? patterns) (some #(not (= % nil)) (map #(matches-any-pattern? % patterns) ss))))

(defn empty-or-not-any-matches-any-pattern? [ss patterns]
  (or (empty? patterns) (nil? (some #(not (= % nil)) (map #(matches-any-pattern? % patterns) ss)))))

(defn empty-or-matches-any-elem? [elem coll]
  (or (empty? coll) (some #(= % elem) coll)))

(defn empty-or-not-matches-any-elem? [elem coll]
  (or (empty? coll) (nil? (some #(= % elem) coll))))

(defn format-date [^java.util.Date dt]
  (if (nil? dt)
    ""
    (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") dt)))

(def ^"[Ljava.nio.file.LinkOption;" NO-LINK-OPTIONS
  (make-array java.nio.file.LinkOption 0))

(defn path-parent ^Path [^Path p]
  (.getParent p))

(defn file-result-parent ^Path [^FileResult fr]
  (path-parent ^Path (:path fr)))

(defn date-before? [^java.util.Date dt1 ^java.util.Date dt2]
  (.before dt1 dt2))

(defn file-time-to-millis ^long [^FileTime ft]
  (.toMillis ft))

(defn date-to-millis ^long [^java.util.Date dt]
  (.getTime dt))

(defn get-stat ^BasicFileAttributes [^Path p]
  (Files/readAttributes p BasicFileAttributes NO-LINK-OPTIONS))

(defn list-paths-under-dir-path [^Path dir-path]
  (let [stream (Files/list dir-path)]
    (try
      ;; because a java Stream is not the same as a stream in clojure, we need to convert it
      (vec (iterator-seq (.iterator stream)))
      (finally
        (.close stream)))))

(defn get-matching-dirs [file-results]
  (sort (distinct (filter #(not (nil? %)) (map file-result-parent file-results)))))

(defprotocol FinderProtocol
  (validate-settings [this])
  (matching-dir-path-by-hidden? [this dir-path])
  (matching-dir-path-by-in-patterns? [this dir-path])
  (matching-dir-path-by-out-patterns? [this dir-path])
  (traversable-dir-path? [this dir-path])
  (matching-dir-path? [this dir-path])
  (nil-or-matching-dir-path? [this dir-path])
  (matching-file-name-by-hidden? [this file-name])
  (matching-file-path-by-hidden? [this file-path])
  (matching-archive-ext? [this ext])
  (has-matching-archive-ext? [this fr])
  (matching-archive-file-name-by-in-patterns? [this file-name])
  (matching-archive-file-name-by-out-patterns? [this file-name])
  (matching-archive-file-name? [this file-name])
  (matching-archive-file-path-by-in-patterns? [this file-path])
  (matching-archive-file-path-by-out-patterns? [this file-path])
  (matching-archive-file-path? [this file-path])
  (matching-archive-file-result? [this fr])
  (matching-ext? [this ext])
  (has-matching-ext? [this fr])
  (matching-file-name-by-in-patterns? [this file-name])
  (matching-file-name-by-out-patterns? [this file-name])
  (matching-file-name? [this file-name])
  (matching-file-path-by-in-patterns? [this file-path])
  (matching-file-path-by-out-patterns? [this file-path])
  (matching-file-path? [this file-path])
  (matching-file-type? [this file-type])
  (matching-file-size? [this file-size])
  (matching-last-mod? [this last-mod])
  (matching-file-result? [this fr])
  (filter-archive-file-path-to-file-result [this file-path])
  (filter-regular-file-path-to-file-result [this file-path file-type])
  (filter-to-file-result [this file-path])
  (get-file-results-under-path [this path min-depth max-depth current-depth])
  (get-file-results-for-path [this path])
  (get-file-results [this])
  (find-files [this])
  (print-matching-dirs [this file-results])
  (print-matching-files [this file-results])
)

; record to a Finder instance: settings + file types
(defrecord Finder [^FindSettings settings]
  FinderProtocol
  (validate-settings [this]
    (let [settings (:settings this)
          expanded-paths (map #(expand-path %) (:paths settings))
          symlink-paths (filter #(symlink-path? %) expanded-paths)
          dir-paths (filter #(dir-path? %) expanded-paths)
          file-paths (filter #(regular-file-path? %) expanded-paths)
          tests [(fn [ss] (if (empty? expanded-paths) "Startpath not defined" nil))
                 (fn [ss]
                   (if
                     (some #(not (exists-path? %)) expanded-paths)
                     "Startpath not found"
                     nil))
                 (fn [ss]
                   (if
                     (some #(not (readable-path? %)) expanded-paths)
                     "Startpath not readable"
                     nil))
                 (fn [ss]
                   (if
                     (and
                      (not (empty? symlink-paths))
                      (not (:follow-symlinks ss)))
                     "Startpath does not match find settings"
                     nil))
                 (fn [ss]
                   (if
                     (and
                      (not (empty? dir-paths))
                      (or
                       (some #(not (matching-dir-path-by-hidden? this %)) dir-paths)
                       (some #(not (matching-dir-path-by-out-patterns? this %)) dir-paths)))
                     "Startpath does not match find settings"
                     nil))
                 (fn [ss]
                   (if
                     (and
                      (not (empty? file-paths))
                      (or
                       (some #(not (matching-file-path-by-hidden? this %)) file-paths)
                       (some #(not (matching-file-path-by-out-patterns? this %)) file-paths)))
                     "Startpath does not match find settings"
                     nil))
                 (fn [ss]
                   (if
                     (and
                      (> (:max-depth ss) -1)
                      (> (:min-depth ss) (:max-depth ss)))
                     "Invalid range for mindepth and maxdepth"
                     nil))
                 (fn [ss]
                   (if
                     (and
                      (not (nil? (:max-last-mod ss)))
                      (not (nil? (:min-last-mod ss)))
                      (date-before? (:max-last-mod ss) (:min-last-mod ss)))
                     "Invalid range for minlastmod and maxlastmod"
                     nil))
                 (fn [ss]
                   (if
                     (and
                      (> (:max-size ss) 0)
                      (> (:min-size ss) (:max-size ss)))
                     "Invalid range for minsize and maxsize"
                     nil))
                 ]
          ]
      (take 1 (filter #(not (nil? %)) (map #(% settings) tests)))))

;  (matching-dir-path-by-hidden? [this ^Path dir-path]
  (matching-dir-path-by-hidden? [this dir-path]
    (or
      (:include-hidden (:settings this))
      (not (hidden-dir-path? dir-path))))

;  (matching-dir-path-by-in-patterns? [this ^Path dir-path]
  (matching-dir-path-by-in-patterns? [this dir-path]
    (or
      (dot-dir? (get-path-name dir-path))
      (empty-or-matches-any-pattern? (path-str dir-path) (:in-dir-patterns (:settings this)))))

;  (matching-dir-path-by-out-patterns? [this ^Path dir-path]
  (matching-dir-path-by-out-patterns? [this dir-path]
    (or
      (dot-dir? (get-path-name dir-path))
      (empty-or-not-matches-any-pattern? (path-str dir-path) (:out-dir-patterns (:settings this)))))

;  (traversable-dir-path? [this ^Path dir-path]
  (traversable-dir-path? [this dir-path]
    (or
      (dot-dir? (get-path-name dir-path))
      (and
        (matching-dir-path-by-hidden? this dir-path)
        (matching-dir-path-by-out-patterns? this dir-path))))

;  (matching-dir-path? [this ^Path dir-path]
  (matching-dir-path? [this dir-path]
    (or
      (dot-dir? (get-path-name dir-path))
      (and
        (matching-dir-path-by-hidden? this dir-path)
        (matching-dir-path-by-in-patterns? this dir-path)
        (matching-dir-path-by-out-patterns? this dir-path))))

;  (nil-or-matching-dir-path? [this ^Path dir-path]
  (nil-or-matching-dir-path? [this dir-path]
    (or
      (nil? dir-path)
      (matching-dir-path? this dir-path)))

;  (matching-file-name-by-hidden? [this ^String file-name]
  (matching-file-name-by-hidden? [this file-name]
    (or
      (:include-hidden (:settings this))
      (not (hidden? file-name))))

;  (matching-file-path-by-hidden? [this ^Path file-path]
  (matching-file-path-by-hidden? [this file-path]
    (or
      (:include-hidden (:settings this))
      (not (hidden-file-path? file-path))))

;  (matching-archive-ext? [this ^String ext]
  (matching-archive-ext? [this ext]
    (and
      (empty-or-matches-any-elem? ext (:in-archive-extensions (:settings this)))
      (empty-or-not-matches-any-elem? ext (:out-archive-extensions (:settings this)))))

;  (has-matching-archive-ext? [this ^FileResult fr]
  (has-matching-archive-ext? [this fr]
    (or
      (and
        (empty? (:in-archive-extensions (:settings this)))
        (empty? (:out-archive-extensions (:settings this))))
      (matching-archive-ext? this (get-path-ext (:path fr)))))

;  (matching-archive-file-name-by-in-patterns? [this ^String file-name]
  (matching-archive-file-name-by-in-patterns? [this file-name]
    (empty-or-matches-any-pattern? file-name (:in-archive-file-patterns (:settings this))))

;  (matching-archive-file-name-by-out-patterns? [this ^String file-name]
  (matching-archive-file-name-by-out-patterns? [this file-name]
    (empty-or-not-matches-any-pattern? file-name (:out-archive-file-patterns (:settings this))))

;  (matching-archive-file-name? [this ^String file-name]
  (matching-archive-file-name? [this file-name]
    (and
      (matching-file-name-by-hidden? this file-name)
      (matching-archive-ext? this (get-ext file-name))
      (matching-archive-file-name-by-in-patterns? this file-name)
      (matching-archive-file-name-by-out-patterns? this file-name)))

;  (matching-archive-file-path-by-in-patterns? [this ^Path file-path]
  (matching-archive-file-path-by-in-patterns? [this file-path]
    (matching-archive-file-name-by-in-patterns? this (get-path-name file-path)))

;  (matching-archive-file-path-by-out-patterns? [this ^Path file-path]
  (matching-archive-file-path-by-out-patterns? [this file-path]
    (matching-archive-file-name-by-out-patterns? this (get-path-name file-path)))

;  (matching-archive-file-path? [this ^Path file-path]
  (matching-archive-file-path? [this file-path]
    (and
      (matching-file-path-by-hidden? this file-path)
      (matching-archive-ext? this (get-path-ext file-path))
      (matching-archive-file-path-by-in-patterns? this file-path)
      (matching-archive-file-path-by-out-patterns? this file-path)))

;  (matching-archive-file-result? [this ^FileResult fr]
  (matching-archive-file-result? [this fr]
    (and
      (matching-dir-path? this (file-result-parent fr))
      (has-matching-archive-ext? this fr)
      (matching-archive-file-name? this (get-path-name (:path fr)))))

;  (matching-ext? [this ^String ext]
  (matching-ext? [this ext]
    (and
      (empty-or-matches-any-elem? ext (:in-extensions (:settings this)))
      (empty-or-not-matches-any-elem? ext (:out-extensions (:settings this)))))

;  (has-matching-ext? [this ^FileResult fr]
  (has-matching-ext? [this fr]
    (or
      (and
        (empty? (:in-extensions (:settings this)))
        (empty? (:out-extensions (:settings this))))
      (matching-ext? this (get-path-ext (:path fr)))))

  ;  (matching-file-name-by-in-patterns? [this ^String file-name]
  (matching-file-name-by-in-patterns? [this file-name]
    (empty-or-matches-any-pattern? file-name (:in-file-patterns (:settings this))))

  ;  (matching-file-name-by-out-patterns? [this ^String file-name]
  (matching-file-name-by-out-patterns? [this file-name]
    (empty-or-not-matches-any-pattern? file-name (:out-file-patterns (:settings this))))

  ;  (matching-file-name? [this ^String file-name]
  (matching-file-name? [this file-name]
    (and
     (matching-file-name-by-hidden? this file-name)
     (matching-ext? this (get-ext file-name))
     (matching-file-name-by-in-patterns? this file-name)
     (matching-file-name-by-out-patterns? this file-name)))

;  (matching-file-path-by-in-patterns? [this ^Path file-path]
  (matching-file-path-by-in-patterns? [this file-path]
    (matching-file-name-by-in-patterns? this (get-path-name file-path)))

;  (matching-file-path-by-out-patterns? [this ^Path file-path]
  (matching-file-path-by-out-patterns? [this file-path]
    (matching-file-name-by-out-patterns? this (get-path-name file-path)))

;  (matching-file-path? [this ^Path file-path]
  (matching-file-path? [this file-path]
    (and
      (matching-file-path-by-hidden? this file-path)
      (matching-ext? this (get-path-ext file-path))
      (matching-file-path-by-in-patterns? this file-path)
      (matching-file-path-by-out-patterns? this file-path)))

  (matching-file-type? [this file-type]
    (and
      (empty-or-matches-any-elem? file-type (:in-file-types (:settings this)))
      (empty-or-not-matches-any-elem? file-type (:out-file-types (:settings this)))))

;  (matching-file-size? [this ^long file-size]
  (matching-file-size? [this file-size]
    (and
      (or
        (= 0 (:max-size (:settings this)))
        (<= file-size (:max-size (:settings this))))
      (or
        (= 0 (:min-size (:settings this)))
        (>= file-size (:min-size (:settings this))))))

;  (matching-last-mod? [this ^java.nio.file.attribute.FileTime last-mod]
  (matching-last-mod? [this last-mod]
    (and
      (or
        (nil? (:max-last-mod (:settings this)))
        (<= (file-time-to-millis last-mod) (date-to-millis (:max-last-mod (:settings this)))))
      (or
        (nil? (:min-last-mod (:settings this)))
        (>= (file-time-to-millis last-mod) (date-to-millis (:min-last-mod (:settings this)))))))

;  (matching-file-result? [this ^FileResult fr]
  (matching-file-result? [this fr]
    (and
      (matching-dir-path? this (file-result-parent fr))
      (has-matching-ext? this fr)
;      (matching-file-name? (get-path-name (:path fr)) (:settings this))
      (matching-file-path? this (:path fr))
      (matching-file-type? this (:file-type fr))
      (matching-file-size? this (:file-size fr))
      (matching-last-mod? this (:last-mod fr))))

;  (filter-archive-file-path-to-file-result [this ^Path file-path]
  (filter-archive-file-path-to-file-result [this file-path]
    (if
      (and
        (or
          (:include-archives (:settings this))
          (:archives-only (:settings this)))
        (matching-archive-file-path? this file-path))
      (new-file-result file-path :archive 0 nil)
      nil))

;  (filter-regular-file-path-to-file-result [this ^Path file-path file-type]
  (filter-regular-file-path-to-file-result [this file-path file-type]
    (if
      (and
        (not (:archives-only (:settings this)))
        (matching-file-path? this file-path)
        (matching-file-type? this file-type))
      (let [stat (if (or (need-last-mod (:settings this)) (need-size (:settings this))) (get-stat file-path) nil)
            file-size (if (need-size (:settings this)) (.size stat) 0)
            last-mod (if (need-last-mod (:settings this)) (.lastModifiedTime stat) nil)]
        (if
          (and
            (matching-file-size? this file-size)
            (matching-last-mod? this last-mod))
          (new-file-result file-path file-type file-size last-mod)
          nil))
      nil))

;  (filter-to-file-result [this ^Path file-path]
  (filter-to-file-result [this file-path]
    (if
      (and
        (nil-or-matching-dir-path? this (path-parent file-path))
        (matching-file-path-by-hidden? this file-path))
      (let [file-type (get-file-type file-path)]
        (if
          (= :archive file-type)
            (filter-archive-file-path-to-file-result this file-path)
            (filter-regular-file-path-to-file-result this file-path file-type)))
      nil))

;  (get-file-results-under-path [this ^Path path ^Integer min-depth ^Integer max-depth ^Integer current-depth]
  (get-file-results-under-path [this path min-depth max-depth current-depth]
    (if
      (and
        (> max-depth -1)
        (> current-depth max-depth))
      []
      (let [path-elems (filter #(or (not (symlink-path? %)) (:follow-symlinks (:settings this))) (list-paths-under-dir-path path))
            recurse (or (= max-depth -1) (< current-depth max-depth))
            path-dirs (if recurse (filter #(traversable-dir-path? this %) (filter #(dir-path? %) path-elems)) [])
            path-files (if (and (> min-depth -1) (< current-depth min-depth)) [] (filter #(regular-file-path? %) path-elems))
            path-results (filter #(not (nil? %)) (map #(filter-to-file-result this %) path-files))
            next-depth (inc current-depth)]
        (if (empty? path-dirs)
          path-results
          (concat path-results
                  (mapcat #(get-file-results-under-path this % min-depth max-depth next-depth) path-dirs))))))

;  (get-file-results-for-path [this ^Path path]
  (get-file-results-for-path [this path]
    (let [settings (:settings this)
          expanded-path (expand-path path)]
      (if (dir-path? expanded-path)
        (if (= (:max-depth settings) 0)
          []
          (if (:recursive settings)
            (get-file-results-under-path this expanded-path (:min-depth settings) (:max-depth settings) 1)
            (get-file-results-under-path this expanded-path (:min-depth settings) 1 1)))
        (if
          (< (:min-depth (:settings this)) 1)
          (let [path-file-result (filter-to-file-result this expanded-path)]
            (if (not (nil? path-file-result))
              [path-file-result]
              []))
          []))))

  (get-file-results [this]
    (sort-file-results
     (mapcat #(get-file-results-for-path this %) (:paths (:settings this))) (:settings this)))

  (find-files [this]
    (let [errs (validate-settings this)]
      (if (empty? errs)
        [(into [] (get-file-results this)) []]
        [[] errs])))

  (print-matching-dirs [this file-results]
    (let [dirs (get-matching-dirs file-results)]
      (if (empty? dirs)
        (log-msg "\nMatching directories: 0")
        (let [format-dir-path (get-dir-path-formatter (:settings this))]
          (log-msg (format "\nMatching directories (%d):" (count dirs)))
          (doseq [d dirs] (log-msg (format-dir-path d)))))))

  (print-matching-files [this file-results]
    (if (empty? file-results)
      (log-msg "\nMatching files: 0")
      (let [format-file-result (get-file-result-formatter (:settings this))]
        (log-msg (format "\nMatching files (%d):" (count file-results)))
        (doseq [fr file-results] (log-msg (format-file-result fr))))))
)

(defn- settings->finder [settings]
  (->Finder settings))

(extend-type FindSettings
  FinderProtocol
  (validate-settings [settings]
    (validate-settings (settings->finder settings)))
  (get-file-results [settings]
    (get-file-results (settings->finder settings)))
  (find-files [settings]
    (find-files (settings->finder settings)))
  (print-matching-dirs [settings file-results]
    (print-matching-dirs (settings->finder settings) file-results))
  (print-matching-files [settings file-results]
    (print-matching-files (settings->finder settings) file-results)))

(extend-type Path
  FinderProtocol
  (matching-dir-path-by-hidden? [dir-path settings]
    (matching-dir-path-by-hidden? (settings->finder settings) dir-path))
  (matching-dir-path-by-in-patterns? [dir-path settings]
    (matching-dir-path-by-in-patterns? (settings->finder settings) dir-path))
  (matching-dir-path-by-out-patterns? [dir-path settings]
    (matching-dir-path-by-out-patterns? (settings->finder settings) dir-path))
  (traversable-dir-path? [dir-path settings]
    (traversable-dir-path? (settings->finder settings) dir-path))
  (matching-dir-path? [dir-path settings]
    (matching-dir-path? (settings->finder settings) dir-path))
  (nil-or-matching-dir-path? [dir-path settings]
    (nil-or-matching-dir-path? (settings->finder settings) dir-path))
  (matching-file-path-by-hidden? [file-path settings]
    (matching-file-path-by-hidden? (settings->finder settings) file-path))
  (matching-archive-file-path-by-in-patterns? [file-path settings]
    (matching-archive-file-path-by-in-patterns? (settings->finder settings) file-path))
  (matching-archive-file-path-by-out-patterns? [file-path settings]
    (matching-archive-file-path-by-out-patterns? (settings->finder settings) file-path))
  (matching-archive-file-path? [file-path settings]
    (matching-archive-file-path? (settings->finder settings) file-path))
  (matching-file-path-by-in-patterns? [file-path settings]
    (matching-file-path-by-in-patterns? (settings->finder settings) file-path))
  (matching-file-path-by-out-patterns? [file-path settings]
    (matching-file-path-by-out-patterns? (settings->finder settings) file-path))
  (matching-file-path? [file-path settings]
    (matching-file-path? (settings->finder settings) file-path))
  (filter-archive-file-path-to-file-result [file-path settings]
    (filter-archive-file-path-to-file-result (settings->finder settings) file-path))
  (filter-to-file-result [file-path settings]
    (filter-to-file-result (settings->finder settings) file-path))
  (get-file-results-under-path [path settings min-depth max-depth current-depth]
    (get-file-results-under-path (settings->finder settings) path min-depth max-depth current-depth))
  (get-file-results-for-path [path settings]
    (get-file-results-for-path (settings->finder settings) path)))

(extend-type FileResult
  FinderProtocol
  (has-matching-archive-ext? [fr settings]
    (has-matching-archive-ext? (settings->finder settings) fr))
  (matching-archive-file-result? [fr settings]
    (matching-archive-file-result? (settings->finder settings) fr))
  (has-matching-ext? [fr settings]
    (has-matching-ext? (settings->finder settings) fr))
  (matching-file-result? [fr settings]
    (matching-file-result? (settings->finder settings) fr)))

(extend-type String
  FinderProtocol
  (matching-file-name-by-hidden? [file-name settings]
    (matching-file-name-by-hidden? (settings->finder settings) file-name))
  (matching-archive-ext? [ext settings]
    (matching-archive-ext? (settings->finder settings) ext))
  (matching-archive-file-name-by-in-patterns? [file-name settings]
    (matching-archive-file-name-by-in-patterns? (settings->finder settings) file-name))
  (matching-archive-file-name-by-out-patterns? [file-name settings]
    (matching-archive-file-name-by-out-patterns? (settings->finder settings) file-name))
  (matching-archive-file-name? [file-name settings]
    (matching-archive-file-name? (settings->finder settings) file-name))
  (matching-ext? [ext settings]
    (matching-ext? (settings->finder settings) ext))
  (matching-file-name-by-in-patterns? [file-name settings]
    (matching-file-name-by-in-patterns? (settings->finder settings) file-name))
  (matching-file-name-by-out-patterns? [file-name settings]
    (matching-file-name-by-out-patterns? (settings->finder settings) file-name))
  (matching-file-name? [file-name settings]
    (matching-file-name? (settings->finder settings) file-name)))

(extend-type clojure.lang.Keyword
  FinderProtocol
  (matching-file-type? [file-type settings]
    (matching-file-type? (settings->finder settings) file-type)))

(extend-type Number
  FinderProtocol
  (matching-file-size? [file-size settings]
    (matching-file-size? (settings->finder settings) file-size)))

(extend-type java.nio.file.attribute.FileTime
  FinderProtocol
  (matching-last-mod? [last-mod settings]
    (matching-last-mod? (settings->finder settings) last-mod)))

(extend-protocol FinderProtocol
  nil
  (nil-or-matching-dir-path? [_ _]
    true))

