;;; ############################################################################
;;;
;;; findsettings.clj
;;;
;;; Defines the settings for a given find instance
;;;
;;; ############################################################################

(ns cljfind.findsettings
  #^{:author "Cary Clark",
     :doc "Defines the settings for a given find instance"}
  (:require [clojure.string :as str])
  (:use [clojure.set :only (union)]
        [clojure.string :as str :only (split lower-case)]
        [cljfind.filetypes :only (from-name to-name)]))

;; sort-by names
(def ^:const ^String FILEPATH "filepath")
(def ^:const ^String FILENAME "filename")
(def ^:const ^String NAME "name")
(def ^:const ^String FILESIZE "filesize")
(def ^:const ^String SIZE "size")
(def ^:const ^String FILETYPE "filetype")
(def ^:const ^String TYPE "type")
(def ^:const ^String LASTMOD "lastmod")

(defn get-sort-by-name [s]
  (cond
    (= :filename s) FILENAME
    (= :filesize s) FILESIZE
    (= :filetype s) FILETYPE
    (= :lastmod s) LASTMOD
    :else FILEPATH))

(defn sort-by-from-name [^String name]
  (let [lname (lower-case name)]
    (cond
      (or
        (= FILENAME lname)
        (= NAME lname)) :filename
      (or
        (= FILESIZE lname)
        (= SIZE lname)) :filesize
      (or
        (= FILETYPE lname)
        (= TYPE lname)) :filetype
      (= LASTMOD lname) :lastmod
      :else :filepath)))

(defrecord FindSettings
  [
    ^Boolean archives-only
    ^Boolean debug
    ^clojure.lang.PersistentHashSet in-archive-extensions
    ^clojure.lang.PersistentHashSet in-archive-file-patterns
    ^clojure.lang.PersistentHashSet in-dir-patterns
    ^clojure.lang.PersistentHashSet in-extensions
    ^clojure.lang.PersistentHashSet in-file-patterns
    ^clojure.lang.PersistentHashSet in-file-types
    ^Boolean include-archives
    ^Boolean include-hidden
    ^Integer max-depth
    ^java.util.Date max-last-mod
    ^long max-size
    ^Integer min-depth
    ^java.util.Date min-last-mod
    ^long min-size
    ^clojure.lang.PersistentHashSet out-archive-extensions
    ^clojure.lang.PersistentHashSet out-archive-file-patterns
    ^clojure.lang.PersistentHashSet out-dir-patterns
    ^clojure.lang.PersistentHashSet out-extensions
    ^clojure.lang.PersistentHashSet out-file-patterns
    ^clojure.lang.PersistentHashSet out-file-types
    ^clojure.lang.PersistentHashSet paths
    ^Boolean print-dirs
    ^Boolean print-files
    ^Boolean print-usage
    ^Boolean print-version
    ^Boolean recursive
    ^clojure.lang.Keyword sort-by
    ^Boolean sort-case-insensitive
    ^Boolean sort-descending
    ^Boolean verbose
  ])

(def ^:const DEFAULT-SETTINGS
  (->FindSettings
   false     ; archives-only
   false     ; debug
   #{}       ; in-archive-extensions
   #{}       ; in-archive-file-patterns
   #{}       ; in-dir-patterns
   #{}       ; in-extensions
   #{}       ; in-file-patterns
   #{}       ; in-file-types
   false     ; include-archives
   false     ; include-hidden
   -1        ; max-depth
   nil       ; max-last-mod
   0         ; max-size
   -1        ; min-depth
   nil       ; min-last-mod
   0         ; min-size
   #{}       ; out-archive-extensions
   #{}       ; out-archive-file-patterns
   #{}       ; out-dir-patterns
   #{}       ; out-extensions
   #{}       ; out-file-patterns
   #{}       ; out-file-types
   #{}       ; paths
   false     ; print-dirs
   false     ; print-files
   false     ; print-usage
   false     ; print-version
   true      ; recursive
   :filepath ; sort-by
   false     ; sort-case-insensitive
   false     ; sort-descending
   false     ; verbose
   ))

(defn add-element [x coll]
  (conj coll x))

(defn add-extensions [^FindSettings settings exts extname]
  (if (empty? exts)
    settings
    (add-extensions
      (update-in settings [extname] #(add-element (nth exts 0) %)) (rest exts) extname)))

(defn add-extension [^FindSettings settings ext extname]
  (let [t (type ext)]
    (cond
      (= t (type []))
        (add-extensions settings ext extname)
      :else
        (add-extensions settings (str/split ext #",") extname))))

(defn add-file-types [^FindSettings settings types typesname]
  (if (empty? types)
    settings
    (add-file-types
     (update-in settings [typesname] #(add-element (from-name (nth types 0)) %)) (rest types) typesname)))

(defn add-file-type [^FindSettings settings typ typesname]
  (let [t (type typ)]
    (cond
      (= t (type []))
      (add-file-types settings typ typesname)
      :else
      (add-file-types settings (str/split typ #",") typesname))))

(defn add-paths [^FindSettings settings paths]
  (if (empty? paths)
    settings
    (add-paths
      (update-in settings [:paths] #(add-element (nth paths 0) %)) (rest paths))))

(defn add-path [^FindSettings settings path]
  (let [t (type path)]
    (cond
      (= t (type []))
        (add-paths settings path)
      :else
        (add-paths settings [path]))))

(defn add-patterns [^FindSettings settings pats patname]
  (if (empty? pats)
    settings
    (add-patterns
      (update-in settings [patname] #(add-element (re-pattern (nth pats 0)) %)) (rest pats) patname)))

(defn add-pattern [^FindSettings settings p patname]
  (let [t (type p)]
    (cond
      (= t (type []))
        (add-patterns settings p patname)
      :else
        (add-patterns settings [p] patname))))

(defn need-last-mod [^FindSettings settings]
  (or
   (= :lastmod (:sort-by settings))
   (not (nil? (:max-last-mod settings)))
   (not (nil? (:min-last-mod settings)))))

(defn need-size [^FindSettings settings]
  (or
   (= :filesize (:sort-by settings))
   (> (:max-size settings) 0)
   (> (:min-size settings) 0)))

(defn set-archives-only [^FindSettings settings b]
  (let [with-archives-only (assoc settings :archives-only b)]
    (if b
      (assoc with-archives-only :include-archives true)
      with-archives-only)))

(defn set-debug [^FindSettings settings b]
  (let [with-debug (assoc settings :debug true)]
    (if b
      (assoc with-debug :verbose true)
      with-debug)))

(defn string-set-to-string [ss]
  (if (empty? ss)
    "[]"
    (str "[\"" (str/join "\", \"" ss) "\"]")))

(defn pattern-set-to-string [ps]
  (if (empty? ps)
    "[]"
    (str "[\"" (str/join "\", \"" (map #(.pattern %) ps)) "\"]")))

(defn filetype-set-to-string [fts]
  (if (empty? fts)
    "[]"
    (str "[" (str/join ", " (map #(to-name %) fts)) "]")))

(defmethod print-method FindSettings
  [^FindSettings settings ^java.io.Writer w]
  (.write w (str
             "FindSettings(archives-only=" (:archives-only settings)
             ", debug=" (:debug settings)
             ", in-archive-extensions=" (string-set-to-string (:in-archive-extensions settings))
             ", in-archive-file-patterns=" (pattern-set-to-string (:in-archive-file-patterns settings))
             ", in-dir-patterns=" (pattern-set-to-string (:in-dir-patterns settings))
             ", in-extensions=" (string-set-to-string (:in-extensions settings))
             ", in-file-patterns=" (pattern-set-to-string (:in-file-patterns settings))
             ", in-file-types=" (filetype-set-to-string (:in-file-types settings))
             ", include-archives=" (:include-archives settings)
             ", include-hidden=" (:include-hidden settings)
             ", max-depth=" (:max-depth settings)
             ", max-last-mod=" (if (nil? (:max-last-mod settings)) "0" (:max-last-mod settings))
             ", max-size=" (:max-size settings)
             ", min-depth=" (:min-depth settings)
             ", min-last-mod=" (if (nil? (:min-last-mod settings)) "0" (:min-last-mod settings))
             ", min-size=" (:min-size settings)
             ", out-archive-extensions=" (string-set-to-string (:out-archive-extensions settings))
             ", out-archive-file-patterns=" (pattern-set-to-string (:out-archive-file-patterns settings))
             ", out-dir-patterns=" (pattern-set-to-string (:out-dir-patterns settings))
             ", out-extensions=" (string-set-to-string (:out-extensions settings))
             ", out-file-patterns=" (pattern-set-to-string (:out-file-patterns settings))
             ", out-file-types=" (filetype-set-to-string (:out-file-types settings))
             ", paths=" (string-set-to-string (:paths settings))
             ", print-dirs=" (:print-dirs settings)
             ", print-files=" (:print-files settings)
             ", print-usage=" (:print-usage settings)
             ", print-version=" (:print-version settings)
             ", recursive=" (:recursive settings)
             ", sort-by=" (get-sort-by-name (:sort-by settings))
             ", sort-case-insensitive=" (:sort-case-insensitive settings)
             ", sort-descending=" (:sort-descending settings)
             ", verbose=" (:verbose settings)
             ")")))
