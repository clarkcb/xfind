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
  (:use [clojure.set :only (union)]
        [clojure.string :as str :only (split lower-case)]
        [cljfind.filetypes :only (from-name)]))

;; sort-by names
(def FILEPATH "path")
(def FILENAME "name")
(def FILESIZE "size")
(def FILETYPE "type")
(def LASTMOD "lastmod")

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
      (= FILENAME lname) :filename
      (= FILESIZE lname) :filesize
      (= FILETYPE lname) :filetype
      (= LASTMOD lname) :lastmod
      :else :filepath)))

(defrecord FindSettings
  [
    archives-only
    debug
    exclude-hidden
    include-archives
    in-archive-extensions
    in-archive-file-patterns
    in-dir-patterns
    in-extensions
    in-file-patterns
    in-file-types
    list-dirs
    list-files
    max-depth
    max-last-mod
    max-size
    min-depth
    min-last-mod
    min-size
    out-archive-extensions
    out-archive-patterns
    out-dir-patterns
    out-extensions
    out-file-patterns
    out-file-types
    paths
    print-usage
    print-version
    sort-by
    sort-case-insensitive
    sort-descending
    recursive
    verbose
  ])

(def DEFAULT-SETTINGS
  (->FindSettings
   false     ; archives-only
   false     ; debug
   true      ; exclude-hidden
   false     ; include-archives
   #{}       ; in-archive-extensions
   #{}       ; in-archive-file-patterns
   #{}       ; in-dir-patterns
   #{}       ; in-extensions
   #{}       ; in-file-patterns
   #{}       ; in-file-types
   false     ; list-dirs
   false     ; list-files
   -1        ; max-depth
   nil       ; max-last-mod
   0         ; max-size
   -1        ; min-depth
   nil       ; min-last-mod
   0         ; min-size
   #{}       ; out-archive-extensions
   #{}       ; out-archive-patterns
   #{}       ; out-dir-patterns
   #{}       ; out-extensions
   #{}       ; out-file-patterns
   #{}       ; out-file-types
   #{}       ; paths
   false     ; print-usage
   false     ; print-version
   :filepath ; sort-by
   false     ; sort-case-insensitive
   false     ; sort-descending
   true      ; recursive
   false     ; verbose
   ))

(defn add-element [x coll]
  (conj coll x))

(defn add-extensions [^FindSettings settings exts extname]
  (if (empty? exts)
    settings
    (add-extensions
      (update-in settings [extname] #(add-element (first exts) %)) (rest exts) extname)))

(defn add-extension [settings ext extname]
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
     (update-in settings [typesname] #(add-element (from-name (first types)) %)) (rest types) typesname)))

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
      (update-in settings [:paths] #(add-element (first paths) %)) (rest paths))))

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
      (update-in settings [patname] #(add-element (re-pattern (first pats)) %)) (rest pats) patname)))

(defn add-pattern [^FindSettings settings p patname]
  (let [t (type p)]
    (cond
      (= t (type []))
        (add-patterns settings p patname)
      :else
        (add-patterns settings [p] patname))))

(defn need-stat [^FindSettings settings]
  (or
   (= :filesize (:sort-by settings))
   (= :lastmod (:sort-by settings))
   (not (nil? (:max-last-mod settings)))
   (not (nil? (:min-last-mod settings)))
   (> (:max-size settings) 0)
   (> (:min-size settings) 0)))

(defn set-num [^FindSettings settings n numname]
  (let [t (type n)]
    (cond
      (= t java.lang.Long)
        (assoc settings numname n)
      :else
        (assoc settings numname (read-string n)))))

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
