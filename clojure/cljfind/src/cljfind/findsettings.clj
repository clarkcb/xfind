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
  (:require [clojure.string :as str]
            ;; [java-time :as jt]
            )
  (:use [clojure.set :only (union)]
        [clojure.string :as str :only (split lower-case)]
        [cljfind.consolecolor]
        [cljfind.filetypes :only (from-name to-name)]
        [cljfind.sortby]))

(defrecord FindSettings
  [
    ^Boolean archives-only
    ^Boolean colorize
    ^Boolean debug
    ^Boolean default-files
    ^clojure.lang.Keyword dir-color
    ^clojure.lang.Keyword ext-color
    ^clojure.lang.Keyword file-color
    ^Boolean follow-symlinks
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

(def ^:const DEFAULT-FIND-SETTINGS
  (->FindSettings
   false     ; archives-only
   true      ; colorize
   false     ; debug
   true      ; default-files
   :cyan     ; dir-color
   :yellow   ; ext-color
   :magenta  ; file-color
   false     ; follow-symlinks
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
  (let [with-debug (assoc settings :debug b)]
    (if b
      (assoc with-debug :verbose true)
      with-debug)))

(defn set-int-val [^FindSettings settings n numname]
  (let [t (type n)]
    (cond
      (= t (type ""))
      (assoc settings numname (Integer/parseInt n))
      :else
      (assoc settings numname n))))

(defn set-long-val [^FindSettings settings n numname]
  (let [t (type n)]
    (cond
      (= t (type ""))
      (assoc settings numname (Long/parseLong n))
      :else
      (assoc settings numname n))))

(defn string-set-to-string ^String [ss]
  (if (empty? ss)
    "[]"
    (str "[\"" (str/join "\", \"" (sort ss)) "\"]")))

(defn pattern-set-to-string ^String [ps]
   (if (empty? ps)
     "[]"
     (str "[\"" (str/join "\", \"" (sort (map #(.pattern ^java.util.regex.Pattern %) ps))) "\"]")))

(defn filetype-set-to-string ^String [fts]
  (if (empty? fts)
    "[]"
    (str "[" (str/join ", " (sort (map #(to-name %) fts))) "]")))

(defmulti setting-to-string
  (fn [k v] (type v)))

(defmethod setting-to-string clojure.lang.PersistentHashSet [k v]
  (let [n (name k)]
    (cond
      (.endsWith n "patterns") (str n "=" (pattern-set-to-string v))
      (.endsWith n "file-types") (str n "=" (filetype-set-to-string v))
      :else (str n "=" (string-set-to-string v)))))

(defmethod setting-to-string clojure.lang.Keyword [k v]
  (str (name k) "=" (name v)))

;(defmethod setting-to-string java.util.Date [k v]
;  (str (name k) "=" (jt/format (jt/formatter "YYYY-MM-dd") (.toInstant v))))

(defmethod setting-to-string java.util.Date [k v]
  (str (name k) "=" \" v \"))

(defmethod setting-to-string :default [k v]
  (if (nil? v)
    (str (name k) "=0")
    (str (name k) "=" v)))

(defn settings-to-string ^String [settings]
  (let [ks (sort (keys settings))
        ss (map #(setting-to-string % (% settings)) ks)]
    (str "FindSettings(" (str/join ", " ss) ")")))

(defmethod print-method FindSettings
  [^FindSettings settings ^java.io.Writer w]
  (.write w (settings-to-string settings)))
