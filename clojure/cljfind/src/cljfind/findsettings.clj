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
    archivesonly
    debug
    excludehidden
    includearchives
    in-archiveextensions
    in-archivefilepatterns
    in-dirpatterns
    in-extensions
    in-filepatterns
    in-filetypes
    listdirs
    listfiles
    maxlastmod
    maxsize
    minlastmod
    minsize
    out-archiveextensions
    out-archivefilepatterns
    out-dirpatterns
    out-extensions
    out-filepatterns
    out-filetypes
    paths
    printusage
    printversion
    sort-by
    sort-caseinsensitive
    sort-descending
    recursive
    verbose
  ])

(def DEFAULT-SETTINGS
  (->FindSettings
   false     ; archivesonly
   false     ; debug
   true      ; excludehidden
   false     ; includearchives
   #{}       ; in-archiveextensions
   #{}       ; in-archivefilepatterns
   #{}       ; in-dirpatterns
   #{}       ; in-extensions
   #{}       ; in-filepatterns
   #{}       ; in-filetypes
   false     ; listdirs
   false     ; listfiles
   nil       ; maxlastmod
   0         ; maxsize
   nil       ; minlastmod
   0         ; minsize
   #{}       ; out-archiveextensions
   #{}       ; out-archivefilepatterns
   #{}       ; out-dirpatterns
   #{}       ; out-extensions
   #{}       ; out-filepatterns
   #{}       ; out-filetypes
   #{}       ; paths
   false     ; printusage
   false     ; printversion
   :filepath ; sort-by
   false     ; sort-caseinsensitive
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

(defn add-filetypes [^FindSettings settings types typesname]
  (if (empty? types)
    settings
    (add-filetypes
     (update-in settings [typesname] #(add-element (from-name (first types)) %)) (rest types) typesname)))

(defn add-filetype [^FindSettings settings typ typesname]
  (let [t (type typ)]
    (cond
      (= t (type []))
      (add-filetypes settings typ typesname)
      :else
      (add-filetypes settings (str/split typ #",") typesname))))

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
   (not (nil? (:maxlastmod settings)))
   (not (nil? (:minlastmod settings)))
   (> (:maxsize settings) 0)
   (> (:minsize settings) 0)))

(defn set-num [^FindSettings settings n numname]
  (let [t (type n)]
    (cond
      (= t java.lang.Long)
        (assoc settings numname n)
      :else
        (assoc settings numname (read-string n)))))

(defn set-archivesonly [^FindSettings settings b]
  (let [with-archivesonly (assoc settings :archivesonly b)]
    (if b
      (assoc with-archivesonly :includearchives true)
      with-archivesonly)))

(defn set-debug [^FindSettings settings b]
  (let [with-debug (assoc settings :debug true)]
    (if b
      (assoc with-debug :verbose true)
      with-debug)))
