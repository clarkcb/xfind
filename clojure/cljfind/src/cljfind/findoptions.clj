;;; ############################################################################
;;;
;;; findoptions.clj
;;;
;;; Defines the available command-line options and utility functions
;;;
;;; ############################################################################

(ns cljfind.findoptions
  #^{:author "Cary Clark",
     :doc "Defines the available command-line options and utility functions"}
  (:require [cljfind.findsettings]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json])
  (:import (java.io File)
           (java.nio.file Paths Path Files)
           (cljfind.findsettings FindSettings))
  (:use [clojure.instant :only (read-instant-date)]
        [clojure.java.io :only (file)]
        [clojure.set :only (union)]
        [clojure.string :as str :only (lower-case)]
        [cljfind.common :only (log-msg)]
        [cljfind.fileutil :only (exists-path? expand-path path-str to-path)]
        [cljfind.findsettings :only
         (->FindSettings DEFAULT-FIND-SETTINGS add-extension add-file-type add-path
                         add-pattern set-archives-only set-debug set-int-val set-long-val
                         sort-by-from-name)]))

(defrecord FindOption [short-arg long-arg desc])

(defn get-sort-arg ^String [^FindOption fo]
  (if (= "" (:short-arg fo))
    (:long-arg fo)
    (str (str/lower-case (:short-arg fo)) "a" (:long-arg fo))))

(defn get-find-options-from-json []
  (let [contents (slurp (io/resource "findoptions.json"))
        find-options-objs (:findoptions (json/read-str contents :key-fn keyword))
        long-names (map #(get % :long) find-options-objs)
        short-names (map #(get % :short "") find-options-objs)
        long-short-map (zipmap long-names short-names)
        descs (map #(.trim %) (map :desc find-options-objs))
        long-desc-map (zipmap long-names descs)
        get-short (fn [l] (get long-short-map l))
        get-desc (fn [l] (get long-desc-map l))]
    (sort-by get-sort-arg (map #(FindOption. (get-short %) % (get-desc %)) long-names))))

(def ^:const FIND-OPTIONS (get-find-options-from-json))

(defn print-option [^FindOption opt]
  (let [format-string "(FindOption short=\"%s\" long=\"%s\" desc=\"%s\")"]
    (println
      (format format-string (:short-arg opt) (:long-arg opt) (:desc opt)))))

(defn print-options []
  (doseq [o FIND-OPTIONS] (print-option o)))

(def bool-action-map
  { :archivesonly (fn [^FindSettings settings b] (set-archives-only settings b))
    :colorize (fn [^FindSettings settings b] (assoc settings :colorize b))
    :debug (fn [^FindSettings settings b] (set-debug settings b))
    :excludearchives (fn [^FindSettings settings b] (assoc settings :include-archives (not b)))
    :excludehidden (fn [^FindSettings settings b] (assoc settings :include-hidden (not b)))
    :followsymlinks (fn [^FindSettings settings b] (assoc settings :follow-symlinks b))
    :help (fn [^FindSettings settings b] (assoc settings :print-usage b))
    :includearchives (fn [^FindSettings settings b] (assoc settings :include-archives b))
    :includehidden (fn [^FindSettings settings b] (assoc settings :include-hidden b))
    :nocolorize (fn [^FindSettings settings b] (assoc settings :colorize (not b)))
    :nofollowsymlinks (fn [^FindSettings settings b] (assoc settings :follow-symlinks (not b)))
    :noprintdirs (fn [^FindSettings settings b] (assoc settings :print-dirs (not b)))
    :noprintfiles (fn [^FindSettings settings b] (assoc settings :print-files (not b)))
    :norecursive (fn [^FindSettings settings b] (assoc settings :recursive (not b)))
    :printdirs (fn [^FindSettings settings b] (assoc settings :print-dirs b))
    :printfiles (fn [^FindSettings settings b] (assoc settings :print-files b))
    :recursive (fn [^FindSettings settings b] (assoc settings :recursive b))
    :sort-ascending (fn [^FindSettings settings b] (assoc settings :sort-descending (not b)))
    :sort-caseinsensitive (fn [^FindSettings settings b] (assoc settings :sort-case-insensitive b))
    :sort-casesensitive (fn [^FindSettings settings b] (assoc settings :sort-case-insensitive (not b)))
    :sort-descending (fn [^FindSettings settings b] (assoc settings :sort-descending b))
    :verbose (fn [^FindSettings settings b] (assoc settings :verbose b))
    :version (fn [^FindSettings settings b] (assoc settings :version b))
  })

(def string-action-map
  { :in-archiveext (fn [^FindSettings settings ^String s] (add-extension settings s :in-archive-extensions))
    :in-archivefilepattern (fn [^FindSettings settings ^String s] (add-pattern settings s :in-archive-file-patterns))
    :in-dirpattern (fn [^FindSettings settings ^String s] (add-pattern settings s :in-dir-patterns))
    :in-ext (fn [^FindSettings settings ^String s] (add-extension settings s :in-extensions))
    :in-filepattern (fn [^FindSettings settings ^String s] (add-pattern settings s :in-file-patterns))
    :in-filetype (fn [^FindSettings settings ^String s] (add-file-type settings s :in-file-types))
    :maxlastmod (fn [^FindSettings settings ^String s] (assoc settings :max-last-mod (read-instant-date s)))
    :minlastmod (fn [^FindSettings settings ^String s] (assoc settings :min-last-mod (read-instant-date s)))
    :out-archiveext (fn [^FindSettings settings ^String s] (add-extension settings s :out-archive-extensions))
    :out-archivefilepattern (fn [^FindSettings settings ^String s] (add-pattern settings s :out-archive-file-pattern))
    :out-dirpattern (fn [^FindSettings settings ^String s] (add-pattern settings s :out-dir-patterns))
    :out-ext (fn [^FindSettings settings ^String s]  (add-extension settings s :out-extensions))
    :out-filepattern (fn [^FindSettings settings ^String s] (add-pattern settings s :out-file-patterns))
    :out-filetype (fn [^FindSettings settings ^String s] (add-file-type settings s :out-file-types))
    :path (fn [^FindSettings settings ^String s] (add-path settings (to-path s)))
    :sort-by (fn [^FindSettings settings ^String s] (assoc settings :sort-by (sort-by-from-name s)))
  })

(def int-action-map
  { :maxdepth (fn [^FindSettings settings i] (set-int-val settings i :max-depth))
    :mindepth (fn [^FindSettings settings i] (set-int-val settings i :min-depth))
  })

(def long-action-map
  { :maxsize (fn [^FindSettings settings l] (set-long-val settings l :max-size))
    :minsize (fn [^FindSettings settings l] (set-long-val settings l :min-size))
  })

(defn get-long-arg-map []
  (let [long-names     (concat (map :long-arg FIND-OPTIONS) ["path"])
        long-map       (zipmap long-names (map #(keyword %) long-names))
        short-options  (remove #(= (:short-arg %) "") FIND-OPTIONS)
        short-long-map (zipmap (map :short-arg short-options) (map #(keyword %) (map :long-arg short-options)))]
    (merge long-map short-long-map)))

(declare settings-from-file)

(defn settings-from-arg-map
  ([^FindSettings settings long-arg-map arg-map errs]
    (let [ks (sort (keys arg-map))
          invalid-ks (remove #(contains? long-arg-map %) (map #(name %) ks))]
      (if (not (empty? invalid-ks))
        [nil [(str "Invalid option: " (first invalid-ks))]]
        (settings-from-arg-map settings long-arg-map ks arg-map errs))))
  ([^FindSettings settings long-arg-map ks arg-map errs]
    (if (or (empty? ks) (not (empty? errs)))
      [settings errs]
      (let [k (keyword (first ks))
            v (k arg-map)]
        (cond
          (contains? bool-action-map k)
            (if (instance? Boolean v)
              (settings-from-arg-map ((k bool-action-map) settings v) long-arg-map (rest ks) arg-map errs)
              (settings-from-arg-map settings long-arg-map (rest ks) arg-map (conj errs (str "Invalid value for option: " (name k)))))
          (contains? string-action-map k)
            (if (instance? String v)
              (settings-from-arg-map ((k string-action-map) settings v) long-arg-map (rest ks) arg-map errs)
              (if (coll? v)
                (if (empty? v)
                  (settings-from-arg-map settings long-arg-map (rest ks) arg-map errs)
                  (settings-from-arg-map ((k string-action-map) settings (first v)) long-arg-map ks (assoc arg-map k (rest v)) errs))
                (settings-from-arg-map settings long-arg-map (rest ks) arg-map (conj errs (str "Invalid value for option: " (name k))))))
          (contains? int-action-map k)
            (if (instance? Integer v)
              (settings-from-arg-map ((k int-action-map) settings v) long-arg-map (rest ks) arg-map errs)
              (if (instance? Long v)
                (settings-from-arg-map ((k int-action-map) settings (.intValue v)) long-arg-map (rest ks) arg-map errs)
                (if (instance? String v)
                  (settings-from-arg-map ((k int-action-map) settings (Integer/parseInt v)) long-arg-map (rest ks) arg-map errs)
                  (settings-from-arg-map settings long-arg-map (rest ks) arg-map (conj errs (str "Invalid value for option: " (name k)))))))
          (contains? long-action-map k)
            (if (instance? Long v)
              (settings-from-arg-map ((k long-action-map) settings v) long-arg-map (rest ks) arg-map errs)
              (if (instance? Integer v)
                (settings-from-arg-map ((k long-action-map) settings (.longValue v)) long-arg-map (rest ks) arg-map errs)
                (if (instance? String v)
                  (settings-from-arg-map ((k long-action-map) settings (Long/parseLong v)) long-arg-map (rest ks) arg-map errs)
                  (settings-from-arg-map settings long-arg-map (rest ks) arg-map (conj errs (str "Invalid value for option: " (name k)))))))
          (= k :settings-file)
            (if (instance? String v)
              (let [[new-settings new-errs] (settings-from-file settings long-arg-map v)]
                (if (empty? new-errs)
                  (settings-from-arg-map new-settings long-arg-map (rest ks) arg-map errs)
                  (settings-from-arg-map settings long-arg-map (rest ks) arg-map (conj errs (str "Error reading settings file: " v) new-errs)))))
          :else
            (settings-from-arg-map settings long-arg-map (rest ks) arg-map (conj errs (str "Invalid option: " (name k)))))))))

(defn settings-from-json
  ([^String json]
    (settings-from-json DEFAULT-FIND-SETTINGS (get-long-arg-map) json))
  ([^FindSettings settings long-arg-map ^String json]
   (let [obj (json/read-str json :key-fn keyword)]
      (settings-from-arg-map settings long-arg-map obj []))))

(defn settings-from-file
  ([f]
    (settings-from-file DEFAULT-FIND-SETTINGS (get-long-arg-map) f))
  ([^FindSettings settings long-arg-map f]
    (let [filepath (expand-path (to-path f))
          pathstr (path-str filepath)]
      (if (not (exists-path? filepath))
        [settings [(str "Settings file not found: " pathstr)]]
        (if (not (.endsWith pathstr ".json"))
          [settings [(str "Invalid settings file (must be JSON): " pathstr)]]
          (try
            (settings-from-json settings long-arg-map (slurp pathstr))
            (catch Exception e
              [settings [(str "Unable to parse JSON in settings file: " pathstr)]])))))))

(defn get-arg-map-from-args
  ([long-arg-map args]
    (get-arg-map-from-args long-arg-map args {} []))
  ([long-arg-map args arg-map errs]
    (defn proc-arg [long-arg-map arg long-arg arg-val args arg-map errs]
      (cond
        ;; 1) no long-arg
        (not long-arg)
        (get-arg-map-from-args long-arg-map (rest args) arg-map (conj errs (str "Invalid option: " arg)))

        ;; 2) boolean option
        (contains? bool-action-map long-arg)
        (get-arg-map-from-args long-arg-map (rest args) (assoc arg-map long-arg true) errs)

        ;; 3) option without arg
        (not arg-val)
        (get-arg-map-from-args long-arg-map (rest args) arg-map (conj errs (str "Missing arg for option " arg)))

        ;; 4) string option
        (contains? string-action-map long-arg)
        (get-arg-map-from-args long-arg-map (drop 2 args) (assoc arg-map long-arg (conj (get arg-map long-arg []) arg-val)) errs)

        ;; 5) int option
        (contains? int-action-map long-arg)
        (if (instance? String arg-val)
          (get-arg-map-from-args long-arg-map (drop 2 args) (assoc arg-map long-arg (Integer/parseInt arg-val)) errs)
          (get-arg-map-from-args long-arg-map (drop 2 args) (assoc arg-map long-arg arg-val) errs))

        ;; 6) long option
        (contains? long-action-map long-arg)
        (if (instance? String arg-val)
          (get-arg-map-from-args long-arg-map (drop 2 args) (assoc arg-map long-arg (Long/parseLong arg-val)) errs)
          (get-arg-map-from-args long-arg-map (drop 2 args) (assoc arg-map long-arg arg-val) errs))

        ;; 7) settings-file option
        (= long-arg :settings-file)
        (get-arg-map-from-args long-arg-map (drop 2 args) (assoc arg-map long-arg arg-val) errs)

        :else
        (get-arg-map-from-args long-arg-map (rest args) (conj errs (str "Invalid option: " arg)))))
    (if (or (empty? args) (not (empty? errs)))
      [arg-map errs]
      (let [arg (first args)
            arg-val (second args)
            long-arg-with-val (re-matches #"^--([a-zA-Z0-9\-]+)=(.*)$" arg)
            long-arg-no-val (re-matches #"^--([a-zA-Z0-9\-]+)$" arg)
            short-args (re-matches #"^-([a-zA-Z0-9])([a-zA-Z0-9]+)$" arg)
            short-arg (re-matches #"^-([a-zA-Z0-9])$" arg)]
        (cond
          ;; 1) match long arg with = and arg-val
          (> (count long-arg-with-val) 2)
          (let [a (second long-arg-with-val)
                k (get long-arg-map a)
                v (nth long-arg-with-val 2)]
            (if k
              (get-arg-map-from-args long-arg-map (rest args) (assoc arg-map k v) errs)
              (get-arg-map-from-args long-arg-map (rest args) arg-map (conj errs (str "Invalid option: " a)))))

          ;; 2) match long arg
          (> (count long-arg-no-val) 1)
          (let [a (second long-arg-no-val)
                k (get long-arg-map a)]
            (proc-arg long-arg-map a k arg-val args arg-map errs))

          ;; 3) match short args
          (> (count short-args) 2)
          (let [a (second short-args)
                as (nth short-args 2)
                k (get long-arg-map a)]
            (proc-arg long-arg-map a k arg-val (concat [arg (str "-" as)] (rest args)) arg-map errs))

          ;; 4) match short arg
          (> (count short-arg) 1)
          (let [a (second short-arg)
                k (get long-arg-map a)]
            (proc-arg long-arg-map a k arg-val args arg-map errs))

          ;; 5) no match, treat as path
          :else
          (get-arg-map-from-args long-arg-map (rest args) (assoc arg-map :path (conj (get arg-map :path []) arg)) errs))))))

(defn settings-from-args [args]
  ;; default print-files to true since running as cli
   (let [initial-settings (assoc DEFAULT-FIND-SETTINGS :print-files true)
         long-arg-map (get-long-arg-map)
         arg-map-with-errs (get-arg-map-from-args long-arg-map args)
         arg-map (first arg-map-with-errs)
         errs (second arg-map-with-errs)]
    (if (not (empty? errs))
      [nil errs]
      (settings-from-arg-map initial-settings long-arg-map arg-map []))))

(defn longest-length [options]
  (let [lens (map #(+ (count (:long-arg %)) (if (:short-arg %) 3 0)) options)]
    (apply max lens)))

(defn option-to-string ^String [^FindOption opt longest]
  (let [s (:short-arg opt)
        l (:long-arg opt)
        d (:desc opt)
        short-string (if (not (empty? s)) (str "-" s ",") "")
        opt-string (str short-string "--" l)]
    (format (str "%-" longest "s %s") opt-string d)))

(defn usage-string ^String []
  (let [longest (longest-length FIND-OPTIONS)]
    (str
     "Usage:\n"
     " cljfind [options] <path> [<path> ...]\n\n"
     "Options:\n "
     (str/join "\n " (map #(option-to-string % longest) FIND-OPTIONS)))))

(defn usage
  ([exit-code]
    (log-msg "" (usage-string) "")
    (System/exit exit-code))
  ([]
   (usage 0)))
