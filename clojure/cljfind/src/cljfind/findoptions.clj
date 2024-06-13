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
  (:require [cljfind.findsettings])
  (:import (java.io File)
           (cljfind.findsettings FindSettings))
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json])
  (:use [clojure.instant :only (read-instant-date)]
        [clojure.set :only (union)]
        [clojure.string :as str :only (lower-case)]
        [cljfind.common :only (log-msg)]
        [cljfind.fileutil :only (expand-path)]
        [cljfind.findsettings :only
         (->FindSettings DEFAULT-SETTINGS add-extension add-file-type add-path
            add-pattern set-archives-only set-debug sort-by-from-name)]))

(defrecord FindOption [short-arg long-arg desc])

(defn get-sort-arg [^FindOption fo]
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

(def ^:const OPTIONS (get-find-options-from-json))

(defn print-option [^FindOption opt]
  (let [format-string "(FindOption short=\"%s\" long=\"%s\" desc=\"%s\")"]
    (println
      (format format-string (:short-arg opt) (:long-arg opt) (:desc opt)))))

(defn print-options []
  (doseq [o OPTIONS] (print-option o)))

(def arg-action-map
  { :in-archiveext (fn [^FindSettings settings ^String s] (add-extension settings s :in-archive-extensions))
    :in-archivefilepattern (fn [^FindSettings settings ^String s] (add-pattern settings s :in-archive-file-patterns))
    :in-dirpattern (fn [^FindSettings settings ^String s] (add-pattern settings s :in-dir-patterns))
    :in-ext (fn [^FindSettings settings ^String s] (add-extension settings s :in-extensions))
    :in-filepattern (fn [^FindSettings settings ^String s] (add-pattern settings s :in-file-patterns))
    :in-filetype (fn [^FindSettings settings ^String s] (add-file-type settings s :in-file-types))
    :maxdepth (fn [^FindSettings settings ^String s] (assoc settings :max-depth (Integer/parseInt s)))
    :maxlastmod (fn [^FindSettings settings ^String s] (assoc settings :max-last-mod (read-instant-date s)))
    :maxsize (fn [^FindSettings settings ^String s] (assoc settings :max-size (Integer/parseInt s)))
    :mindepth (fn [^FindSettings settings ^String s] (assoc settings :min-depth (Integer/parseInt s)))
    :minlastmod (fn [^FindSettings settings ^String s] (assoc settings :min-last-mod (read-instant-date s)))
    :minsize (fn [^FindSettings settings ^String s] (assoc settings :min-size (Integer/parseInt s)))
    :out-archiveext (fn [^FindSettings settings ^String s] (add-extension settings s :out-archive-extensions))
    :out-archivefilepattern (fn [^FindSettings settings ^String s] (add-pattern settings s :out-archive-file-pattern))
    :out-dirpattern (fn [^FindSettings settings ^String s] (add-pattern settings s :out-dir-patterns))
    :out-ext (fn [^FindSettings settings ^String s]  (add-extension settings s :out-extensions))
    :out-filepattern (fn [^FindSettings settings ^String s] (add-pattern settings s :out-file-patterns))
    :out-filetype (fn [^FindSettings settings ^String s] (add-file-type settings s :out-file-types))
    :path (fn [^FindSettings settings ^String s] (add-path settings s))
    :sort-by (fn [^FindSettings settings ^String s] (assoc settings :sort-by (sort-by-from-name s)))
  })

(def bool-flag-action-map
  { :archivesonly (fn [^FindSettings settings b] (set-archives-only settings b))
    :debug (fn [^FindSettings settings b] (set-debug settings b))
    :excludearchives (fn [^FindSettings settings b] (assoc settings :include-archives (not b)))
    :excludehidden (fn [^FindSettings settings b] (assoc settings :include-hidden (not b)))
    :help (fn [^FindSettings settings b] (assoc settings :print-usage b))
    :includearchives (fn [^FindSettings settings b] (assoc settings :include-archives b))
    :includehidden (fn [^FindSettings settings b] (assoc settings :include-hidden b))
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

(defn get-long-arg [^String arg]
  (let [long-names     (map :long-arg OPTIONS)
        long-map       (zipmap long-names (repeat 1))
        short-options  (remove #(= (:short-arg %) "") OPTIONS)
        short-long-map (zipmap (map :short-arg short-options) (map :long-arg short-options))]
    (cond
      (contains? long-map arg) (keyword arg)
      (contains? short-long-map arg) (keyword (get short-long-map arg))
      :else nil)))

(defn settings-from-map [^FindSettings settings ks m errs]
  (if (empty? ks)
    [settings errs]
    (let [k (keyword (nth ks 0))
          v (k m)]
      (cond
        (contains? arg-action-map k)
          (settings-from-map ((k arg-action-map) settings v) (rest ks) m errs)
        (contains? bool-flag-action-map k)
          (settings-from-map ((k bool-flag-action-map) settings v) (rest ks) m errs)
        (= k :path)
          (do
            (settings-from-map (add-path settings v) (rest ks) m errs))
        :else
          (settings-from-map settings (rest ks) m (conj errs (str "Invalid option: " k)))))))

(defn settings-from-json
  ([^String json]
    (settings-from-json DEFAULT-SETTINGS json))
  ([^FindSettings settings ^String json]
    (let [obj (json/read-str json :key-fn keyword)
          ks (keys obj)]
      (settings-from-map settings ks obj []))))

(defn settings-from-file [^FindSettings settings ^File f]
  (let [contents (slurp f)]
    (settings-from-json settings contents)))

(defn settings-from-args
  ([args]
    ;; default print-files to true since running as cli
    (settings-from-args (assoc DEFAULT-SETTINGS :print-files true) args []))
  ([^FindSettings settings args errs]
    (if (or (empty? args) (not (empty? errs)))
      [settings errs]
      (let [arg (nth args 0)
            a (if (.startsWith arg "-") (str/replace arg #"^\-+" ""))
            k (if a (get-long-arg a))
            a2 (second args)]
        (if a
          (cond
            (contains? arg-action-map k)
              (if a2
                (settings-from-args ((k arg-action-map) settings a2) (drop 2 args) errs)
                (settings-from-args settings (rest args) (conj errs (str "Missing arg for option " a))))
            (contains? bool-flag-action-map k)
              (settings-from-args ((k bool-flag-action-map) settings true) (rest args) errs)
            (= k :settings-file)
              (let [[file-settings file-errs] (settings-from-file settings a2)]
                (settings-from-args file-settings (drop 2 args) (concat errs file-errs)))
            :else
              (settings-from-args settings (rest args) (conj errs (str "Invalid option: " a))))
          ;;(settings-from-args (assoc settings :startpath arg) (rest args) errs)
          (settings-from-args (add-path settings arg) (rest args) errs))))))

(defn longest-length [options]
  (let [lens (map #(+ (count (:long-arg %)) (if (:short-arg %) 3 0)) options)]
    (apply max lens)))

(defn option-to-string [^FindOption opt longest]
  (let [s (:short-arg opt)
        l (:long-arg opt)
        d (:desc opt)
        short-string (if (not (empty? s)) (str "-" s ",") "")
        opt-string (str short-string "--" l)]
    (format (str "%-" longest "s %s") opt-string d)))

(defn usage-string []
  (let [longest (longest-length OPTIONS)]
    (str
      "Usage:\n"
      " cljfind [options] <path> [<path> ...]\n\n"
      "Options:\n "
      (str/join "\n " (map #(option-to-string % longest) OPTIONS)))))

(defn usage
  ([exit-code]
    (log-msg "" (usage-string) "")
    (System/exit exit-code))
  ([]
   (usage 0)))
