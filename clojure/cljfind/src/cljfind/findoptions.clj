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
  (:import (java.io File))
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json])
  (:use [clojure.set :only (union)]
        [clojure.string :as str :only (lower-case)]
        [cljfind.common :only (log-msg)]
        [cljfind.fileutil :only (expand-path)]
        [cljfind.findsettings :only
         (->FindSettings DEFAULT-SETTINGS add-extension add-file-type add-path
            add-pattern set-archives-only set-debug set-num sort-by-from-name)]))

(defrecord FindOption [short-arg long-arg desc])

(defn get-sortarg [^FindOption fo]
  (if (= "" (:short-arg fo))
    (:long-arg fo)
    (str (str/lower-case (:short-arg fo)) "a" (:long-arg fo))))

(defn get-findoptions-from-json []
  (let [contents (slurp (io/resource "findoptions.json"))
        findoptions-objs (:findoptions (json/read-str contents :key-fn keyword))
        longnames (map #(get % :long) findoptions-objs)
        shortnames (map #(get % :short "") findoptions-objs)
        longshortmap (zipmap longnames shortnames)
        descs (map #(.trim %) (map :desc findoptions-objs))
        longdescmap (zipmap longnames descs)
        get-short (fn [l] (get longshortmap l))
        get-desc (fn [l] (get longdescmap l))]
    (sort-by get-sortarg (map #(FindOption. (get-short %) % (get-desc %)) longnames))))

(def OPTIONS (get-findoptions-from-json))

(defn print-option [opt]
  (let [format-string "(FindOption short=\"%s\" long=\"%s\" desc=\"%s\")"]
    (println
      (format format-string (:short-arg opt) (:long-arg opt) (:desc opt)))))

(defn print-options []
  (doseq [o OPTIONS] (print-option o)))

(def arg-action-map
  { :in-archiveext (fn [settings s] (add-extension settings s :in-archive-extensions))
    :in-archivefilepattern (fn [settings s] (add-pattern settings s :in-archive-file-patterns))
    :in-dirpattern (fn [settings s] (add-pattern settings s :in-dir-patterns))
    :in-ext (fn [settings s] (add-extension settings s :in-extensions))
    :in-filepattern (fn [settings s] (add-pattern settings s :in-file-patterns))
    :in-filetype (fn [settings s] (add-file-type settings s :in-file-types))
    :maxlastmod (fn [settings s] (assoc settings :max-last-mod (clojure.instant/read-instant-date s)))
    :maxsize (fn [settings s] (assoc settings :max-size (Integer/parseInt s)))
    :minlastmod (fn [settings s] (assoc settings :min-last-mod (clojure.instant/read-instant-date s)))
    :minsize (fn [settings s] (assoc settings :min-size (Integer/parseInt s)))
    :out-archiveext (fn [settings s] (add-extension settings s :out-archive-extensions))
    :out-archivefilepattern (fn [settings s] (add-pattern settings s :out-archive-file-pattern))
    :out-dirpattern (fn [settings s] (add-pattern settings s :out-dir-patterns))
    :out-ext (fn [settings s]  (add-extension settings s :out-extensions))
    :out-filepattern (fn [settings s] (add-pattern settings s :out-file-patterns))
    :out-filetype (fn [settings s] (add-file-type settings s :out-file-types))
    :path (fn [settings s] (add-path settings s))
    :sort-by (fn [settings s] (assoc settings :sort-by (sort-by-from-name s)))
  })

(def bool-flag-action-map
  { :archivesonly (fn [settings b] (set-archives-only settings b))
    :debug (fn [settings b] (set-debug settings b))
    :excludearchives (fn [settings b] (assoc settings :include-archives (not b)))
    :excludehidden (fn [settings b] (assoc settings :exclude-hidden b))
    :help (fn [settings b] (assoc settings :print-usage b))
    :includearchives (fn [settings b] (assoc settings :include-archives b))
    :includehidden (fn [settings b] (assoc settings :exclude-hidden (not b)))
    :listdirs (fn [settings b] (assoc settings :list-dirs b))
    :listfiles (fn [settings b] (assoc settings :list-files b))
    :norecursive (fn [settings b] (assoc settings :recursive (not b)))
    :recursive (fn [settings b] (assoc settings :recursive b))
    :sort-ascending (fn [settings b] (assoc settings :sort-descending (not b)))
    :sort-caseinsensitive (fn [settings b] (assoc settings :sort-case-insensitive b))
    :sort-casesensitive (fn [settings b] (assoc settings :sort-case-insensitive (not b)))
    :sort-descending (fn [settings b] (assoc settings :sort-descending b))
    :verbose (fn [settings b] (assoc settings :verbose b))
    :version (fn [settings b] (assoc settings :version b))
  })

(defn get-long-arg [^String arg]
  (let [longnames (map :long-arg OPTIONS)
        longmap (zipmap longnames (repeat 1))
        shortoptions (remove #(= (:short-arg %) "") OPTIONS)
        shortlongmap (zipmap (map :short-arg shortoptions) (map :long-arg shortoptions))]
    (cond
      (contains? longmap arg) (keyword arg)
      (contains? shortlongmap arg) (keyword (get shortlongmap arg))
      :else nil)))

(defn settings-from-map [settings ks m errs]
  (if (empty? ks)
    [settings errs]
    (let [k (keyword (first ks))
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
  ([json]
    (settings-from-json DEFAULT-SETTINGS json))
  ([settings json]
    (let [obj (json/read-str json :key-fn keyword)
          ks (keys obj)]
      (settings-from-map settings ks obj []))))

(defn settings-from-file [settings f]
  (let [contents (slurp f)]
    (settings-from-json settings contents)))

(defn settings-from-args
  ([args]
    ;; default list-files to true since running as cli
    (settings-from-args (assoc DEFAULT-SETTINGS :list-files true) args []))
  ([settings args errs]
    (if (or (empty? args) (not (empty? errs)))
      [settings errs]
      (let [arg (first args)
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

(defn usage []
  (log-msg "" (usage-string) "")
  (System/exit 0))
