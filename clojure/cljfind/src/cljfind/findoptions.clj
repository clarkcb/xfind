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
        [cljfind.common :only (as-keyword as-string log-msg)]
        [cljfind.argtokenizer]
        [cljfind.fileutil :only (exists-path? expand-path path-str to-path)]
        [cljfind.findsettings :only
          (->FindSettings DEFAULT-FIND-SETTINGS add-extension add-file-type add-path
           add-pattern set-archives-only set-debug set-int-val set-long-val
           sort-by-from-name)]))

(defrecord FindOption [short-arg long-arg desc arg-type])

(defn get-sort-arg ^String [^FindOption fo]
  (if (= "" (:short-arg fo))
    (:long-arg fo)
    (str (str/lower-case (:short-arg fo)) "a" (:long-arg fo))))

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

(defn get-action-arg-type [^String long-arg]
  (let [long-key (as-keyword long-arg)]
    (cond
      (contains? (set (keys bool-action-map)) long-key) :bool
      (contains? (set (keys string-action-map)) long-key) :string
      (contains? (set (keys int-action-map)) long-key) :int
      (contains? (set (keys long-action-map)) long-key) :long
      (= long-key :path) :string
      (= long-key :settings-file) :string
      :else :unknown)))

(defn get-find-options-from-json []
  (let [contents (slurp (io/resource "findoptions.json"))
        find-options-objs (:findoptions (json/read-str contents :key-fn keyword))]
    (map #(->FindOption (get % :short "") (get % :long) (get % :desc) (get-action-arg-type (get % :long))) find-options-objs)))

(def ^:const FIND-OPTIONS (get-find-options-from-json))

(def arg-tokenizer (get-arg-tokenizer-for-options FIND-OPTIONS))

(defn print-option [^FindOption opt]
  (let [format-string "(FindOption short=\"%s\" long=\"%s\" desc=\"%s\")"]
    (println
      (format format-string (:short-arg opt) (:long-arg opt) (:desc opt)))))

(defn print-options []
  (doseq [o FIND-OPTIONS] (print-option o)))

(defn get-long-arg-map []
  (let [long-names     (concat (map :long-arg FIND-OPTIONS) ["path"])
        long-map       (zipmap long-names (map #(keyword %) long-names))
        short-options  (remove #(= (:short-arg %) "") FIND-OPTIONS)
        short-long-map (zipmap (map :short-arg short-options) (map #(keyword %) (map :long-arg short-options)))]
    (merge long-map short-long-map)))

(declare update-settings-from-file)

(defn update-settings-from-tokens
  ([^FindSettings settings tokens]
    (update-settings-from-tokens settings tokens []))
  ([^FindSettings settings tokens errs]
    (if (or (empty? tokens) (not (empty? errs)))
      [settings errs]
      (let [token (first tokens)
            name (:name token)
            arg-type (:type token)
            value (:value token)]
        (case arg-type
          :bool
            (cond
              (not (contains? bool-action-map name))
                (update-settings-from-tokens settings [] [(str "Invalid option: " name)])
              (not (boolean? value))
                (update-settings-from-tokens settings [] [(str "Invalid value option: " name)])
              :else
                (update-settings-from-tokens ((name bool-action-map) settings value) (rest tokens) errs))
          :string
            (cond
              (and (not (contains? string-action-map name)) (not (= name :settings-file)))
                (update-settings-from-tokens settings [] [(str "Invalid option: " name)])
              (not (string? value))
                (update-settings-from-tokens settings [] [(str "Invalid value option: " name)])
              :else
                (if (= name :settings-file)
                  (let [[new-settings new-errs] (update-settings-from-file settings value)]
                    (if (empty? new-errs)
                      (update-settings-from-tokens new-settings (rest tokens) errs)
                      (update-settings-from-tokens settings (rest tokens) (concat errs new-errs))))
                  (update-settings-from-tokens ((name string-action-map) settings value) (rest tokens) errs)))
          :int
            (cond
              (not (contains? int-action-map name))
                (update-settings-from-tokens settings [] [(str "Invalid option: " name)])
              (not (int? value))
                (update-settings-from-tokens settings [] [(str "Invalid value option: " name)])
              :else
                (update-settings-from-tokens ((name int-action-map) settings value) (rest tokens) errs))
          :long
            (cond
              (not (contains? long-action-map name))
                (update-settings-from-tokens settings [] [(str "Invalid option: " name)])
              (not (integer? value))
                (update-settings-from-tokens settings [] [(str "Invalid value option: " name)])
              :else
                (update-settings-from-tokens ((name long-action-map) settings value) (rest tokens) errs))
          (update-settings-from-tokens settings [] [(str "Unknown token type: " name arg-type)]))))))

(defn settings-from-tokens [tokens]
  (update-settings-from-tokens DEFAULT-FIND-SETTINGS tokens))

(defn update-settings-from-arg-map [^FindSettings settings arg-map]
  (let [[tokens errs] (tokenize-arg-map arg-tokenizer arg-map)]
    (if (not (empty? errs))
      [settings errs]
      (update-settings-from-tokens settings tokens))))

(defn settings-from-arg-map [arg-map]
  (update-settings-from-arg-map DEFAULT-FIND-SETTINGS (get-long-arg-map) arg-map))

(defn update-settings-from-json [^FindSettings settings ^String json]
  (let [[tokens errs] (tokenize-json arg-tokenizer json)]
    (if (not (empty? errs))
      [settings errs]
      (update-settings-from-tokens settings tokens))))

(defn settings-from-json [^String json]
  (update-settings-from-json DEFAULT-FIND-SETTINGS json))

(defn update-settings-from-file [^FindSettings settings f]
  (let [[tokens errs] (tokenize-file arg-tokenizer f)]
    (if (not (empty? errs))
      [settings errs]
      (update-settings-from-tokens settings tokens))))

(defn settings-from-file [f]
  (update-settings-from-file DEFAULT-FIND-SETTINGS (get-long-arg-map) f))

(defn update-settings-from-args [^FindSettings settings args]
  (let [[tokens errs] (tokenize-args arg-tokenizer args)]
    (if (not (empty? errs))
      [settings errs]
      (update-settings-from-tokens settings tokens))))

(defn settings-from-args [args]
  ;; default print-files to true since running as cli
  (update-settings-from-args (assoc DEFAULT-FIND-SETTINGS :print-files true) args))

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
  (let [options (sort-by get-sort-arg FIND-OPTIONS)
        longest (longest-length options)]
    (str
     "Usage:\n"
     " cljfind [options] <path> [<path> ...]\n\n"
     "Options:\n "
     (str/join "\n " (map #(option-to-string % longest) options)))))

(defn usage
  ([exit-code]
    (log-msg "" (usage-string) "")
    (System/exit exit-code))
  ([]
    (usage 0)))
