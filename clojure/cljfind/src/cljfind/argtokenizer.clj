;;; ############################################################################
;;;
;;; argtokenizer.clj
;;;
;;; Tokenizer for command-line arguments, arg-maps, json strings and files
;;;
;;; ############################################################################

(ns cljfind.argtokenizer
  #^{:author "Cary Clark",
     :doc "Defines tokenizer for command-line arguments, arg-maps, json strings and files"}
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json])
  (:import (java.io File)
           (java.nio.file Paths Path Files))
  (:use [clojure.instant :only (read-instant-date)]
        [clojure.java.io :only (file)]
        [clojure.string :as str :only (blank? lower-case)]
        [cljfind.common :only (as-keyword as-string log-msg)]
        [cljfind.fileutil :only (exists-path? expand-path path-str to-path)]))

(def arg-match-patterns
  [
    [:long-arg-with-val #"^--(\S+)=(.*)$"]
    [:long-arg-no-val #"^--(\S+)$"]
    [:short-args #"^-([^\-\s])(\S+)$"]
    [:short-arg #"^-([^\-\s])$"]
  ])

(defn get-arg-matches
  ([^String arg]
   (get-arg-matches arg arg-match-patterns))
  ([^String arg match-patterns]
   (if (empty? match-patterns)
     [:no-match []]
     (let [[match-type pattern] (first match-patterns)
           matches (re-matches pattern arg)]
       (if (> (count matches) 0)
         [match-type matches]
         (get-arg-matches arg (rest match-patterns)))))))

(defrecord ArgToken [name type value])

(defprotocol ArgTokenizerProtocol
  (get-arg-type [this arg])
  (get-long-arg [this arg])
  (get-long-arg-for-arg-and-type [this arg arg-type])
  (get-value-for-string-and-type [this str-val arg-type])
  (tokenize-args [this args] [this args tokens errs])
  (tokenize-arg-map [this arg-map] [this arg-map arg-keys tokens errs])
  (tokenize-json [this json-str])
  (tokenize-file [this f])
  )

(defrecord ArgTokenizer [bool-map string-map int-map long-map]
  ArgTokenizerProtocol
  (get-arg-type [this arg]
    (let [kwarg (as-keyword arg)]
      (cond
        (contains? (:bool-map this) kwarg) :bool
        (contains? (:string-map this) kwarg) :string
        (contains? (:int-map this) kwarg) :int
        (contains? (:long-map this) kwarg) :long
        :else :unknown)))

  (get-long-arg-for-arg-and-type [this arg arg-type]
    (let [kwarg (as-keyword arg)]
      (case arg-type
        :bool (get (:bool-map this) kwarg)
        :string (get (:string-map this) kwarg)
        :int (get (:int-map this) kwarg)
        :long (get (:long-map this) kwarg)
        nil)))

  (get-long-arg [this arg]
    (get-long-arg-for-arg-and-type this arg (get-arg-type this arg)))

  (get-value-for-string-and-type [this str-val arg-type]
    (case arg-type
      :bool [(Boolean/parseBoolean str-val) nil]
      :string [str-val nil]
      :int (try [(Integer/parseInt str-val) nil]
             (catch Exception e [str-val "Invalid integer value"]))
      :long (try [(Long/parseLong str-val) nil]
              (catch Exception e [str-val "Invalid long value"]))
      [str-val "Unknown argument type"]))

  (tokenize-args [this args]
    (tokenize-args this args [] []))

  (tokenize-args [this args tokens errs]
    (if (or (empty? args) (not (empty? errs)))
      [tokens errs]
      (let [arg (first args)
            [match-type matches] (get-arg-matches arg)]

        (case match-type
          :long-arg-with-val
          (let [arg-name (second matches)
                arg-type (get-arg-type this arg-name)]
            (if (= :unknown arg-type)
              (tokenize-args this [] tokens [(str "Invalid option: " arg-name)])
              (let [long-name (get-long-arg-for-arg-and-type this arg-name arg-type)
                    [value verr] (get-value-for-string-and-type this (nth matches 2) arg-type)]
                (if (nil? verr)
                  (tokenize-args this (rest args) (conj tokens (->ArgToken long-name arg-type value)) [])
                  (tokenize-args this [] tokens [verr])))))

          :long-arg-no-val
          (let [arg-name (second matches)
                arg-type (get-arg-type this arg-name)]
            (if (= :unknown arg-type)
              (tokenize-args this [] tokens [(str "Invalid option: " name)])
              (let [long-name (get-long-arg-for-arg-and-type this arg-name arg-type)]
                (if (= arg-type :bool)
                  (tokenize-args this (rest args) (conj tokens (->ArgToken long-name arg-type true)) [])
                  (if (empty? (rest args))
                    (tokenize-args this [] tokens [(str "Missing arg for option " arg-name)])
                    (let [next-arg (second args)
                          [value verr] (get-value-for-string-and-type this next-arg arg-type)]
                      (if (nil? verr)
                        (tokenize-args this (drop 2 args) (conj tokens (->ArgToken long-name arg-type value)) [])
                        (tokenize-args this [] tokens [verr]))))))))

          :short-args
          (let [a (second matches)
                long-name (get-long-arg this a)
                as (nth matches 2)]
            (if (nil? long-name)
              (tokenize-args this [] tokens [(str "Invalid option: " a)])
              (tokenize-args this (concat [(str "--" (as-string long-name)) (str "-" as)] (rest args)) tokens [])))

          :short-arg
          (let [a (second matches)
                long-name (get-long-arg this a)]
            (if (nil? long-name)
              (tokenize-args this [] tokens [(str "Invalid option: " a)])
              (tokenize-args this (concat [(str "--" (as-string long-name))] (rest args)) tokens [])))

          ;; :no-match - treat as path
          (tokenize-args this (rest args) (conj tokens (->ArgToken :path :string arg)) [])))))

  (tokenize-arg-map [this arg-map]
    (tokenize-arg-map this arg-map (sort (keys arg-map)) [] []))

  (tokenize-arg-map [this arg-map arg-keys tokens errs]
    (if (or (empty? arg-keys) (not (empty? errs)))
      [tokens errs]
      (let [k (as-keyword (first arg-keys))
            s (as-string (first arg-keys))
            v (k arg-map)
            arg-type (get-arg-type this k)]
        (case arg-type
          :bool
          (if (boolean? v)
            (tokenize-arg-map this arg-map (rest arg-keys) (conj tokens (->ArgToken k :bool v)) errs)
            (tokenize-arg-map this arg-map [] tokens [(str "Invalid value for option: " (name k))]))

          :string
          (if (string? v)
            (tokenize-arg-map this arg-map (rest arg-keys) (conj tokens (->ArgToken k :string v)) errs)
            (if (coll? v)
              (if (empty? v)
                (tokenize-arg-map this arg-map (rest arg-keys) tokens errs)
                (tokenize-arg-map this (assoc arg-map k (rest v)) arg-keys (conj tokens (->ArgToken k :string (first v))) errs))
              (tokenize-arg-map this arg-map [] tokens [(str "Invalid value for option: " (name k))])))

          :int
          (if (int? v)
            (tokenize-arg-map this arg-map (rest arg-keys) (conj tokens (->ArgToken k :int v)) errs)
            (tokenize-arg-map this arg-map [] tokens [(str "Invalid value for option: " (name k))]))

          :long
          (if (integer? v)
            (tokenize-arg-map this arg-map (rest arg-keys) (conj tokens (->ArgToken k :long v)) errs)
            (tokenize-arg-map this arg-map [] tokens [(str "Invalid value for option: " (name k))]))

          ;; :unknown
          (tokenize-arg-map this arg-map [] tokens [(str "Invalid option: " (name k))])))))

  (tokenize-json [this json-str]
    (let [obj (json/read-str json-str :key-fn keyword)]
      (tokenize-arg-map this obj)))

  (tokenize-file [this f]
    (let [filepath (expand-path (to-path f))
          pathstr (path-str filepath)]
      (if (not (exists-path? filepath))
        [[] [(str "Settings file not found: " pathstr)]]
        (if (not (.endsWith pathstr ".json"))
          [[] [(str "Invalid settings file (must be JSON): " pathstr)]]
          (try
            (tokenize-json this (slurp pathstr))
            (catch Exception e
              [[] [(.getMessage e)]]))))))

  ) ;; end ArgTokenizer record

(defn get-map-for-options-and-type [options arg-type]
  (let [opts-of-type (filter #(= (:arg-type %) arg-type) options)
        long-map (reduce (fn [m o] (assoc m (as-keyword (:long-arg o)) (as-keyword (:long-arg o)))) {} opts-of-type)
        short-options (filter #(not (str/blank? (:short-arg %))) opts-of-type)
        short-map (reduce (fn [m o] (assoc m (as-keyword (:short-arg o)) (as-keyword (:long-arg o)))) {} short-options)]
    (merge long-map short-map)))

(defn get-arg-tokenizer-for-options [options]
  (let [bool-map (get-map-for-options-and-type options :bool)
        string-map (get-map-for-options-and-type options :string)
        int-map (get-map-for-options-and-type options :int)
        long-map (get-map-for-options-and-type options :long)]
    (->ArgTokenizer bool-map string-map int-map long-map)))
