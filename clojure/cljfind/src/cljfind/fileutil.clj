;;; ############################################################################
;;;
;;; fileutil.clj
;;;
;;; File-related utility functions
;;;
;;; ############################################################################

(ns cljfind.fileutil
  #^{:author "Cary Clark",
     :doc "Module to provide file-related utility functions"}
  (:import (java.io File)
           (java.nio.file Files Path Paths LinkOption))
  (:use [clojure.string :only (split)]))

(def ^:const DOT_DIRS #{"." ".."})

; needs string argument and returns string
(defn expand-path [^String f]
  (let [home (File. (System/getProperty "user.home"))]
    (if (.startsWith f "~")
      (str home (.substring f 1))
      f)))

(defn exists? [f]
  (cond
    (instance? java.nio.file.Path f) (Files/exists f (into-array LinkOption []))
    (instance? java.io.File f) (.exists f)
    :else false))

(defn get-parent ^String [f]
  (cond
    (instance? java.nio.file.Path f) (if (nil? (.getParent f)) "" (.toString (.getParent f)))
    (instance? java.io.File f) (.getParent f)
    (instance? java.util.zip.ZipEntry f) (.getParent f)
    (instance? java.util.jar.JarEntry f) (.getParent f)
    :else f))

(defn get-name ^String [f]
  (cond
    (instance? java.nio.file.Path f) (.toString (.getFileName f))
    (instance? java.io.File f) (.getName f)
    (instance? java.util.zip.ZipEntry f) (.getName f)
    (instance? java.util.jar.JarEntry f) (.getName f)
    :else f))

(defn get-ext [f]
  (let [name (get-name f)
        dotindex (.lastIndexOf name ".")]
    (if
      (and
       (> dotindex 0)
       (< dotindex (- (.length name) 1)))
      (.toLowerCase (peek (split name #"\.")))
      "")))

(defn has-ext? [f ^String ext]
  (= (.toLowerCase ext) (get-ext f)))

(defn is-dir? [d]
  (cond
    (instance? java.nio.file.Path d) (Files/isDirectory d (into-array LinkOption []))
    (instance? java.io.File d) (.isDirectory d)))

(defn is-file? [f]
  (cond
    (instance? java.nio.file.Path f) (Files/isRegularFile f (into-array LinkOption []))
    (instance? java.io.File f) (.isFile f)))

(defn is-dot-dir? [^String name]
  (contains? DOT_DIRS name))

(defn path-str [p]
  (cond (instance? java.nio.file.Path p) (.toString p)
        (instance? java.io.File p) (.getPath p)
        :else p))

(defn to-path [^String f]
  (Paths/get f (into-array String [])))

(defn split-path [p]
  (split (path-str p) (re-pattern File/separator)))

(defn hidden? [^String name]
  (and
    (.startsWith name ".")
    (not (is-dot-dir? name))))

(defn hidden-dir? [d]
  (let [elems (split-path d)]
    (some #(hidden? %) elems)))

(defn hidden-file? [f]
  (hidden? (get-name f)))

(defn sep-count [^String f]
  (count (filter #(= % java.io.File/separatorChar) f)))
