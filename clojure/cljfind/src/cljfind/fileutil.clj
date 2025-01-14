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

(defn path-str ^String [p]
  (cond
    (instance? Path p) (.toString p)
    (instance? File p) (.getPath p)
    :else p))

(defn to-path [^String f & rest]
  (let [rest' (if (nil? rest) (into-array String []) (into-array String rest))]
    (Paths/get f rest')))

(defn expand-path ^Path [^Path p]
  (let [path-str (.toString p)
        sep-index (.indexOf path-str File/separator)
        user-str (System/getProperty "user.home")
        user-path (to-path user-str)
        home-path (.getParent user-path)
        home-str (if (nil? home-path) "." (.toString home-path))]
    (if (.startsWith path-str "~")
      (if (or (= path-str "~") (= path-str (str "~" File/separator)))
        user-path
        (if (= sep-index 1)
          (to-path user-str (.substring path-str 2))
          (to-path home-str (.substring path-str 1))))
      p)))

(defn exists-path? [^Path f]
  (Files/exists f (into-array LinkOption [])))

(defn readable-path? [^Path f]
  (Files/isReadable f))

(defn get-parent-name ^String [^Path f]
  (let [parent (.getParent f)]
    (if (nil? parent) "" (.toString parent))))

(defn get-name ^String [f]
  (cond
    (instance? Path f) (.toString (.getFileName f))
    (instance? File f) (.getName f)
    (instance? java.util.zip.ZipEntry f) (.getName f)
    (instance? java.util.jar.JarEntry f) (.getName f)
    :else f))

(defn get-path-name ^String [^Path f]
  (.toString (.getFileName f)))

(defn get-ext ^String [f]
  (let [name (get-name f)
        dotindex (.lastIndexOf name ".")]
    (if
      (and
       (> dotindex 0)
       (< dotindex (- (.length name) 1)))
      (.toLowerCase (peek (split name #"\.")))
      "")))

(defn get-path-ext ^String [^Path f]
  (get-ext (.toString (.getFileName f))))

(defn has-ext? [f ^String ext]
  (= (.toLowerCase ext) (get-ext f)))

(defn is-dir-path? [^Path d]
  (Files/isDirectory d (into-array LinkOption [])))

(defn is-file-path? [^Path f]
  (Files/isRegularFile f (into-array LinkOption [])))

(defn is-symlink-path? [^Path f]
  (Files/isSymbolicLink f))

(defn is-dot-dir? [^String name]
  (contains? DOT_DIRS name))

(defn split-path [p]
  (split (path-str p) (re-pattern File/separator)))

(defn hidden? [^String name]
  (and
    (.startsWith name ".")
    (not (is-dot-dir? name))))

(defn hidden-dir-path? [^Path d]
  (let [elems (split-path d)]
    (some #(hidden? %) elems)))

(defn hidden-file-path? [^Path f]
  (hidden? (.toString (.getFileName f))))
