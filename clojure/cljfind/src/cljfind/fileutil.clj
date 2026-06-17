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

(def ^:private ^"[Ljava.nio.file.LinkOption;" NO_LINK_OPTIONS
  (into-array LinkOption []))

(defn path->string ^String [^Path p]
  (.toString p))

(defn file->path-string ^String [^File f]
  (.getPath f))

(defn parent-path ^Path [^Path p]
  (.getParent p))

(defn file-name-path ^Path [^Path p]
  (.getFileName p))

(defn path-str ^String [p]
  (cond
    (instance? Path p) (path->string ^Path p)
    (instance? File p) (file->path-string ^File p)
    :else p))

(defn to-path ^Path [^String f & rest]
  (let [rest' (if (nil? rest) (into-array String []) (into-array String rest))]
    (Paths/get f rest')))

(defn expand-path ^Path [^Path p]
  (let [path-str (.toString p)
        sep-index (.indexOf path-str File/separator)
        user-str (System/getProperty "user.home")
        user-path (to-path user-str)
        home-path (parent-path user-path)
        home-str (if (nil? home-path) "." (path->string home-path))]
    (if (.startsWith path-str "~")
      (if (or (= path-str "~") (= path-str (str "~" File/separator)))
        user-path
        (if (= sep-index 1)
          (to-path user-str (.substring path-str 2))
          (to-path home-str (.substring path-str 1))))
      p)))

(defn exists-path? [^Path f]
  (Files/exists f NO_LINK_OPTIONS))

(defn readable-path? [^Path f]
  (Files/isReadable f))

(defn get-parent-name ^String [^Path f]
  (let [parent (parent-path f)]
    (if (nil? parent) "" (path->string parent))))

(defn get-name ^String [f]
  (cond
    (instance? Path f) (path->string (file-name-path ^Path f))
    (instance? File f) (.getName ^File f)
    (instance? java.util.zip.ZipEntry f) (.getName ^java.util.zip.ZipEntry f)
    (instance? java.util.jar.JarEntry f) (.getName ^java.util.jar.JarEntry f)
    :else f))

(defn get-path-name ^String [^Path f]
  (path->string (file-name-path f)))

(defn get-ext ^String [f]
  (let [^String name (str (get-name f))
        dotindex (.lastIndexOf name ".")]
    (if
      (and
       (> dotindex 0)
       (< dotindex (- (.length name) 1)))
      (.toLowerCase ^String (peek (split name #"\.")))
      "")))

(defn get-path-ext ^String [^Path f]
  (get-ext (path->string (file-name-path f))))

(defn has-ext? [f ^String ext]
  (= (.toLowerCase ext) (get-ext f)))

(defn dir-path? [^Path d]
  (Files/isDirectory d NO_LINK_OPTIONS))

(defn regular-file-path? [^Path f]
  (Files/isRegularFile f NO_LINK_OPTIONS))

(defn symlink-path? [^Path f]
  (Files/isSymbolicLink f))

(defn dot-dir? [^String name]
  (contains? DOT_DIRS name))

(defn split-path [p]
  (split (path-str p) (re-pattern File/separator)))

(defn hidden? [^String name]
  (and
   (.startsWith name ".")
   (not (dot-dir? name))))

(defn hidden-dir-path? [^Path d]
  (let [elems (split-path d)]
    (some #(hidden? %) elems)))

(defn hidden-file-path? [^Path f]
  (hidden? (path->string (file-name-path f))))
