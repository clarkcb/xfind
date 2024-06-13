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
  (:import (java.io File))
  (:use [clojure.string :only (split)]))

(def ^:const DOT_DIRS #{"." ".."})

; needs string argument and returns string
(defn expand-path [f]
  (let [home (File. (System/getProperty "user.home"))]
    (if (.startsWith f "~")
      (str home (.substring f 1))
      f)))

(defn get-name [f]
  (if (or (instance? java.io.File f)
          (instance? java.util.zip.ZipEntry f)
          (instance? java.util.jar.JarEntry f))
    (.getName f)
    f))

(defn get-ext [^File f]
  (let [name (get-name f)
        dotindex (.lastIndexOf name ".")]
    (if 
      (and
        (> dotindex 0)
        (< dotindex (- (.length name) 1)))
      (.toLowerCase (peek (split name #"\.")))
      "")))

(defn get-files-in-directory [^File d]
  (filter #(.isFile %) (.listFiles d)))

(defn has-ext? [^File f ^String ext]
  (= (.toLowerCase ext) (get-ext f)))

(defn is-dot-dir? [^String name]
  (contains? DOT_DIRS name))

(defn split-path [^File f]
  (split (.getPath f) (re-pattern File/separator)))

(defn hidden? [^String name]
  (and
    (.startsWith name ".")
    (not (is-dot-dir? name))))

(defn hidden-dir? [^File d]
  (let [elems (split-path d)]
    (some #(hidden? %) elems)))

(defn hidden-file? [^File f]
  (hidden? (get-name f)))

(defn sep-count [^String f]
  (count (filter #(= % java.io.File/separatorChar) f)))
