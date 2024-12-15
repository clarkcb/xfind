(ns cljfind.fileutil-test
  (:use [clojure.java.io :only (file)])
  (:import (java.io File)
           (java.nio.file Files Path Paths LinkOption))
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.fileutil :only
         (expand-path get-ext has-ext? hidden? is-dot-dir? split-path to-path)]))

(deftest test-expand-path
  (let [user-str (System/getProperty "user.home")
        user-path (to-path user-str)
        home-path (.getParent user-path)
        home-str (if (nil? home-path) "." (.toString home-path))
        ]
    (testing "test-expand-path"
      (is (= (expand-path (to-path "~")) user-path))
      (is (= (expand-path (to-path "~/")) user-path))
      (is (= (expand-path (to-path "filename.txt")) (to-path "filename.txt")))
      (is (= (expand-path (to-path "./filename.txt")) (to-path "./filename.txt")))
      (is (= (expand-path (to-path "/filename.txt")) (to-path "/filename.txt")))
      (is (= (expand-path (to-path "~/filename.txt")) (to-path user-str "filename.txt")))
      (is (= (expand-path (to-path "~cary")) (to-path home-str "cary")))
      (is (= (expand-path (to-path "~cary/")) (to-path home-str "cary")))
      (is (= (expand-path (to-path "~cary/filename.txt")) (to-path home-str "cary" "filename.txt"))))))

(deftest test-get-ext
  (testing "test-get-ext"
    (is (= (get-ext "filename.txt") "txt"))
    (is (= (get-ext "filename.") ""))
    (is (= (get-ext "filename") ""))
    (is (= (get-ext ".filename.txt") "txt"))
    (is (= (get-ext ".filename.") ""))
    (is (= (get-ext ".filename") ""))))

(deftest test-has-ext
  (testing "test-has-ext"
    (is (not (has-ext? "filename.txt" "xyz")))
    (is (has-ext? "filename.txt" "txt"))
    (is (has-ext? "filename." ""))
    (is (has-ext? "filename" ""))
    (is (has-ext? ".filename.txt" "txt"))
    (is (has-ext? ".filename." ""))
    (is (has-ext? ".filename" ""))))

(deftest test-hidden?
  (testing "test-hidden?"
    (is (not (hidden? "filename.txt")))
    (is (not (hidden? ".")))
    (is (not (hidden? "..")))
    (is (hidden? ".filename.txt"))))

(deftest test-is-dot-dir?
  (testing "test-is-dot-dir?"
    (is (is-dot-dir? "."))
    (is (is-dot-dir? ".."))))

(deftest test-split-path
  (testing "test-split-path"
    (let [elems (split-path (file "~/path/to/nowhere"))]
      (is (= (first elems) "~"))
      (is (= (second elems) "path")))))
