(ns cljfind.findfile-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.findfile :only (new-find-file find-file-path)]))

(deftest test-findfile-abs-path
  (let [filepath "~/src/xfind/clojure/cljfind/src/cljfind/findfile.clj"
        findfile (new-find-file (file filepath) :code)]
    (testing "test-findfile-abs-path"
             (is (= (find-file-path findfile) filepath)))))

(deftest test-findfile-rel-path-1
  (let [filepath "./findfile.clj"
        findfile (new-find-file (file filepath) :code)]
    (testing "test-findfile-rel-path-1"
             (is (= (find-file-path findfile) filepath)))))

(deftest test-findfile-rel-path-2
  (let [filepath "../findfile.clj"
        findfile (new-find-file (file filepath) :code)]
    (testing "test-findfile-rel-path-2"
             (is (= (find-file-path findfile) filepath)))))
