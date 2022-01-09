(ns cljfind.fileresult-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.fileresult :only (new-file-result file-result-path)]))

(deftest test-fileresult-abs-path
  (let [filepath "~/src/xfind/clojure/cljfind/src/cljfind/fileresult.clj"
        fileresult (new-file-result (file filepath) :code)]
    (testing "test-fileresult-abs-path"
             (is (= (file-result-path fileresult) filepath)))))

(deftest test-fileresult-rel-path-1
  (let [filepath "./fileresult.clj"
        fileresult (new-file-result (file filepath) :code)]
    (testing "test-fileresult-rel-path-1"
             (is (= (file-result-path fileresult) filepath)))))

(deftest test-fileresult-rel-path-2
  (let [filepath "../fileresult.clj"
        fileresult (new-file-result (file filepath) :code)]
    (testing "test-fileresult-rel-path-2"
             (is (= (file-result-path fileresult) filepath)))))
