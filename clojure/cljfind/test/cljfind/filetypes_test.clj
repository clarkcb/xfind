(ns cljfind.filetypes-test
  (:require [clojure.test :refer :all])
  (:use [cljfind.filetypes :only (get-filetype)]))

(deftest test-get-filetype-archive-file
  (let [f "archive.zip"]
    (testing "test-get-filetype-archive-file"
      (is (= (get-filetype f) :archive)))))

(deftest test-get-filetype-binary-file
  (let [f "binary.exe"]
    (testing "test-get-filetype-binary-file"
      (is (= (get-filetype f) :binary)))))

(deftest test-get-filetype-text-file
  (let [f "text.txt"]
    (testing "test-get-filetype-text-file"
      (is (= (get-filetype f) :text)))))

(deftest test-get-filetype-code-file
  (let [f "code.clj"]
    (testing "test-get-filetype-code-file"
      (is (= (get-filetype f) :code)))))

(deftest test-get-filetype-xml-file
  (let [f "markup.xml"]
    (testing "test-get-filetype-xml-file"
      (is (= (get-filetype f) :xml)))))

(deftest test-get-filetype-unknown-file
  (let [f "unknown.xyz"]
    (testing "test-get-filetype-unknown-file"
      (is (= (get-filetype f) :unknown)))))
