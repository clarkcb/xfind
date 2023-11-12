(ns cljfind.filetypes-test
  (:require [clojure.test :refer :all])
  (:use [cljfind.filetypes :only (get-file-type)]))

(deftest test-get-file-type-archive-file
  (let [f "archive.zip"]
    (testing "test-get-file-type-archive-file"
      (is (= (get-file-type f) :archive)))))

(deftest test-get-file-type-audio-file
  (let [f "music.mp3"]
    (testing "test-get-file-type-audio-file"
      (is (= (get-file-type f) :audio)))))

(deftest test-get-file-type-binary-file
  (let [f "binary.exe"]
    (testing "test-get-file-type-binary-file"
      (is (= (get-file-type f) :binary)))))

(deftest test-get-file-type-code-file
  (let [f "code.clj"]
    (testing "test-get-file-type-code-file"
      (is (= (get-file-type f) :code)))))

(deftest test-get-file-type-font-file
  (let [f "font.ttf"]
    (testing "test-get-file-type-font-file"
      (is (= (get-file-type f) :font)))))

(deftest test-get-file-type-image-file
  (let [f "image.png"]
    (testing "test-get-file-type-image-file"
             (is (= (get-file-type f) :image)))))

(deftest test-get-file-type-text-file
  (let [f "text.txt"]
    (testing "test-get-file-type-text-file"
             (is (= (get-file-type f) :text)))))

(deftest test-get-file-type-video-file
  (let [f "movie.mp4"]
    (testing "test-get-file-type-video-file"
             (is (= (get-file-type f) :video)))))

(deftest test-get-file-type-xml-file
  (let [f "markup.xml"]
    (testing "test-get-file-type-xml-file"
      (is (= (get-file-type f) :xml)))))

(deftest test-get-file-type-unknown-file
  (let [f "unknown.xyz"]
    (testing "test-get-file-type-unknown-file"
      (is (= (get-file-type f) :unknown)))))
