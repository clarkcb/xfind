(ns cljfind.argtokenizer-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.fileutil :only (to-path)]
        [cljfind.argtokenizer]
        [cljfind.findoptions]))

(deftest test-short-help-arg
  (let [arg-tokenizer (get-arg-tokenizer-for-options FIND-OPTIONS)
        [tokens errs] (tokenize-args arg-tokenizer ["-h"])]
    (testing "test-short-help-arg"
             (is (= (count tokens) 1))
             (is (empty? errs))
             (is (= (:name (first tokens)) :help))
             (is (= (:type (first tokens)) :bool))
             (is (= (:value (first tokens)) true))
             )))

(deftest test-long-help-arg
  (let [arg-tokenizer (get-arg-tokenizer-for-options FIND-OPTIONS)
        [tokens errs] (tokenize-args arg-tokenizer ["--help"])]
    (testing "test-long-help-arg"
             (is (= (count tokens) 1))
             (is (empty? errs))
             (is (= (:name (first tokens)) :help))
             (is (= (:value (first tokens)) true))
             )))

(deftest test-combined-short-args
  (let [arg-tokenizer (get-arg-tokenizer-for-options FIND-OPTIONS)
        [tokens errs] (tokenize-args arg-tokenizer ["-Cvzx" "clj"])]
    (testing "test-combined-short-args"
             (is (= (count tokens) 4))
             (is (empty? errs))
             (is (= (:name (first tokens)) :nocolorize))
             (is (= (:value (first tokens)) true))
             (is (= (:name (nth tokens 1)) :verbose))
             (is (= (:value (nth tokens 1)) true))
             (is (= (:name (nth tokens 2)) :includearchives))
             (is (= (:value (nth tokens 2)) true))
             (is (= (:name (nth tokens 3)) :in-ext))
             (is (= (:value (nth tokens 3)) "clj"))
             )))

(deftest test-long-arg-with-val
  (let [arg-tokenizer (get-arg-tokenizer-for-options FIND-OPTIONS)
        [tokens errs] (tokenize-args arg-tokenizer ["--settings-file=~/src/xfind/shared/settings.json"])]
    (testing "test-long-arg-with-val"
             (is (= (count tokens) 1))
             (is (empty? errs))
             (is (= (:name (first tokens)) :settings-file))
             (is (= (:value (first tokens)) "~/src/xfind/shared/settings.json"))
             )))
