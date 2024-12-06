(ns cljfind.findsettings-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.findsettings :only
         (DEFAULT-FIND-SETTINGS add-extension add-pattern set-archives-only set-debug)]))

(deftest test-default-settings
  (let [settings DEFAULT-FIND-SETTINGS]
    (testing "test-default-settings"
      (is (not (:archives-only settings)))
      (is (not (:debug settings)))
      (is (not (:follow-symlinks settings)))
      (is (not (:include-archives settings)))
      (is (not (:include-hidden settings)))
      (is (empty? (:paths settings)))
      (is (not (:print-dirs settings)))
      (is (not (:print-files settings)))
      (is (not (:print-usage settings)))
      (is (not (:print-version settings)))
      (is (:recursive settings))
      (is (not (:verbose settings))))))

(deftest test-add-extensions
  (let [settings  DEFAULT-FIND-SETTINGS
        with-txt (add-extension settings "txt" :in-extensions)
        with-mult (add-extension with-txt "cs,clj" :in-extensions)]
    (testing "test-add-extensions"
      (is (= (count (:in-extensions with-txt)) 1))
      (is (= (count (:in-extensions with-mult)) 3)))))

(deftest test-add-pattern
  (let [settings     DEFAULT-FIND-SETTINGS
        with-pattern (add-pattern settings "Find" :dir-patterns)]
    (testing "test-add-pattern"
      (is (= (count (:dir-patterns with-pattern)) 1)))))

(deftest test-set-archives-only
  (let [settings           DEFAULT-FIND-SETTINGS
        with-archives-only (set-archives-only settings true)]
    (testing "test-set-archives-only"
      (is (:archives-only with-archives-only))
      (is (:include-archives with-archives-only)))))

(deftest test-set-debug
  (let [settings   DEFAULT-FIND-SETTINGS
        with-debug (set-debug settings true)]
    (testing "test-set-debug"
      (is (:debug with-debug))
      (is (:verbose with-debug)))))
