(ns cljfind.findsettings-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.findsettings :only
    (DEFAULT-SETTINGS add-extension add-pattern set-archivesonly set-debug)]))

(deftest test-default-settings
  (let [settings DEFAULT-SETTINGS]
    (testing "test-default-settings"
      (is (not (:archivesonly settings)))
      (is (not (:debug settings)))
      (is (:excludehidden settings))
      (is (not (:firstmatch settings)))
      (is (= (:linesafter settings) 0))
      (is (= (:linesbefore settings) 0))
      (is (not (:listdirs settings)))
      (is (not (:listfiles settings)))
      (is (not (:listlines settings)))
      (is (= (:maxlinelength settings) 150))
      (is (not (:multilineoption-REMOVE settings)))
      (is (:printresults settings))
      (is (not (:printusage settings)))
      (is (not (:printversion settings)))
      (is (:recursive settings))
      (is (not (:findarchives settings)))
      (is (= (:startpath settings) nil))
      (is (not (:uniquelines settings)))
      (is (not (:verbose settings))))))

(deftest test-add-extensions
  (let [settings DEFAULT-SETTINGS
        with-txt (add-extension settings "txt" :in-extensions)
        with-mult (add-extension with-txt "cs,clj" :in-extensions)]
    (testing "test-add-extensions"
      (is (= (count (:in-extensions with-txt)) 1))
      (is (= (count (:in-extensions with-mult)) 3)))))

(deftest test-add-pattern
  (let [settings DEFAULT-SETTINGS
        with-pattern (add-pattern settings "Find" :findpatterns)]
    (testing "test-add-pattern"
      (is (= (count (:findpatterns with-pattern)) 1)))))

(deftest test-set-archivesonly
  (let [settings DEFAULT-SETTINGS
        with-archivesonly (set-archivesonly settings true)]
    (testing "test-set-archivesonly"
      (is (:archivesonly with-archivesonly))
      (is (:findarchives with-archivesonly)))))

(deftest test-set-debug
  (let [settings DEFAULT-SETTINGS
        with-debug (set-debug settings true)]
    (testing "test-set-debug"
      (is (:debug with-debug))
      (is (:verbose with-debug)))))
