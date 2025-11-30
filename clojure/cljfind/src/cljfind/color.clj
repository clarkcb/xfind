;;; ############################################################################
;;;
;;; color.clj
;;;
;;; Defines generic colors
;;;
;;; ############################################################################

(ns cljfind.color
  #^{:author "Cary Clark",
     :doc "Defines generic colors"}
  (:use [clojure.string :as str :only (lower-case)]
        [cljfind.consolecolor]))

;; color keyword names
(def ^:const COLOR_NAMES [:black :red :green :yellow :blue :magenta :cyan :white])

(defn get-console-color-for-color [c]
  (cond
    (= :black c) CONSOLE_COLOR_BLACK
    (= :red c) CONSOLE_COLOR_RED
    (= :green c) CONSOLE_COLOR_GREEN
    (= :yellow c) CONSOLE_COLOR_YELLOW
    (= :blue c) CONSOLE_COLOR_BLUE
    (= :magenta c) CONSOLE_COLOR_MAGENTA
    (= :cyan c) CONSOLE_COLOR_CYAN
    (= :white c) CONSOLE_COLOR_WHITE
    :else CONSOLE_COLOR_BLACK))
