(ns take.wing.amino.acid-test
    (:use [clojure.test])
    (:require
     [take.wing.amino.acid :as ont]
     [tawny.owl :as o]
     [tawny.reasoner :as r]
     [tawny.fixture :as f]))

(use-fixtures :each (f/reasoner :hermit))

(deftest reasonable
  (is (r/consistent? take.wing.amino.acid/take-wing-amino-acid))
  (is (r/coherent? take.wing.amino.acid/take-wing-amino-acid)))
