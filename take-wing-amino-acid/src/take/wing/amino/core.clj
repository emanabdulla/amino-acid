(ns take.wing.amino
  [:use [tawny.owl]]
  [:require [take.wing.amino.acid]])


(defn -main [& args]
  (save-ontology take.wing.amino.acid/take-wing-amino-acid "take-wing-amino-acid.omn"))
