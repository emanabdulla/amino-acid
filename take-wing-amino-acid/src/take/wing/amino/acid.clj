

(ns take.wing.amino.acid
  (:require [clojure.string])
  (:use [tawny.owl]
        [tawny.pattern]
        [tawny.reasoner]))

(defontology aao
  :iri "http://www.purl.org/ontolink/aao")


(defclass AminoAcid)
(defclass RefiningFeature)
(defclass PhysicoChemicalProperty :super RefiningFeature)
(defclass Hydrophobicity :super PhysicoChemicalProperty)

(defoproperty hasHydrophobicity 
  :domain AminoAcid
  :range Hydrophobicity 
  :characteristic :functional)
(defclass Hydrophobic 
  :super Hydrophobicity)
(defclass Hydrophilic 
  :super Hydrophobicity 
  :disjoint Hydrophobic)
(refine Hydrophobic 
        :disjoint Hydrophilic) 
(refine Hydrophobicity 
        :equivalent (object-or Hydrophilic Hydrophobic))

(as-subclasses
 (defclass Hydrophobicity :super PhysicoChemicalProperty) 
 :disjoint :cover
 (defclass Hydrophobic)
 (defclass Hydrophilic))
(defoproperty hasHydrophobicity :domain AminoAcid 
  :range Hydrophobicity :characteristic :functional)
(defpartition Hydrophobicity
  [Hydrophobic Hydrophillic]
  :comment "Part of the Hydrophobicity value partition"
  :super PhysicoChemicalProperty
  :domain AminoAcid)
(defmacro defaapartition [& body]
  `(defpartition
     ~@body :super PhysicoChemicalProperty
     :domain AminoAcid))
(defaapartition Size
  [Small Tiny Large]
  :comment "The physical size of the amino acid.")
(defaapartition Charge
  [Negative Neutral Positive]
  :comment "The charge of an amino acid.")

(defaapartition SideChainStructure
  [Aliphatic Aromatic]
  :comment "Does the side chain contain rings or not?")

(defaapartition Polarity
  [Polar NonPolar]
  :comment "Whether there is a polarity across the amino acid.")
;; annotation properties
(defaproperty hasLongName)
(defaproperty hasShortName)
(defaproperty hasSingleLetterName)
(defn amino-acid
  "Define a new amino acid. Names is a vector with the long, three letter and
  single amino acid version. Properties are the five value partitions for each
  aa, as a list."
  [names properties]
  {:pre [(= 3 (count names))
         (= 5 (count properties))]}

  (let [aa (owl-class (first names)
                      :super AminoAcid
                      ;; we have don't test the values are correct here
                      ;; because the code layout should make the order obvious
                      ;; and the range constraints should protect us during
                      ;; reasoning.
                      (owl-some hasCharge (nth properties 0))
                      (owl-some hasHydrophobicity (nth properties 1))
                      (owl-some hasPolarity (nth properties 2))
                      (owl-some hasSideChainStructure (nth properties 3))
                      (owl-some hasSize (nth properties 4))
                      :label (first names)
                      :annotation
                      (annotation hasLongName (nth names 0))
                      (annotation hasShortName (nth names 1))
                      (annotation hasSingleLetterName (nth names 2)))]
    ;; and return types for intern
    (map ->Named
         names
         (repeat aa))))
(defn amino-acids
  [& definitions]
  (apply
   concat
   (map
    (fn [[names props]] (amino-acid names props))
    (partition 2 definitions))))

(defmacro defaminoacids
  [& definitions]
  `(tawny.pattern/intern-owl-entities
    (apply amino-acids
           (tawny.util/name-tree ~definitions))))
(defaminoacids
  [Alanine         Ala A]  [Neutral  Hydrophobic NonPolar Aliphatic Tiny]
  [Arginine        Arg R]  [Positive Hydrophilic Polar    Aliphatic Large]
  [Asparagine   Asn N]  [Neutral  Hydrophilic Polar    Aliphatic Small]
  [Aspartate      Asp D] [Negative Hydrophilic Polar    Aliphatic Small]
  [Cysteine        Cys C] [Neutral  Hydrophobic Polar    Aliphatic Small]
  [Glutamate     Glu E] [Negative Hydrophilic Polar    Aliphatic Small]
  [Glutamine     Gln Q] [Neutral  Hydrophilic Polar    Aliphatic Large]
  [Glycine          Gly G] [Neutral  Hydrophobic NonPolar Aliphatic Tiny]
  [Histidine        His H] [Positive Hydrophilic Polar    Aromatic  Large]
  [Isoleucine      Ile I]  [Neutral  Hydrophobic NonPolar Aliphatic Large]
  [Leucine          Leu L] [Neutral  Hydrophobic NonPolar Aliphatic Large]
  [Lysine             Lys K] [Positive Hydrophilic Polar    Aliphatic Large]
  [Methionine      Met M] [Neutral  Hydrophobic NonPolar Aliphatic Large]
  [Phenylalanine  Phe F] [Neutral  Hydrophobic NonPolar Aromatic  Large]
  [Proline             Pro P] [Neutral  Hydrophobic NonPolar Aliphatic Small]
  [Serine              Ser S] [Neutral  Hydrophilic Polar    Aliphatic Tiny]
  [Threonine        Thr T] [Neutral  Hydrophilic Polar    Aliphatic Tiny]
  [Tryptophan      Trp W] [Neutral  Hydrophobic NonPolar Aromatic  Large]
  [Tyrosine           Try Y] [Neutral  Hydrophobic Polar    Aromatic  Large]
  [Valine               Val V] [Neutral  Hydrophobic NonPolar Aliphatic Small]
  )
(apply as-disjoint (subclasses AminoAcid))

(defn property-for-partition [partition-value]
  (let [partition
        (first (direct-superclasses partition-value))
        op
        (.getObjectPropertiesInSignature aao)]
    (first
     (filter
      #(= partition
          (first (.getRanges % aao)))
      op))))
(defn amino-acid-def [partition-values]
(let [name
      (str
       (clojure.string/join
        (map
         #(.getFragment
           (.getIRI %))
         partition-values))
       "AminoAcid")
      exist
      (map
       (fn [val]
         (owl-some
          (property-for-partition val)
          val))
       partition-values)]

  (->Named
   name
   (owl-class
    name
    :label name
    :equivalent
    (owl-and AminoAcid exist)))))
(defn cart [colls]
(if (empty? colls)
  '(())
  (for [x (first colls)
        more (cart (rest colls))]
    (cons x more))))

;; build the classes
(doall
(map
 amino-acid-def
 ;; kill the empty list
 (rest
  (map
   #(filter identity %)
   ;; combination of all of them
   (cart
    ;; list of values for each partitions plus nil
    (map
     #(cons nil (seq (direct-subclasses %)))
     ;; all our partitions
     (seq (direct-subclasses PhysicoChemicalProperty))))))))
