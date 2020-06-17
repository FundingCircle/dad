(ns dad.medley
  (:require [clojure.set :as set :refer [union]]
            [clojure.string :refer [lower-case]]))

(set! *warn-on-reflection* true)

(def case-insensitive-sorted-map
  "Be careful when using this! If any keys are the same except for casing, one will be lost."
  (sorted-map-by (fn [a b] (compare (lower-case a) (lower-case b)))))

(defn deep-mergecat
  "Recursively merges maps together. If all the maps supplied have nested maps
  under the same keys, these nested maps are merged. Otherwise:
  
  * If the two values under the same keys are both sets, they’re combined with `clojure.set/union`
  * If the two values under the same keys are both collections, they’re concatenated
  * Otherwise, the value is overwritten as in `clojure.core/merge`
  
  This is a modified version of `medley.core/deep-merge` from https://github.com/weavejester/medley"
  {:arglists '([& maps])
   :based-on "https://github.com/weavejester/medley/blob/6c79c4cce52b276daa3c2b6eaea78f96904bca56/src/medley/core.cljc#L213"}
  ([])
  ([a] a)
  ([a b]
   (when (or a b)
     (letfn [(merge-entry [m e]
               (let [k  (key e)
                     v' (val e)]
                 (if (contains? m k)
                   (assoc m k (let [v (get m k)]
                                (cond (and (map? v) (map? v'))    (deep-mergecat v v')
                                      (and (set? v) (set? v'))    (union v v')
                                      (and (coll? v) (coll? v'))  (concat v v')
                                      :else                       v')))
                   (assoc m k v'))))]
       (reduce merge-entry (or a {}) (seq b)))))
  ([a b & more]
   (reduce deep-mergecat (or a {}) (cons b more))))
