(ns dad.db.export.flattt
  "flattt = “Flatten To Tables” — I’m thinking of extracting this as a standalone library/tool.
  
  Prior art: https://github.com/OpenDataServices/flatten-tool"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.walk :as walk :refer [postwalk]]
            [inflections.core :refer [singular]]
            [medley.core :refer [deep-merge]]))

(s/def ::non-blank-string (s/and string? (complement str/blank?)))
(s/def ::non-blank-keyword (s/with-gen keyword?
                              #(gen/fmap keyword (s/gen ::non-blank-string))))
(s/def ::scalar           (s/or :number number?
                                :keyword keyword?
                                :string string?))
(s/def ::non-empty-scalar (s/or :number number?
                                :keyword ::non-blank-keyword
                                :string ::non-blank-string))
(s/def ::col-name         (s/or :keyword ::non-blank-keyword
                                :string  ::non-blank-string))
(s/def ::row-col-val      (s/or :scalar      ::non-empty-scalar
                                :scalar-coll (s/coll-of ::non-empty-scalar :gen-max 10)))
(s/def ::row-cols         (s/map-of ::col-name ::row-col-val :gen-max 10))
(s/def ::key-cols         (s/map-of ::col-name ::non-empty-scalar))
(s/def ::rows             (s/map-of ::key-cols ::row-cols :gen-max 10))
(s/def ::table-name       ::non-blank-keyword)
(s/def ::tables           (s/map-of ::table-name ::rows :gen-max 10))

;; Specs used only for/by the private functions
(s/def ::path      (s/every ::scalar :min-count 1 :gen-max 10))
(s/def ::path-val  (s/or :row-col-var ::row-col-val
                         :rows        ::rows))
(s/def ::path-rows (s/every-kv ::path ::row-cols :gen-max 10))

(defn- unkeyword
  [v]
  (if (keyword? v)
    (name v)
    v))

(def ^:private separator "-")

(defn- join-names
  [names]
  (try
    (->> (map unkeyword names)
         (str/join separator)
         (keyword))
    (catch Exception e
      (println names)
      (throw e))))

;; This is used only for testing.
(s/def ::simple-test-map
  ;; Using every-kv and every here because they don’t conform, and conformance is both unneeded here
  ;; and also causes problems in the fdef:fn.
  (s/every-kv (s/or :s string? :k keyword?)
              (s/or :s ::non-empty-scalar
                    :sc (s/every ::non-empty-scalar :gen-max 5)
                    :scm (s/every ::simple-test-map :gen-max 2)
                    :m ::simple-test-map)
              :gen-max 5
              :conform-keys true))

(s/fdef fold-props
  :args (s/cat :m (s/with-gen ::simple-test-map
                    #(gen/frequency [[9 (s/gen ::simple-test-map)]
                                     [1 (gen/return {:foo {:bar "baz"
                                                           "props" (gen/generate (s/gen ::simple-test-map))}})]])))
  :ret  ::simple-test-map
  :fn   (fn [{{in :m} :args
              out     :ret}]
          (let [map-seq (fn map-seq [m] (tree-seq map? #(interleave (keys %) (vals %)) m))]
            (if (some #{"props"} (set (map-seq in)))
              ; TODO: this is incomplete; it doesn’t assert that the entries of "props" are retained
              (not-any? #{"props"} (map-seq out))
              (= out in)))))

(defn- fold-props
  "TODO: we could probably fold this operation into pathize.
  This will leave alone recordsets named :props, by virtue of recordset keys being keywords, not
  strings."
  [m]
  (postwalk
    (fn [v]
      (if-let [props (and (map? v) (map? (v "props")) (v "props"))]
        (merge (dissoc v "props") props)
        v))
    m))

(s/fdef path+rows->indexed-rows
  :args (s/cat :path     ::path
               :row-cols (s/every ::row-cols :min-count 1 :gen-max 10))
  :ret  (s/and ::path-rows #(every? (comp pos-int? last first) %))
  :fn   (fn [{{:keys [path row-cols]} :args
              ret                     :ret}]
          (and (= (set row-cols) (set (vals ret)))
               (every? #(= path (butlast %)) (keys ret))
               (let [indices (sort (map (comp last key) ret))]
                 (apply < indices)))))

(defn- path+rows->indexed-rows
  "Accepts a path, and a sequential collection of maps that represent rows. Returns a map of
  paths to those rows, with each path being the supplied path with an additional value: the index of
  that row, starting from 1 (because 0-based indexing is absurd)."
  [path rows]
  (->> rows
       (map-indexed (fn [i v]
                      [(conj path (inc i))
                       v]))
       (into {})))

(defn- pathize
  {:derived-from "https://andersmurphy.com/2019/11/30/clojure-flattening-key-paths.html"}
  ([m]
   (pathize m []))
  ([m path]
   (->> m
       (map (fn [[k v]]
                 (cond
                   (and (map? v) (seq v))
                   (pathize v (conj path k))

                   (and (coll? v) (sequential? v) (every? map? v))
                   (path+rows->indexed-rows (conj path k) v)

                   :else
                   [(conj path k) v])))
        (into {}))))

(defn- interpolate-paths
  [paths]
  (reduce-kv
    (fn [paths path _v]
      (let [path-variants (map #(take % path) (range 2 (count path) 2))]
         (->> (mapv #(vector % (last %)) path-variants)
              (into {})
              (merge paths))))
    paths
    paths))

(defn- key-col-name
  [key-val]
  (if (or (number? key-val)
          (uuid? key-val))
      :id
      :name))

; TODO
; (s/fdef add-fk
;   :args (s/cat :m            ::row-cols
;                :name-and-val (s/tuple ::non-blank-keyword ::scalar))
;   :ret  ::key-cols)

(defn- add-fk
  [m [fk-table-name fk-table-key-val]]
  (let [col-name (keyword (or (singular fk-table-name) fk-table-name))]
    (-> (assoc m col-name (unkeyword fk-table-key-val))
        (vary-meta deep-merge {::columns {col-name {::fk-table-name fk-table-name}}}))))

; TODO
; (s/fdef path+val->tables
;   :args (s/cat :tables   ::tables
;                :path+val (s/tuple ::path ::path-val))
;   :ret  ::tables)

(defn- path+val->tables
  "Adds the supplied value to the supplied tables aggregate, as per the supplied path."
  [tables path v]
  (let [kpp (partition 2 path)
        table-name-parts (take-nth 2 (butlast path))
        table-name (join-names table-name-parts)
        fk-table? (pos? (count table-name-parts))
        f-keys (if fk-table?
                  (reduce add-fk {} (vec (butlast kpp))) ; convert to vector because of a spec that uses s/tuple
                  {})
        p-keys (let [key-val (second (last kpp))]
                 (assoc f-keys (key-col-name key-val) (unkeyword key-val)))
        col-name  (if (odd? (count path))
                    (keyword (last path))
                    :val)
        col (cond (map? v)           v
                  (= (last path) v)  {}
                  :else              {col-name v})]
    (update-in tables [table-name p-keys] merge col)))

; TODO
; (s/fdef db->tables
;   :args (s/cat :db :dad/db)
;   :ret  ::tables)

(defn db->tables
  [db]
  (->> db
       (fold-props)
       (pathize)
       (interpolate-paths)
       (reduce-kv path+val->tables {})))
