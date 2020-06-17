(ns dad.db.export.flattt-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is are testing]]
            [dad.db.export.flattt :as f]
            [expound.alpha :as expound]))

; See https://github.com/bhb/expound#printer-options
(set! s/*explain-out* (expound/custom-printer {:print-specs? false}))

(stest/instrument (stest/enumerate-namespace 'dad.db.export.flattt))
    
(deftest fold-props
  (are [in expected] (= expected (#'f/fold-props in))
  
    ; in
    {:containers  {"API"     {"props"          {"marathon-ids" {"kp" "/saclib/api"}}
                              "something else" "that should be preserved"}
                   "Hutch"   {"props" {"marathon-ids" {"kp" "/saclib/hutch"}
                                       "technologies" ["RabbitMQ" "Ruby"]}}
                   "Sidekiq" {"props" {"marathon-ids" {"kp" "/saclib/sidekiq"}
                                       "technologies" ["Ruby"]}}
                   "Web"     {"props" {"marathon-ids" {"kp" "/saclib/web"}}}}
     :description "Salad Container Library -- builds libraries of salad containers (duh)"
     :props       {"regions"       ["kp"]
                   "marathon-ids"  {"kp" "/saclib"}
                   "repos"         ["saclib"]
                   "related-repos" ["saclib_adapter" "saclib-client"]}}
    ; expected
    {:containers   {"API"     {"marathon-ids"   {"kp" "/saclib/api"}
                               "something else" "that should be preserved"}
                    "Hutch"   {"marathon-ids" {"kp" "/saclib/hutch"}
                               "technologies" ["RabbitMQ" "Ruby"]}
                    "Sidekiq" {"marathon-ids" {"kp" "/saclib/sidekiq"}
                               "technologies" ["Ruby"]}
                    "Web"     {"marathon-ids" {"kp" "/saclib/web"}}}
     :description    "Salad Container Library -- builds libraries of salad containers (duh)"
     ;; This recordset should be left alone, as-is — fold-props folds props *inside* records, not recordsets
     :props        {"regions"       ["kp"]
                    "marathon-ids"  {"kp" "/saclib"}
                    "repos"         ["saclib"]
                    "related-repos" ["saclib_adapter" "saclib-client"]}}))

(deftest pathize
  (are [in expected] (= expected (#'f/pathize in))
    ; in
    {:technologies {"Clojure" {"links" {"main" "https://clojure.org"}
                               "recommendations" [{"type" "assess", "date" "2011-09-15"}
                                                  {"type" "adopt",  "date" "2012-01-12"}]}}}
    
    ; expected
    {[:technologies "Clojure" "links" "main"]      "https://clojure.org"
     [:technologies "Clojure" "recommendations" 1] {"type" "assess", "date" "2011-09-15"}
     [:technologies "Clojure" "recommendations" 2] {"type" "adopt",  "date" "2012-01-12"}}
  
    ; --------------------

    ; in
    {:records [{"event-type" "start", "date" "2011-09-15"}
               {"event-type" "stop",  "date" "2012-01-12"}]}
    
    ; expected
    {[:records 1] {"event-type" "start", "date" "2011-09-15"}
     [:records 2] {"event-type" "stop",  "date" "2012-01-12"}}
  
    ; --------------------

    ; in
    {:systems {"Discourse" {"summary"    "Web forums that don’t suck."
                            "links"      {"main" "https://discourse.org"}
                            "containers" {"web"   {"summary" "web server", "technology" "JRun", "tags" {"regions" ["us", "uk"]}}
                                          "db"    {"summary" "db server", "technology" "Access"}}}}}
    ; expected
    {[:systems "Discourse" "summary"]                           "Web forums that don’t suck."
     [:systems "Discourse" "links" "main"]                      "https://discourse.org"
     [:systems "Discourse" "containers" "web" "summary"]        "web server"
     [:systems "Discourse" "containers" "web" "technology"]     "JRun"
     [:systems "Discourse" "containers" "web" "tags" "regions"] ["us", "uk"]
     [:systems "Discourse" "containers" "db" "summary"]         "db server"
     [:systems "Discourse" "containers" "db" "technology"]      "Access"}))

(deftest interpolate-paths
  (are [in expected] (= expected (#'f/interpolate-paths in))
    ; in
    {[:technologies "Clojure" "links" "main"]    "https://clojure.org"
     [:technologies "Clojure" "recommendations"] [{"type" "assess", "date" "2011-09-15"}
                                                  {"type" "adopt",  "date" "2012-01-12"}]}
    ; expected
    {[:technologies "Clojure"]                   "Clojure"
     [:technologies "Clojure" "links" "main"]    "https://clojure.org"
     [:technologies "Clojure" "recommendations"] [{"type" "assess", "date" "2011-09-15"}
                                                  {"type" "adopt",  "date" "2012-01-12"}]}
  
    ; --------------------
    
    ; in
    {[:systems "Discourse" "summary"]                           "Web forums that don’t suck."
     [:systems "Discourse" "links" "main"]                      "https://discourse.org"
     [:systems "Discourse" "containers" "web" "summary"]        "web server"
     [:systems "Discourse" "containers" "web" "technology"]     "JRun"
     [:systems "Discourse" "containers" "web" "tags" "regions"] ["us", "uk"]
     [:systems "Discourse" "containers" "db" "summary"]         "db server"
     [:systems "Discourse" "containers" "db" "technology"]      "Access"}
  
    ; expected
    {[:systems "Discourse"]                                     "Discourse"
     [:systems "Discourse" "summary"]                           "Web forums that don’t suck."
     [:systems "Discourse" "links" "main"]                      "https://discourse.org"
     [:systems "Discourse" "containers" "web"]                  "web"
     [:systems "Discourse" "containers" "web" "summary"]        "web server"
     [:systems "Discourse" "containers" "web" "technology"]     "JRun"
     [:systems "Discourse" "containers" "web" "tags" "regions"] ["us", "uk"]
     [:systems "Discourse" "containers" "db"]                   "db"
     [:systems "Discourse" "containers" "db" "summary"]         "db server"
     [:systems "Discourse" "containers" "db" "technology"]      "Access"}))

(deftest add-fk
  (testing "Adding a key to an empty map with no meta"
    (let [m {}
          fk-table-name :technologies
          fk-table-key-val "Clojure"
          expected {:technology "Clojure"}
          expected-meta {::f/columns {:technology {::f/fk-table-name fk-table-name}}}
          res (#'f/add-fk m [fk-table-name fk-table-key-val])]
      (is (= expected res))
      (is (= expected-meta (meta res)))))
  (testing "Adding a key to a non-empty map with meta"
    (let [m (with-meta {:technology "Clojure"}
                       {::f/columns {:technology {::f/fk-table-name :technologies}}})
          fk-table-name :recommendations
          fk-table-key-val "adopt"
          expected {:technology "Clojure" :recommendation "adopt"}
          expected-meta {::f/columns {:technology {::f/fk-table-name :technologies}
                                      :recommendation {::f/fk-table-name :recommendations}}}
          res (#'f/add-fk m [fk-table-name fk-table-key-val])]
      (is (= expected res))
      (is (= expected-meta (meta res)))))
  (testing "A string table name should be converted to a keyword"
    (let [m {}
          fk-table-name "technologies"
          fk-table-key-val "Clojure"
          expected {:technology "Clojure"}
          expected-meta {::f/columns {:technology {::f/fk-table-name fk-table-name}}}
          res (#'f/add-fk m [fk-table-name fk-table-key-val])]
      (is (= expected res))
      (is (= expected-meta (meta res))))))

(deftest path+val->tables
  (testing "data"
    (are [path v expected] (= expected (#'f/path+val->tables {} path v))
      [:technologies "Clojure" :recommendations 1]
      {"type" "assess", "date" "2011-09-15"}
      {:technologies-recommendations {{:technology "Clojure" :id 1} {"type" "assess", "date" "2011-09-15"}}}

      ; --------------------
      
      [:records 1]
      {"event-type" "start", "date" "2011-09-15"}
      {:records {{:id 1} {"event-type" "start", "date" "2011-09-15"}}}
      
      ; --------------------

      [:systems "Discourse" "summary"]
      "Web forums that don’t suck."
      {:systems {{:name "Discourse"} {:summary "Web forums that don’t suck."}}}

      ; --------------------

      [:systems "Discourse"]
      "Discourse"
      {:systems {{:name "Discourse"} {}}}
      
      ; --------------------
      
      [:systems "Discourse" "containers" "db"]
      "db"
      {:systems-containers {{:name "db" :system "Discourse"} {}}}

      ; --------------------

      [:systems "Discourse" "links" "main"]
      "https://discourse.org"
      {:systems-links {{:name "main" :system "Discourse"} {:val "https://discourse.org"}}}

      ; --------------------

      [:systems "Discourse" "containers" "web" "technology"]
      "Tomcat"
      {:systems-containers {{:system "Discourse" :name "web"} {:technology "Tomcat"}}}

      ; --------------------

      [:systems "Discourse" "containers" "web" "tags" "regions"]
      ["us", "uk"]
      {:systems-containers-tags {{:system "Discourse" :container "web" :name "regions"}
                                 {:val ["us", "uk"]}}}))
  (testing "metadata"
    (are [path expected] (= expected (-> (#'f/path+val->tables {} path :foo) first val first key meta))
      [:systems "Discourse" "containers" "db"]
      ; res: {:systems-containers {{:name "db" :system "Discourse"} {}}}
      {::f/columns {:system {::f/fk-table-name :systems}}})))

(deftest db->tables
  (let [db {:technologies {"Clojure" {"links"           {"main" "https://clojure.org"}
                                      "props"           {"hosted" "true"}
                                      "recommendations" [{"type" "assess", "date" "2011-09-15"}
                                                         {"type" "adopt", "date" "2012-01-12"}]}
                           "Crux"    {"links"           {"main" "https://opencrux.com"}}
                           "Kafka"   {"links"           {"main" "https://kafka.apache.org"}
                                      "recommendations" [{"type" "assess", "date" "2013-12-16"}
                                                         {"type" "adopt",  "date" "2016-03-03"}]}}
            :systems      {"Discourse" {"links"      {"main" "https://discourse.org"}
                                        "containers" {"web"   {"summary" "web server", "technology" "Tomcat"}
                                                      "db"    {"summary" "db server",  "technology" "Access"}
                                                      "cache" {"summary" "hot keys",   "technology" "PHP"}}}}}
        ;; TODO: it’s not great that some keys are keywords and some are strings. Let’s make them all one or the
        ;; other!
        expected {:technologies                 {{:name "Clojure"} {:hosted "true"}
                                                 {:name "Crux"}    {}
                                                 {:name "Kafka"}   {}}
                  :technologies-links           {{:name "main" :technology "Clojure"} {:val "https://clojure.org"}
                                                 {:name "main" :technology "Crux"}    {:val "https://opencrux.com"}
                                                 {:name "main" :technology "Kafka"}   {:val "https://kafka.apache.org"}}
                  :technologies-recommendations {{:technology "Clojure" :id 1} {"type" "assess", "date" "2011-09-15"}
                                                 {:technology "Clojure" :id 2} {"type" "adopt",  "date" "2012-01-12"}
                                                 {:technology "Kafka"   :id 1} {"type" "assess", "date" "2013-12-16"}
                                                 {:technology "Kafka"   :id 2} {"type" "adopt",  "date" "2016-03-03"}}
                  :systems                      {{:name "Discourse"} {}}
                  :systems-links                {{:name "main" :system "Discourse"} {:val "https://discourse.org"}}
                  :systems-containers           {{:system "Discourse" :name "web"}   {:summary "web server" :technology "Tomcat"}
                                                 {:system "Discourse" :name "db"}    {:summary "db server"  :technology "Access"}
                                                 {:system "Discourse" :name "cache"} {:summary "hot keys"   :technology "PHP"}}}
        res (#'f/db->tables db)]
    (is (= expected res))
    (is (s/valid? ::f/tables res) (s/explain-str ::f/tables res))
    (doseq [[key-cols _non-key-cols] (:technologies-recommendations res)]
      (is (= {::f/columns {:technology {::f/fk-table-name :technologies
                                        ;; TODO: this should also have ::f/fk-col-name
                                        }}}
             (meta key-cols))))))
