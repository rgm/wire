(ns troy-west.wire-test
  (:require
   #?(:clj  [clojure.test :refer [deftest is]]
      :cljs [clojure.test :refer [deftest is] :include-macros true])
   [troy-west.wire :as sut]))

(def dep-map
  {:foo/c [[:foo/a :foo/b]
           *]
   :foo/d [[:foo/c]
           inc]
   :foo/e [[:foo/a :foo/c :foo/d]
           +]})

(deftest compile-graph-test
  (let [graph (sut/compile-graph dep-map)]
    (is (= dep-map (:wire/dep-map graph)))
    (is (= {:foo/c #{:foo/a :foo/b}
            :foo/d #{:foo/c}
            :foo/e #{:foo/a :foo/c :foo/d}}
           (-> graph :wire/dep-graph :dependencies)))
    (is (= {:foo/a #{:foo/c :foo/e}
            :foo/b #{:foo/c}
            :foo/c #{:foo/d :foo/e}
            :foo/d #{:foo/e}}
           (-> graph :wire/dep-graph :dependents)))))

(deftest free-variables-test
  (is (= #{:foo/a :foo/b}
         (sut/free-variables (:wire/dep-graph (sut/compile-graph dep-map))))))

(deftest execute-graph-test
  (is (= {:foo/a 15 :foo/b 3 :foo/c 45 :foo/d 46 :foo/e 106}
         (sut/execute-graph (sut/compile-graph dep-map) {:foo/a 15 :foo/b 3}))))

(deftest compile-and-execute-test
  (is (= {:foo/a 15 :foo/b 3 :foo/c 45 :foo/d 46 :foo/e 106}
         (sut/compile-and-execute dep-map {:foo/a 15 :foo/b 3}))))

(deftest override-bound-value-test
  (is (= {:foo/a 15 :foo/b 3 :foo/c 20 :foo/d 21 :foo/e 56}
         (sut/compile-and-execute dep-map {:foo/a 15 :foo/b 3 :foo/c 20}))))

(deftest compose-with-merge-test
  (is (= {:foo/a 15 :foo/b 14 :foo/c 210 :foo/d 211 :foo/e 436}
         (sut/compile-and-execute (merge dep-map {:foo/b [[:foo/a] dec]})
                                  {:foo/a 15}))))

(deftest with-ns-test
  (is (= (sut/with-ns {:foo/c [[:a :b]
                               *]
                       :foo/d [[:foo/c]
                               inc]
                       :foo/e [[:a :foo/c :foo/d]
                               +]}
           :bar)
         {:foo/c [[:bar/a :bar/b]
                  *]
          :foo/d [[:foo/c]
                  inc]
          :foo/e [[:bar/a :foo/c :foo/d]
                  +]})))

(deftest replace-keys-test
  (is (= (sut/replace-keys {:foo/c [[:foo/a :foo/b]
                                    *]
                            :foo/d [[:foo/c]
                                    inc]
                            :foo/e [[:foo/a :foo/c :foo/d]
                                    +]}
                           {:foo/c :bar/z})
         {:bar/z [[:foo/a :foo/b]
                  *]
          :foo/d [[:bar/z]
                  inc]
          :foo/e [[:foo/a :bar/z :foo/d]
                  +]})))

(deftest replace-namespaces-test
  (is (= (sut/replace-namespaces {:foo/c [[:bar/a :bar/b]
                                          *]
                                  :bar/d [[:foo/c]
                                          inc]
                                  :foo/e [[:foo/a :foo/c :bar/d]
                                          +]}
                                 {:bar :baz})
         {:foo/c [[:baz/a :baz/b]
                  *]
          :baz/d [[:foo/c]
                  inc]
          :foo/e [[:foo/a :foo/c :baz/d]
                  +]})))

(deftest append-ns-test
  (is (= (sut/append-ns {:foo/c [[:bar/a :bar/b]
                                 *]
                         :bar/d [[:foo/c]
                                 inc]
                         :foo/e [[:foo/a :foo/c :bar/d]
                                 +]}
                        :baz)
         {:foo.baz/c [[:bar.baz/a :bar.baz/b]
                      *]
          :bar.baz/d [[:foo.baz/c]
                      inc]
          :foo.baz/e [[:foo.baz/a :foo.baz/c :bar.baz/d]
                      +]}))
  (is (= (sut/append-ns {:foo/c [[:bar/a :bar/b]
                                 *]
                         :bar/d [[:foo/c]
                                 inc]
                         :foo/e [[:foo/a :foo/c :bar/d]
                                 +]}
                        :baz
                        {:exclude #{:bar}})
         {:foo.baz/c [[:bar/a :bar/b]
                      *]
          :bar/d     [[:foo.baz/c]
                      inc]
          :foo.baz/e [[:foo.baz/a :foo.baz/c :bar/d]
                      +]}))
  (is (= (sut/append-ns {:foo/c [[:bar/a :bar/b]
                                 *]
                         :bar/d [[:foo/c]
                                 inc]
                         :foo/e [[:foo/a :foo/c :bar/d]
                                 +]}
                        :baz
                        {:only #{:bar}})
         {:foo/c     [[:bar.baz/a :bar.baz/b]
                      *]
          :bar.baz/d [[:foo/c]
                      inc]
          :foo/e     [[:foo/a :foo/c :bar.baz/d]
                      +]})))

(deftest filter-ns-test
  (is (= (sut/filter-ns {:foo/c [[:bar/a :bar/b]
                                 *]
                         :bar/d [[:foo/c]
                                 inc]
                         :foo/e [[:foo/a :foo/c :bar/d]
                                 +]}
                        :bar)
         {:bar/d [[:foo/c]
                  inc]})))

(deftest re-filter-ns-test
  (is (= (sut/re-filter-ns {:foo/c [[:bar/a :bar/b]
                                    *]
                            :bar/d [[:foo/c]
                                    inc]
                            :foo/e [[:foo/a :foo/c :bar/d]
                                    +]}
                           #"f.*")
         {:foo/c [[:bar/a :bar/b]
                  *]
          :foo/e [[:foo/a :foo/c :bar/d]
                  +]})))

(deftest list-namespaces-test
  (is (= (sut/list-namespaces {:foo/c [[:bar/a :bar/b]
                                       *]
                               :bar/d [[:foo/c]
                                       inc]
                               :foo/e [[:foo/a :foo/c :bar/d]
                                       +]})
         #{"foo" "bar"})))
