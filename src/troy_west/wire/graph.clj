(ns troy-west.wire.graph
  "
  ### Visualisation

  There are three built in functions for visualising graphs,
  `viz-graph-names`, `viz-graph-results` and `viz-graph-fns`.

  Each can be called like:

  ```clojure
  => (wire/viz-graph-names graph {:foo/a 15 :foo/b 3})
  ```
  ![View graph with names](img/viz-graph-names.png)
  ```clojure
  => (wire/viz-graph-results graph {:foo/a 15 :foo/b 3})
  ```
  ![View graph with results](img/viz-graph-results.png)
  ```clojure
  => (wire/viz-graph-fns graph {:foo/a 15 :foo/b 3})
  ```
  ![View graph with functions](img/viz-graph-fns.png)
  "

  (:require
   [clojure.walk :as w]
   [rhizome.viz :as viz]
   [troy-west.wire :as wire]))

;;
;; Fns for use in visualising graphs
;;

(defn format-value
  ([_form] (format-value nil))
  ([form filter-keys]
   (w/postwalk
    (fn [x] (cond
              (number? x) (format "%.2f" (float x))
              (map? x) (for [[k v] x]
                         (if (contains? filter-keys k) k [k v]))
              (seq? x) (into [] x)
              (nil? x) ::free
              :else x))
    form)))

(defn describe-with-result
  [_ args results]
  (fn [n]
    {:label [n (format-value (n results) args)]}))

(defn describe-with-fn
  [graph args _results]
  (fn [n]
    {:label [n (format-value (-> graph :wire/dep-map n second) args)]}))

(defn viz-graph*
  "
  Low level function for using rhizome.viz functions to visualise graphs.
  "
  [viz-fn graph args results {:keys [node->descriptor]
                              :or   {node->descriptor (fn [_ _ _]
                                                        (fn [n] {:label n}))}
                              :as   opts}]
  (let [deps (-> graph :wire/dep-graph :dependencies)]
    (apply viz-fn
           (concat (keys deps) (keys args))
           deps
           :node->descriptor
           (node->descriptor graph args results)
           :options {:rankdir :BT}
           (apply concat (-> opts
                             (dissoc :node->descriptor)
                             (assoc :edge->descriptor (fn [_ _] {:dir :back})))))))

(defn viz-graph-names
  "
  Visualise a graph of the names (keywords) of the nodes.

  The 2 arity version of this function will execute the `graph` with the given
  `args`. The 3 arity version allows you to provided an `opts` map containing
  a `:results` map of the execution to avoid this function executing the graph.
  "
  ([graph args]
   (viz-graph-names graph args (wire/execute-graph graph args)))
  ([graph args {:keys [viz-fn results]
                :or   {viz-fn  viz/view-graph
                       results {}}
                :as   opts}]
   (viz-graph* viz-fn graph args results opts)))

(defn viz-graph-results
  "
  Visualise a graph of the names of the nodes along with their resolved value.

  The 2 arity version of this function will execute the `graph` with the given
  `args`. The 3 arity version allows you to provided an `opts` map containing
  a `:results` map of the execution to avoid this function executing the graph.
  "
  ([graph args]
   (viz-graph-results graph args {:results (wire/execute-graph graph args)}))
  ([graph args {:keys [viz-fn results]
                :or   {viz-fn  viz/view-graph
                       results {}}
                :as   opts}]
   (viz-graph* viz-fn graph args results (assoc opts
                                                :node->descriptor
                                                describe-with-result))))

(defn viz-graph-fns
  "
  Visualise a graph of the names of the nodes along with the function associated with
  each node.

  The 2 arity version of this function will execute the `graph` with the given
  `args`. The 3 arity version allows you to provided an `opts` map containing
  a `:results` map of the execution to avoid this function executing the graph.
  "
  ([graph args]
   (viz-graph-fns graph args {:results (wire/execute-graph graph args)}))
  ([graph args {:keys [viz-fn results]
                :or   {viz-fn  viz/view-graph
                       results {}}
                :as   opts}]
   (viz-graph* viz-fn graph args results (assoc opts
                                                :node->descriptor
                                                describe-with-fn))))
