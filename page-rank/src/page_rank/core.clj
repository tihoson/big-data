(ns page-rank.core
  (:gen-class)
  (:require [cheshire.core :as json]))

(defrecord Node [name neighbours parents rank])

(defn contain?
  [e coll]
  (some #(= e %) coll))

(defn get-node-parents
  [node graph]
  (->> graph
       (filter #(contain? (:name node) (:neighbours %)))
       (map :name)))

(defn update-parents
  [node graph]
  (update node :parents #(if (nil? %)
                           (get-node-parents node graph)
                           (:parents node))))

(defn read-graph
  [path]
  (->> path
       slurp
       (#(json/parse-string % keyword))
       (map map->Node)
       (#(map (fn [node] (update-parents node %)) %))
       (#(map (fn [node] (assoc node :rank (/ 1 (count %)))) %))
       (map (fn [node] [(:name node) node]))
       (into {})))


(def alpha 0.85)

(defn get-pagerank-sum
  [node graph]
  (reduce + (map (fn [node] 
                   (let [size (count (:neighbours node))]
                     (if (zero? size)
                       0
                       (/ (:rank node) size))))
                 (map (fn [[_ node]] node) (select-keys graph (:parents node))))))

(defn calc-new-node-rank
  [node graph]
  (+ 
   (* (- 1 alpha) (/ 1 (count graph)))
   (* (get-pagerank-sum node graph) alpha)))

(defn update-ranks
  [graph]
  (let [updated (map (fn [[_ node]] 
                       (assoc node :rank (calc-new-node-rank node graph))) 
                     graph)
        total-weight (reduce + (map :rank updated))
        normalized (map (fn [node] 
                          [(:name node) (assoc node :rank (/ (:rank node) total-weight))]) 
                        updated)]
    (into {} normalized)))

(defn calc-rank
  [graph]
  (let [tries 100000]
    (loop [graph graph
           iter 0]
      (if (> tries iter)
        (recur (update-ranks graph) (inc iter))
        graph))))

(defn -main
  [& args]
  (->> (first args)
       read-graph
       calc-rank
       (map (fn [[name node]] [name (:rank node)]))
       (sort-by second)
       reverse
       (run! println)))
