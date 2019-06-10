(ns util.sdef
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::number number?)
(spec/def ::vector vector?)
(spec/def ::boolean boolean?)

(defn gen-check [pspec param msg]
  ;; TODO take into account the global vars ?
  `(if (spec/invalid? (spec/conform ~pspec ~param))
     (let [ed# (merge {::spec/failure :assertion-failed}
                 (spec/explain-data* ~pspec ['~param] ['~pspec] [] ~param))]
       (throw (ex-info "return value does not conform to spec" {::info true}
                (ex-info (str ~msg (with-out-str (spec/explain-out ed#))) ed#))))
     ;;else
     true))

(defn inject-spec-assert
  "adds :pre condition for each function parameter annotated with keyword that is valid spec"
  [form]
  ;;(println "==" form "==")
  (let [[type content] (:body form)
        plist          (->> form :params :params (map last)
                         (map #(when-let [aspec (seq (filter spec/get-spec (keys (meta %))))] [% (first aspec)]))
                         (filter seq))
        passert        (map (fn [[param pspec]] (gen-check pspec param "Defun parameters spec assertion failed\n"))
                         plist)
        res            (assoc form :body
                         (or
                           (and  (= :body type)
                             [:prepost+body {:body    content
                                             :prepost {:pre passert}}])
                           (and (= :prepost+body type)
                             [:prepost+body (update-in content [:prepost :pre] concat passert)])))]

    ;;(println "==" form res "==")
    
    res))

(defn inject-ret-assert [tags prepost]
  (let [the-spec (->> tags keys (filter spec/get-spec) first)
        res (if the-spec
              (update-in prepost [:post] conj
                (gen-check the-spec '% "Return value does not conform to spec\n"))
              ;; else
              prepost)]
    ;;(println "==" tags prepost res "==")
    
    res))

(defmacro sdefn [& form]
  (let [ast (spec/conform :clojure.core.specs.alpha/defn-args form)]
    (when (spec/invalid? ast) (spec/assert* :clojure.core.specs.alpha/defn-args form))

    (let [args (if (-> ast :fn-tail first (= :arity-1))
                 (update-in ast [:fn-tail 1] inject-spec-assert)
                 (update-in ast [:fn-tail 1 :bodies] (fn [b] (mapv inject-spec-assert b))))

          bodies-ast (if (-> args :fn-tail first (= :arity-1))
                       [(-> args :fn-tail last)]
                       (-> args :fn-tail last :bodies))]

      ;;(clojure.pprint/pprint args)

      `(defn ~(:fn-name ast)
         ~(or (:docstring ast) "")
         ~@(map
             (fn [b]
               (apply list
                 (mapv last (-> b :params :params))
                 ;; TODO that might be ugly if there are no args to validate?
                 (->> b :body last :prepost (inject-ret-assert (meta (:fn-name ast))))
                 (-> b :body last :body)))
             bodies-ast)))))
