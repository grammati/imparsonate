(ns imparsonate.core
  (:import [clojure.lang Keyword IFn IPersistentVector IPersistentSet]
           [java.util.regex Pattern])
  (:use [clojure [string :only [triml]]]))

(declare ^:dynamic *parser*)
(declare ^:dynamic *modifiers*)

(def ^:dynamic *debug* false)
(defn debug [& msgs]
  (if *debug* (apply prn msgs)))

(defn parse-error [msg]
  (throw (Exception. (str msg)))) ;; FIXME
 
  
(defprotocol ParserInput
  (skip-ignorable [this]))
  
(extend-protocol ParserInput
  String
    (skip-ignorable [this] (triml this)))

    
(defmulti rule 
  "Returns a function that matches the given pattern against the input, and returns
  either a two-element vector containing the result and the remaining input, or nil."
  (fn 
    ([pattern] :default) 
    ([pattern handler] (type pattern))))

(defmethod rule :default [pattern]
  (rule pattern identity))

(defmethod rule String [^String s handler]
  (let [first-char  (first s)
        last-char   (str (last s))
        identifier? (re-find #"\w" last-char)   ; FIXME - this is a nasty hack - need a more flexible way to tokenize...
        checker     (if identifier?
                      (fn [tail]
                        (or (empty? tail)
                            (re-find #"^\W" tail)))
                      (constantly true))]
    (with-meta
      (fn [input]
        (debug "Calling string rule: " s)
        (let [^String input (skip-ignorable input)]
          (if (.startsWith input s)
            (let [tail (.substring input (.length s))]
              (if (checker tail)
                [(handler s) tail])))))
      {::rule s ::handler handler})))

#_(defmethod rule String [s handler]
  (rule (re-pattern (str (Pattern/quote s) "\\b")) handler)) ; ??? much shorter, cleaner code, but it doesn't work... :(

  
(defn anchor-left [^Pattern re]
  (re-pattern (str "^(?:" (.pattern re) ")")))

(defmethod rule Pattern [^Pattern pattern handler]
  (let [pattern     (anchor-left pattern)           ; FIXME - write re-find equivalent that calls .lookingAt? Or it this just as good?
        no-handler? (identical? handler identity)]
    (with-meta
      (fn [input] 
        (debug "Calling pattern rule: " pattern)
        (let [^String input (skip-ignorable input)]
          (if-let [m (re-find pattern input)]
            (let [^String matched (if (string? m) m (m 0))
                  tail (.substring input (.length matched))
                  result (if (or (string? m) no-handler?)
                           (handler m)
                           (apply handler m))]
              [result tail]))))
      {::rule pattern ::handler handler})))

      
(defmethod rule Keyword [kwd handler]
  (let [n        (name kwd)
        modifier (*modifiers* (-> n last str keyword))
        kwd      (if modifier (->> n butlast (apply str) keyword) kwd)
        r        (delay ((or modifier identity) (kwd *parser*)))]   ;; Hmmm - binds the rule to a specific parser the first time is it used...
    (with-meta
      (fn [input]
        (debug "Calling keyword rule: " kwd)
        (if-let [r @r]
          (if-let [[matched tail] (r input)]
            [(handler matched) tail])
          (throw (Exception. (str "Invalid reference to rule " kwd))))) ; EEK - you don't find out until you try to actually parse something! :(
      {::rule kwd ::handler handler})))

(defn apply-modifiers 
  "Apply modifiers to a sequence of rules.
  eg: [:foo :+ :bar] => [(one-or-more :foo) :bar]
  "
  [rules]
  (loop [[r & [modifier & rest-if-mod :as rest-if-no-mod]] rules
         rules-out []]
    (if-not r
      rules-out
      (let [mod (*modifiers* modifier)
            r   (if mod (mod r) r)]
        (recur (if mod rest-if-mod rest-if-no-mod) (conj rules-out r))))))

(defmethod rule IPersistentVector [rules handler]
  (let [rules-in    rules
        rules       (apply-modifiers rules)
        keep?       (complement string?)
        rules       (vec (for [r rules] [(rule r) (keep? r)]))
        single?     (= 1 (count (filter second rules)))
        no-handler? (identical? handler identity)]
    (with-meta
      (fn [input]
        (debug "Calling vector rule: " rules-in)
        (loop [[[r keep?] & rules] rules
               tail input
               result []]
          (if r
            (if-let [[matched tail] (r tail)]
              (recur rules tail (if keep? (conj result matched) result)))
            (let [result (cond
                           single?     (handler (first result))
                           no-handler? result
                           :else       (apply handler result))]
              [result tail]))))
      {::rule rules-in ::handler handler})))

(defmethod rule IPersistentSet [rules handler]
  (let [rules-in rules
        rules    (doall (map rule rules))]
    (with-meta
      (fn [input]
        (debug "Calling set rule: " rules-in)
        (let [matched (filter (complement nil?) (map #(% input) rules))]
          (condp = (count matched)
            0 nil
            1 (let [[matched tail] (first matched)]
                [(handler matched) tail])
            (parse-error "Ambiguous") ; TODO - way better error message!
            )))
      {::rule rules-in ::handler handler})))

(defmethod rule IFn [f handler]
  (let [f (or (::root-rule (meta f)) f)]  ;if f is itself a parser, use its root rule
    (if (identical? handler identity)
      f
      (with-meta
        (fn [input]
          (if-let [[matched tail] (f input)]
            [(handler matched) tail]))
        {::rule f ::handler handler}))))

(prefer-method rule Keyword IFn)
(prefer-method rule IPersistentVector IFn)
(prefer-method rule IPersistentSet IFn)


(defn repeater
  "Wraps a rule with one that will match between min and max times, inclusive." 
  [r min max]
  (let [r (rule r)]
    (fn [input]
      (loop [in input
             matched []]
        (if (= (count matched) max)
          [matched in]
          (if-let [[match tail] (r in)]
            (recur tail (conj matched match))
            (if (< (count matched) min)
              nil
              [matched in])))))))
        
(defn zero-or-more [r]
  (repeater r 0 nil))
  
(defn one-or-more [r]
  (repeater r 1 nil))

(defn zero-or-one 
  "Return either the result of matching the given rule once, or nil."
  [r]
  (rule (repeater r 0 1) first))
        
(def optional zero-or-one) ; alias

(def ^:dynamic *modifiers* {
  :* zero-or-more
  :+ one-or-more
  :? zero-or-one
  ;; :i case-insensitive
  ;; :w+ :w- force whitespace / no-whitespace?
  })

(defn list-of 
  ([r] (list-of r ","))
  ([r delim] (list-of r delim true))
  ([r delim allow-empty]
    (let [r (rule [r [delim r] :*] #(concat [%1] %2))]
      (if allow-empty
        (zero-or-one r)
        r))))


(defn parser 
  "Returns a parser function for the given grammar. 
  The grammar should be a map with keyword keys and values that are either:
  * A rule defintion (a String, Pattern, Vector, or Set), or
  * A rule, as returned by the rule function.
  "
  [grammar]
  (let [ruleset (into {} (for [[k v] grammar] [k (rule v)]))
        root-rule (:root ruleset)]
    (when-not root-rule
      (throw (IllegalArgumentException. "No :root rule defined.")))
    (with-meta
      (fn [input]
        (binding [*parser* ruleset]
          (if-let [[result tail] (root-rule input)]
            (if (empty? (triml tail))
              result
              (parse-error (str "Unconsumed non-whitespace characters: " tail))))))
      {::ruleset ruleset ::root-rule root-rule})))
            
(defn- make-rulemap 
  "Slurp a flat sequence of names, rule-defs, and optional handlers into a map,
  figuring out whether to group them in twos or threes - that is, deduce whether
  a handler function has been provided for each rule.
  eg: (make-rulemap [
        :foo \"foo\" 
        :bar \"bar\" #(baz % 3)])
   => { :foo (rule \"foo\")
        :bar (rule \"bar\" #(baz % 3)) }
  "
  [rules]
  (loop [rulemap {}
         [name ruledef & [handler & rest-if-handler :as rest-if-no-handler]] rules]
    (if-not name
      rulemap
      (if-not ruledef
        (throw (IllegalArgumentException. (str "Rule " name " has no definition.")))
        (if (or (keyword? handler) (nil? handler))
          (recur (assoc rulemap name `(rule ~ruledef)) rest-if-no-handler)
          (recur (assoc rulemap name `(rule ~ruledef ~handler)) rest-if-handler))))))

(defmacro defparser 
  [name & rules]
  `(def ~name (parser ~(make-rulemap rules))))


  
;; debugging:
(defn fragment 
  "Test input against a non-root rule, or against all rules."
  ([parser input]
    (binding [*parser* (::ruleset (meta parser))]
       (doseq [[rule-name r] *parser*]
         (if-let [[result tail] (r input)]
           (println rule-name " => " result " (" (count tail) " chars remaining)")))))
  ([parser rule-name input]
    (binding [*parser* (::ruleset (meta parser))]
      (let [r (rule-name *parser*)]
        (if r
          (r input)
          (throw (Exception. (str "No such rule: " rule-name)))))))) 












