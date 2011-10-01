(ns imparsonate.test.core
  (:use [imparsonate.core] :reload)
  (:use [clojure.test])
  (:use [clojure [string :only [upper-case]]]))


(deftest test-string-rule
  (let [r (rule "foo")]
    (is (= (r "foo") ["foo" ""]))
    (is (= (r " \t\n foo") ["foo" ""]))
    (is (= (r "foo-") ["foo" "-"]))
    (is (nil? (r "bar")))
    (is (nil? (r "xfoo")))
    (is (nil? (r "-foo")))
    (is (nil? (r "foox")))
    )
  (let [r (rule "[")]
    (is (= (r "[") ["[" ""]))
    (is (nil? (r "x")))
    ))

(defn argcount [& args]
  (count args))
  
(deftest test-regex-rule
  (testing "No handler provided."
    (testing "With no groups in the regex, the result is a string."
      (let [r (rule #"foo")]
        (is (= (r "foo") ["foo" ""]))
        ))
    (testing "With groups in the regex, the result is a vector of strings."
      (let [r (rule #"f(o+)")]
        (is (= (r "foo") [["foo" "oo"] ""]))
        )))
  (testing "With handler function."
    (testing "With no groups in the regex, the handler should be passed a single string argument."
      (let [r (rule #"foo" argcount)]
        (is (= (r "foo") [1 ""]))
        ))
    (testing "With groups in the regex, handler should receive multiple argument strings."
      (let [r (rule #"f(o+)" argcount)]
        (is (= (r "foo") [2 ""]))
        ))))

(deftest test-vec-rule
  (let [r (rule [#"foo\b" #"bar"])]
    (is (nil? (r "foobar")))
    (is (= (r "foo bar") [["foo" "bar"] ""]))
    ))
    
(deftest test-set-rule
  (let [r (rule #{"foo" "bar"})]
    (is (= (r "foo") ["foo" ""]))
    (is (= (r "bar") ["bar" ""]))
    ))
    
(deftest test-apply-modifiers
  (let [r (apply-modifiers ["foo" :+ "bar"])]
    (is (= 2 (count r)))))

    
(deftest test-foobar-1
  (let [p (parser {
          :root  [:foo :bar]
          :foo   "foo"
          :bar   "bar"
          })]
    (is (= (p "foo bar") ["foo" "bar"]))
    ))

(deftest test-foobar-2
  (let [p (parser {
          :root  [:foo* :bar?]
          :foo   "foo"
          :bar   "bar"
          })]
    (is (= (p "foo bar") [["foo"] "bar"]))
    (is (= (p "bar") [[] "bar"]))
    (is (= (p "foo foo bar") [["foo" "foo"] "bar"]))
    (is (= (p "") [[] nil]))
    (is (= (p "foo foo") [["foo" "foo"] nil]))
    ))

(deftest test-foobar-3
  (let [p (parser {
          :root  [:foo :bar]
          :foo   (rule "foo" upper-case)
          :bar   "bar"
          })]
    (is (= (p "foo bar") ["FOO" "bar"]))
    ))

  
(defparser json-parser
  :root     #{:array :object}
  :array    ["[" (list-of :any) "]"]    vec
  :any      #{:array :object :number 
              :string :bool :null}
  :object   ["{" :kvpairs? "}"]         #(into {} %)
  :kvpairs  [:kvpair ["," :kvpair]:*]   #(concat [%1] %2)
  :kvpair   [:string ":" :any]          vector
  :number   #"\d+|\d*\.\d+"             #(Double/valueOf %)
  :string   #"\"([^\"]*)\""             (fn [_ s] s)
  :bool     #{"true" "false"}           {"true" true "false" false}
  :null     "null"                      (fn [_] nil)
  )

(deftest test-json
  (let [p json-parser]
    (is (= (p "{}") {}))
    (is (= (p "[]") []))
    (is (= (p "[23]") [23.0]))
    (is (= (p "[23,]") nil))
    (is (= (p "[null, true, false]") [nil true false]))
    (is (= (p "[{}]") [{}]))
    (is (= (p "{\"foo\": 23}") {"foo" 23.0}))
    (is (= (p "{\"foo\": 23, \"bar\": [\"hello\", \"world\"]}")
           {"foo" 23.0 "bar" ["hello" "world"]}))
    ))


(defparser sql-parser
  :root       #{:select :insert :update :delete}
  :select     ["select" :cols "from" :tablename :order-by?]
                (fn [cols tbl order] {
                  :type :select
                  :cols cols
                  :table tbl
                  :order order
                  })
  :cols       [:colname ["," :colname]:*]
                (fn [col cols]
                  (concat [col] cols))
  :colname    #"\w+"
  :tablename  #"\w+"
  :order-by   ["order" "by" [:colname #{"asc" "desc"}:?]]
                (fn [[colname dir]]
                  [colname (keyword (or dir "asc"))])
  :insert     "insert TODO"
  :update     "update TODO"
  :delete     "delete TODO"
  )

(deftest test-sql
  (is (= (sql-parser "select foo, bar from blah order by foo")
         {:type :select
          :cols ["foo" "bar"]
          :table "blah"
          :order ["foo" :asc]})))


(defrecord Element [tag attrs content])

(defparser xml-parser
  :root          :element
  :any           #{:element :text} ; TODO - comment, PI, ...
  :element       #{:empty-elt :nonempty-elt}
  :empty-elt     [:tagstart "/>"]
  :nonempty-elt  [:tagstart ">" :any* :endtag]
                  (fn [e content endtag]
                    (if (not= (:tag e) endtag)
                      (parse-error (str "Closing tag " endtag " does not match start tag " (:tag e)))
                      (assoc e :content content)))
  :tagstart      ["<" :name :attr*]
                  (fn [tagname attrs] 
                    (Element. tagname (into {} attrs) []))
  :endtag        ["</" :name ">"]
  :attr          [:name "=" :quoted-string]
  :name          #"\w+"
  :quoted-string #"\"([^\"]*)\""
                   (fn [_ s] s)
  :text          #"[^<]+"
  )

(deftest text-xml
  (let [xml "<foo/>"]
    (is (= (xml-parser xml) (Element. "foo" {} []))))
  (let [xml "<foo></foo>"]
    (is (= (xml-parser xml) (Element. "foo" {} []))))
  (let [xml "<foo><bar/><bar/></foo>"
        bar (Element. "bar" {} [])]
    (is (= (xml-parser xml) (Element. "foo" {} [bar bar]))))
  (let [xml "<foo><bar baz=\"boo\" baz2=\"blah\" /></foo>"]
    (is (= (xml-parser xml) (Element. "foo" {} [(Element. "bar" {"baz" "boo" "baz2" "blah"} [])]))))
  (let [xml "<foo>hello</foo>"]
    (is (= (xml-parser xml) (Element. "foo" {} ["hello"]))))
  )










