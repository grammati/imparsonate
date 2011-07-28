# Imparsonate

Imparsonate is a library for parsing structured text in Clojure.

## Examples

Here is a JSON parser (well, almost... close enough for an example):

```clojure
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

    (json-parser "{\"foo\": 23, \"bar\": [42, null, true]}")
    ;; => {"foo" 23 "bar" [42 nil true]}
```
    
Let's walk through that example step by step.

The `defparser` macro is, unsurprisingly, mostly equivalent to `(def name (parser ...))` (but not
quite).

`parser` returns a function that takes text as input, attempts to match it against the rules, and
returns the parsed result (or throws an exception if it does not match).

The body of the parser definition consists of any number of _rules_, each of which consists of
exactly two or three parts:

* A keyword naming the rule
* A rule definition, which can be one of several types (see below)
* Optionally, a handler function, for turning the matched text into something useful.

I'll walk through the JSON example and explain the rules one at at time.

First, there must be a rule named :root - the parser will always start with that. Note that when I
say "first", I mean logically - the rules can actually be listed in any order (but it makes sense
to put the root rule at the top).

In the case of our JSON parser, the root rule is `#{:array :object}`. This introduces our first
rule type: sets. A set is an _or_ rule - the parser will attempt to match each of the sub-rules
contained in the set, and expect exactly one of them to match. In JSON, the root of a document
must be either an array or an object (that's a map, in Clojure terms).

The sub-rules inside our root rule are keywords, which are our second rule type. A keyword is a
reference to another rule. So when we see `#{:array :object}`, we know to look further down the
rule list for the definitions of those rules.

Next we come to the rule definition for :array, which is `["[" (list-of :any) "]"]`. This is our
next rule type: vector. A vector is an _and_ rule - each of its sub-rules must match, in order,
for the vector-rule to match. In this case, our rule consists of 3 sub-rules: two strings and
a... something.

String rules, like `"["` and `"]"`, do exactly what you think they do - they match
themselves. There's not much more to say.

The middle part of the `:array` rule is... well, actually, let's not talk about `(list-of :any)`
just yet - but we'll come back to it, I promise.

Finally, the `:array` rule specifies a handler function, "vec". But we haven't talked about
handler functions yet, so forget I said that. We'll come back to that too.

OK, let's skip over the `:any` rule - theres nothing new there - and look at the `:object`
rule. It's pretty much like the `:array` rule, except that the bit between the "{" and "}" is just
the keyword `:kvpairs?`. We already know that a keyword is a reference to another rule, but there
is no rule named `:kvpairs?` - so what's up? If you're familiar with regular expressions, you can
guess - this is a reference to the `:kvpairs` rule, but with a zero-or-one modifier. We need this
because an object-literal in JSON is allowed to be empty, and the :kvpairs rule itself will only
match one or more key-value pairs (not zero).

The `:object` also defines a handler function, so let's talk about that now. A handler is a
function that takes the matched part of the input and returns something useful. If you don't
supply a handler, it defaults to `identity` (sort-of - see below).

The handler for the :object rule is `#(into {} %)`. Why?. To understand, we need a bit of
background on how imparsonate calls handlers. The arguments to the handler depend on the type of
the rule. In this case, we have a vector rule. In general, a vector rule that contains N sub-rules
will pass N arguments to the handler when it successfully matches. But there is an exception; any
sub-rule that is a string is omitted from the argument list. So in this case, rather than passing
three arguments to the handler we only pass one. The first and third arguments, which would always
be exactly "[" and "]" respectively, are omitted. The one remaining argument is the result of
matching the second of the three sub-rules, which is `:kvpairs?`.

So to understand why `#(into {} %)` works, we need to know what the argument is going to be - that
is, we need to know what the `:kvpairs?` rule will return. That rule is defined as `[:kvpair [","
:kvpair]:*]`. That looks a little odd. But really, it's just a vector with 3 items in it - a
keyword, a vector, and another keyword. The keyword `:*` is special - when found inside a
vector-rule, it is interpreted by imparsonate as a modifier for the preceding rule. There are 3
built-in modifiers: `:*`, `:+`, and `:?`, meaning zero-or-more, one-or-more, and zero-or-one
respectively. In fact, there are functions in imparsonate with those names, and you can use them
directly if you prefer. So, for example, all of the following are equivalent rule definitions: *
`[:foo+ :bar?]` * `[:foo :+ :bar :?]` * `[(one-or-more :foo) (zero-or-one :bar)]`

Okay, where were we? Oh yeah, we're trying to understand how the `:object` handler, `#(into {}
%)`, works. So far we have seen that it receives the value returned by `:kvpairs?`, which we now
know means `(zero-or-one :kvpairs)`, which returns either `nil` (for the _zero_ case), or the
result of matching the rule `:kvpairs` (for the _one_ case).

The `:kvpairs` rule calls its handler, which is `#(concat [%1] %2)`, with two arguments. The first
is the return value of the `:kvpair` rule. The second is the result of matching `(zero-or-more
["," :kvpair])`, which is a sequence of results of matching the rule `["," :kvpair]`. Pop quiz:
what does that rule return? Recall two things that we talked about above - that the default
handler is `identity`, and that a vector rule calls its handler with one argument for each
non-string-literal member. So the rule discards the "," and calls `identity` with one argument
(which is good because identity only accepts one argument) - the return value of the `:kvpair`
rule. So our handler receives a key-value pair as its first argument and a sequence (possibly
empty) of key-value pairs as its second, and squishes them together into one sequence.

We're almost there. Let's look at the `:kvpair` rule, which we can now breeze through. It matches
a `:string' and an `:any` separated by a colon, and passes them to `vector`, yielding a
two-element vector (remember, the ":" is discarded).

Let's walk back through those steps from the bottom up. `:kvpair` gives us a two-element
vector. `:kvpairs` gives a sequence of same. `:kvpairs?` gives us said sequence, or nil. And
finally, we can see that the handler for `:object`, #(into {} %) builds a map from those
two-elemnt vectors. Note that `(into {} nil)` is valid, and returns an empty map, so no special
handling is needed for that case.

The rest is easy - we have a couple of regular expression rules for `:number` and `:string`. A
regex rule calls its handler with one argument for the matching text, and one for each group in
the regex. By the way, these rules are the reason I say this is "almost" a JSON parser - for the
purpose of this example I have glossed over several details of JSON, like scientific notation for
numbers and backslash escapes for strings.

All that's left is to talk about `list-of`, as I promised. So, you remember all the shenanigans
involved in matching a possibly-empy comma-separated list of key-value pairs? Wouldn't it be nice
to wrap that messiness up in a function? That's what `list-of` is. `(list-of :foo)` just means
`(zero-or-more [:foo ["," :foo] :*])`, along with a handler that wraps up the result into a single
sequence. So really we should have defined `:object` using `(list-of :kvpair)` - but that would
have been cheating for an introductory example.


## Documentation

Detailed documentation of rules and handlers.

FIXME

## Whitespace

Handling of whitespace is hard-coded right now. That should change, and become more configurable,
but for now it seems to work well enough for well-formed input in all the cases I have tried. It
would very likely match bad input in many cases too :(

The current behavior is: 

* leading whitespace will be consumed before matching any rule 
* whitespace is never required unless you explicitly require it (eg: with a regex rule) 
* a string rule will only match if it ends at a word boundary (eg: the rule "foo" will match
"foo-bar", but not "foobar").





