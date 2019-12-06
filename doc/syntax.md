---
title: Rusk Syntax
---

```
<start> ::= module ;;

line-comment ::= "//" <free-text except '\n'> ;;
block-comment ::= "/*" <free-text> "*/" ;;


module ::= (event-stmt | type-stmt | var-stmt | const-stmt
            | invariant-stmt | state-stmt)* ;;

event-stmt ::= "event" (event-item <sep-end-by ",">)* ";" ;;
event-item ::= event-name at-alias arg-list? ;;
event-name ::= <dotted-name> ;;

type-stmt ::= not-documented-yet ;;

var-stmt ::= not-documented-yet ;;

state-stmt ::= "state" state-name at-alias arg-list?
                "{" state-element* "}" ;;
state-name ::= <dotted-name> ;;
state-element ::= var-stmt | invariant-stmt | transition-stmt | tau-stmt ;;

var-stmt ::= "var" <identifier> ":" type-expr "=" expr ";" ;;
const-stmt ::= "const" <identifier> ":" type-expr "=" expr ";" ;;

invariant-stmt ::=
  "invariant" "{"
    ( cond-expr ";")+
  "}" at-stmt-description ;;

transition-stmt ::=
  "transition" event-name arg-list?
  ("when" (cond-expr at-phrase-description <sep-end-by ",">)+)?
  "-->" "{"
    post-cond+
  "}" ;;

post-cond ::=
  "post" "{"
    ("target" (target-name <sep-end-by ",">)+ ";")?
    (cond-expr \<sep-end-by ";"\>)+
  "}" at-stmt-description
  ;;

target-name ::= <identifier> | "state" ;;
result-name ::= <identifier followed by "'"> | "state'" ;;
// result-name can be used at cond-expr in post-cond.

arg-list ::= arg <sep-end-by ",">+ ;;
arg ::= <identifier>
      | "(" expr ")"
      | "(" expr ":" type-expr ")"
    ;;

tau-stmt ::=
  "tau" arg-list?
  ("when" (cond-expr at-phrase-description <sep-end-by ",">)+)?
  "-->" "{"
    post-cond+
  "}" ;;

at-alias ::= "@" "(" <free-text> ")"
           | "@" "(-" <free-text> "-)"
         ;;
at-phrase-description ::= "@" "[-" <free-text> "-]" ;;
at-stmt-description ::= "@" "{-" <free-text> "-}" ;;
at-line-description ::= "@" "//" <free-text except '\n'> "-}" ;;


cond-expr ::= <expr> ;;
type-expr ::= <expr> ;;

case-expr ::=
  "case" <expr exclude record syntax> "{"
  (expr ("when" cond-expr)? "=>" expr <sep-end-by ";">)+
  "}"
  ;;

if-expr ::=
  "if" <cond-expr excludes record syntax>  "{" expr "}"
  ("else" "{" expr "}")?
  ;;

let-expr ::=
  "let"
  (<identifier> "=" <expr exclude "in" binary operator> <sep-end-by ",">)+
  "in" expr ;;

<dq-string> ::= "\"" <free-text> "\"" ;;
<dotted-name> ::= (<identifier> <sep-by ".">)+ ;;
```


## Operators and Precedence

1. term (highest precedence)
    * identifier
    * number
    * string: \<dq-string\>
    * paren: "(" expr ")"
    * list literal: "\[" ... "]"
    * set literal: "__set" "{" ... "}"
    * map literal: "__map" "{" ... "}"
    * channel set: "{|" expr "|}"
    * target event set: "|[" expr "]|"
    * compound expr \
        * \<case-expr\>
        * \<let expr\>
        * \<if-expr\>
        * record mutation with inside syntax: "{" expr "|" \<identifier\> "<-" expr "," ... "}"
        * quantifier expr: ("exists" | "exists1" | "forall") ((gen-expr | filter-expr) \<sep-end-by ","\>)+ "|" cond-expr
            * gen-expr ::= expr ;; // elem <- collection, elem : Type
            * filter-expr ::= cond-expr ;;
    * any keyword
    * given keyword
    * state keyword
2. [quasi-term] property
    * left assoc: expr "." expr
3. [quasi-term] index (get nth element)
    * left assoc: expr "!!" expr
4. [quasi-term] record construction
    * record construction, no assoc: expr "{" \<identifier\> "=" expr "," ... "}"
    * record mutation with outside syntax, left assoc: expr "{" \<identifier\> "<-" expr "," ... "}"
5. [quasi-term] as-a
    * left assoc: expr ":" type-expr
6. application
    * left assoc: expr expr
7. user defined binary operator
    * left assoc: expr \<user-op\> expr
    * left assoc: expr \`\<identifier\>\` expr
8. mutiplication
    * left assoc: expr "\*" expr
    * left assoc: expr "/" expr
9. addition
    * left assoc: expr "+" expr
    * left assoc: expr "-" expr
10. concatenation, exclusion.
    * left assoc: expr "++" expr
    * left assoc: expr "--" expr
11. comparison
    * no assoc: expr "==" expr
    * no assoc: expr "/=" expr
    * no assoc: expr "<" expr
    * no assoc: expr "<=" expr
    * no assoc: expr ">" expr
    * no assoc: expr ">=" expr
    * no assoc: expr "in" expr
12. material conditional
    * left assoc: cond-expr "=>" cond-expr
13. logical conjunction
    * left assoc: cond-expr "&&" cond-expr
14. logical disjunction
    * left assoc: cond-expr "||" cond-expr
15. pipe
    * left assoc: expr "|>" expr
    * f a |> g b == g b (f a)
16. channel communication
    * receive a messsage, no assoc: chan "?" expr
    * send a messsage, no assoc: chan "!" expr
    * chan ::= expr ;;
17. process communication (lowest precedence)
    * CSP prefix operation, right assoc: event "->" process
    * CSP sequential composition, left assoc: process ";>>" process
    * event ::= expr ;;
    * process ::= expr ;;


## Literals

* list literal
	* "[" (expr \<sep-end-by ","\>)\* "]"

* list comprehension
	* generator + filter + map.
	* from collection: "[" expr "|" expr "<-" collection "," ... "," cond-expr "," ... "]"
	* from type: "[" expr "|" expr ":" type-expr "," ... "," cond-expr ... "]"
	* collection ::= expr;;
	* type-expr ::= expr;;


* set literal
	* "__set" "{" (expr \<sep-end-by ","\>)\* "}"  <!--* -->

* map literal
	* "__map" "{" (expr "|->" expr \<sep-end-by ","\>)\* "}"


## Name Resolution

1. Local Objects in each brace blocks `{ ... }`.
2. Module Level Objects.
3. Names which are imported by `use` statement.
3. Modules in same directory as module itself contained.
4. Objects in prelude module in same directory as module itself contained.
5. std Objects.

## Module Sample

```
// hello.rsk
use message { Hello };
use message;
fn greet (name:String) =
	name ++ ": " ++
	Hello ++ ", " ++ message.World ++ "!";
```

```
// message.rsk
const Hello = "hello"
const World = "world";
```
