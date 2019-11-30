---
title: Rusk Syntax
---

```
<start> ::= module ;;

line-comment ::= "//" <free-text except '\n'> ;;
block-comment ::= "/*" <free-text> "*/" ;;


module ::= event state* ;;

event ::= "event" (event-item <sep-end-by ",">)* ";" ;;
event-item ::= event-name at-alias arg-list? ;;
event-name ::= <dotted-name> ;;

state ::= "state" state-name at-alias arg-list?
            "{" state-element* "}" ;;
state-name ::= <dotted-name> ;;
state-element ::= var | invariant | transition ;;

var ::= "var" <identifier> ":" type-expr "=" expr ";" ;;

invariant ::=
  "invariant" "{"
    ( cond-expr ";")+
  "}" at-stmt-description ;;

transition ::=
  "transition" event-name arg-list?
  ("when" cond-expr at-phrase-description)?
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
// result-name can be used at cond-expr in post-ocnd.

arg-list ::= "(" (arg <sep-end-by ",">)+ ")" ;;
arg ::= <identifier> ":" type-expr ;;

at-alias ::= "@" "(" <free-text> ")"
           | "@" "(-" <free-text> "-)"
         ;;
at-phrase-description ::= "@" "[-" <free-text> "-]" ;;
at-stmt-description ::= "@" "{-" <free-text> "-}" ;;


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
  "let" (<identifier> "=" <expr exclude "in" binary operator> <sep-end-by ",">)+ "in" expr ;;

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
    * channel set: "{|" expr "|}"
    * target event set: "|[" expr "]|"
    * compound expr \
        * \<case-expr\>
        * \<let expr\>
        * \<if-expr\>
        * record mutation with inside syntax: "{" expr "|" \<identifier\> "<-" expr "," ... "}"
        * quantifier expr: ("exists" | "exists1" | "forall") (expr \<sep-end-by ","\>)+ "|" cond-expr
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
12. communication
    * receive a messsage, no assoc: chan "?" expr
    * send a messsage, no assoc: chan "!" expr
    * CSP prefix operation, left assoc: event "->" process
    * chan ::= expr;;
    * event ::= expr;;
    * process ::= expr;;
13. material conditional
    * left assoc: cond-expr "=>" cond-expr
14. logical conjunction
    * left assoc: cond-expr "&&" cond-expr
15. logical disjunction
    * left assoc: cond-expr "||" cond-expr
16. pipe (lowest precedence)
    * left assoc: expr "|>" expr
    * f a |> g b == g b (f a)


## Literals

* list literal
	* "[" (expr \<sep-end-by ","\>)\* "]"

* list comprehension
	* generator + filter + map.
	* TODO: from collection: "[" expr "|" expr "<-" collection "," ... "," cond-expr "," ... "]"
	* from collection: "[" expr "|" expr "in" collection "," ... "," cond-expr "," ... "]"
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
