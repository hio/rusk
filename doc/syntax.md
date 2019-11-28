---
title: Rusk Syntax
---

(outdated. need to be updated.)

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
    ( (expr "=>")? expr ";")+
  "}" at-description ;;

transition ::=
  "transition" event-name arg-list?
  ("when" expr at-guard-description)?
  "-->" "{"
    "post" "{"
      ("target" (target-name <sep-end-by ",">)+ ";")?
      (expr \<sep-end-by ";"\>)+
    "}" at-description
  "}" ;;
target-name ::= <identifier followed by "'"> ;;


arg-list ::= "(" (arg <sep-end-by ",">)+ ")" ;;
arg ::= <identifier> ":" type-expr ;;

at-alias ::= "@" "(" <free-text> ")"
           | "@" "(-" <free-text> "-)"
         ;;
at-guard-description ::= "@" "[-" <free-text> "-]" ;;
at-description ::= "@" "{-" <free-text> "-}" ;;


type-expr ::= <<expr>> ;;

case-expr ::=
  "case" <expr exclude record syntax> "{"
  (expr ("when" expr)? "=>" expr <sep-end-by ";">)+
  "}"
  ;;

if-expr ::=
  "if" <expr excludes record syntax>  "{" expr "}"
  ("else" "{" expr "}")?
  ;;

<dq-string> ::= "\"" <free-text> "\"" ;;
<dotted-name> ::= (<identifier> <sep-by ".">)+ ;;
```


## Operators and Precedence

1. term (highest precedence)
    * identifier
    * number
    * string: \<dq-string\>
    * paren: "(" expr ")"
    * list literal: "[" ... "]"
    * set literal: "__set" "{" ... "}"
    * channel set: "{|" expr "|}"
    * target event set: "|[" expr "]|"
    * compound expr \
        * \<case-expr\>
        * let expr
        * \<if-expr\>
        * record mutation with inside syntax: "{" expr "|" \<idenfifier\> "<-" expr "," ... "}"
        * quantifier expr: ("exists" | "exists1" | "forall") (expr \<sep-end-by ","\>)+ "|" expr
    * any keyword
    * given keyword
    * state keyword
2. [quasi-term] property
    * left assoc: expr "." expr
3. [quasi-term] index
    * left assoc: expr "!!" expr
4. [quasi-term] record construction
    * no assoc: expr "{" \<idenfifier\> "=" expr "," ... "}"
5. [quasi-term] as-a
    * left assoc: expr ":" expr
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
    * CSP prefix operation, no assoc: event "+>" process
    * chan ::= expr;;
    * event ::= expr;;
    * process ::= expr;;
13. material conditional
    * left assoc: expr "=>" expr
14. logical conjunction
    * left assoc: expr "&&" expr
15. logical disjunction
    * left assoc: expr "||" expr
16. pipe (lowest precedence)
    * left assoc: expr "|>" expr
    * f a |> g b == g b (f a)


## Literals

* list literal
	* "[" (expr \<sep-end-by ","\>)\* "]"

* list comprehension from collection:
	* "[" expr "|" expr "in" collection "&" ... "]"
	* TODO: "[" expr "|" expr "<-" collection "," ... "]"
	* collection ::= expr;;

* list comprehension from type:
	* "[" expr "|" expr ":" type "&" ... "]"
	* type ::= expr;;


* set literal
	* "__set" "{" (expr \<sep-end-by ","\>)\* "}"
