# Rusk: a KML dialect

## Usage

```
rusk [--debug] file
```

read the file and generate a markdown document.

## Example

```
event greet@(do greeting);

state Greeting@(greeting)
{
	var greeted: Boolean = false;
	transition greet
		when greeted == false @[- not yet -]
		-->
	{
		post {
			target greeted;
			greeted' = true;
			state' = Bye;
		}@{-
			Hello!
		-}
	}
}

state Bye@(bye!)
{
	// ...
}
```

## Syntax

```
<start> = module ;;

module = event state* ;;

event = "event" (event-item <sep-end-by ",">)* ";" ;;
event-item = event-name at-summary arg-list? ;;
event-name = <dotted-name> ;;

state = "state" state-name at-summary arg-list?
          "{" state-element* "}" ;;
state-name = <dotted-name> ;;
state-element = var | invariant | transition ;;

var = "var" <identifier> ":" type-expr "=" expr ";" ;;

invariant =
  "invariant" "{"
    ( (expr "=>")? expr ";")+
  "}" at-description-invariant ;;

transition =
  "transition" event-name arg-list?
  ("when" expr at-summary-guard)?
  "-->" "{"
    "post" "{"
      ("target" (target-name <sep-end-by ",">)+ ";")?
      (target-name "=" expr ";")+
    "}" at-description-post-cond
  "}" ;;
target-name = <identifier followed by "'"> ;;


arg-list = "(" (arg <sep-end-by ",">)+ ")" ;;
arg = <identifier> ":" type-expr ;;

at-summary = "@" "(" <free-text> ")" ;;
at-summary-guard = "@" "[-" <free-text> "-]" ;;
at-description-invariant = "@" "{" <free-text> "}" ;;
at-description-post-cond = "@" "{-" <free-text> "-}" ;;

line-comment = "//" <free-text except '\n'> ;;


type-expr = expr ;;

expr = expr "+" expr
     | expr "-" expr
     | expr "*" expr
     | expr "/" expr
     | expr "++" expr
     | expr "==" expr
     | expr "/=" expr
     | expr "<" expr
     | expr "<=" expr
     | expr ">" expr
     | expr ">=" expr
     | expr "&&" expr
     | expr "||" expr
     | "(" expr ")"
     | expr expr
     | expr "." <identifier>
     | term

term = <identifier> | <integer> | <dq-string>
     | case-expr
     ;;

case-expr =
  "case" expr "{"
  (expr ("when" expr)? "=>" expr <sep-end-by ";">)+
  "}"
  ;;

<dq-string> = "\"" <free-text> "\"" ;;
<dotted-name> = (<identifier> <sep-by ".">)+ ;;
```

## TODO

* integrated summary text: `"@" "[" <free-text> "]"`
* integrated description text: `"@" "{-" <free-text> "-}"`
* list expression:  `"[" (expr <sep-end-by ",">)* "]"`
* tuple expression:  `"(" ")" | "(" expr "," (expr <sep-end-by ",">)* ")"`
* if expression:  `"if" expr "{" expr "}" "else" "{" expr "}"`
<!-- (ternary expr:) `expr ? expr : expr`
<!-- (if_else method+lambda:) `bool-expr.if_else (\-> expr) (\-> expr)` -->
* object expression:  `expr "{" (<identifier> "=" expr ";")+ "}"`
* object definision: `"object" "{" (object-event | var)* "}"`
* let expression: `"let" (<identifier> "=" expr <sep-end-by ",">)+ "in" expr`
* semantic check
* function definision: `"fn" <identifier> arg-list "->" type-expr ...`
* type definision: `"type" <identifier> <identifier>* "=" type-expr`
<!-- * type check -->
* sphinx generator
* json generator
* HTML henerator

## References

### KML

* 《日経Robo》ファナックが買収したロボベンチャー、ソフトの技術力で群を抜く
  仕様を厳密に記述する「形式手法」を実践、約2万行を記述
  https://tech.nikkeibp.co.jp/dm/atcl/mag/15/00140/00017/?P=2
* 協働ロボットCOROの開発における形式的仕様記述KMLの開発と適用
  https://www.slideshare.net/liferobotics/corokml
* KML文法を思い出す
  https://github.com/minekoa/til/blob/master/formalmethod/kml/example.md

## ChangeLog

### Release 0.1.1 (2019-11-07)

Improvement.
Bug Fix

### Release 0.1.0 (2019-11-04)

Initial release.
