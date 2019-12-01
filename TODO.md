---
title: TODO
---

## Tasks

### Near Future

- TD-162: Doc::PlainText, MarkedText
- TD-150: list comprehension [x | elem <- collection] rather than [x | elem in collection].
- TD-104: tau (arg) ... guard -\->

### Misc

- RF-139: allow space chars between @ and {- -}
- TD-149: given keyword rather than an identifier.
- TD-103: module description.
- TD-105: fn definition: fn f (arg) ...: t = body;
- TD-107: when a @[- desc -], b @[- desc -, --> { ... }
- TD-126: allow single paren in @(- -)
- RF-122: drop escapes by (( and ))
- TD-161: split up KwOper `in`.
- RF-115: Option<Box<String>> rather than Box<Option<String>> (31)

### Backlog

- TD-136: remove indentation in description.
- TD-114: nested anon record def.
- RF-116: sort scanner::KeywordKind by alphabetical
- RF-117: sort scanner::PunctuationKind by alphabetical
- RF-118: sort scanner::OperatorKind by alphabetical
- TD-119: @(((- -))) or @(-\-- -\--)for fewer escape.
- TD-120: description for containing block. e.g. state{@{-...-}...}
- TD-124: if/then/else rather than if cond {} else {}
- TD-125: case x|arm... rather than match x{arm...}
- TD-127: unicode block, mathematical operators
- TD-128: ReST formatter
- TD-135: Doc encode option/never twice same decoration.
- TD-137: parse markdown in description.
- TD-138: set serde attributes for json formatter.
- TD-140: tuple expression:  `"(" ")" | "(" expr "," (expr <sep-end-by ",">)* ")"`
- TD-141: ternary expr `expr ?? expr !! expr`
- TD-142: semantic check.
- TD-143: type check.
- TD-144: HTML formatter.
- TD-145: use clap cli parser.
- TD-146: use pulldown-cmark markdown parser.
- TD-147: try combine again without impl type.
- TD-148: phrase description as a lowest prec expr.
- TD-152: rename summary to alias.
- RF-153: simplify rs code { x:x, } into just { x, }
- TD-154: binary operator for CSP sequencial composition alternative of `[]`.

blocked/blocks:

- TD-123: alternative operator for p [] q
- TD-109: event set without pragma.
- TD-110: event item, @//, \
  (try) `sep ::= "," <@//>?`, \
  (rethink) `item }`
- TD-111: field, @//
- TD-112: type ; @//
- TD-113: f:T @// ,

## Rethinks

* `=`, `==` and `<-`
    * comparison: 1 == 1
    * binding: x == get\_something
    * pattern match: Just _ == maybe
    * binding with destruction: Just elem == list !! ix
    * record construction: Type { field = init } \
      Type { field: init }?
      Type { field := init }?
    * record mutation: obj { field <- new\_value } \
      obj { field <-: new\_value }
      obj { field: new\_value }
      obj { field := init }?
      obj { field = new\_value } \


## Done

- (newer)
- TD-134: Doc encode option/no line-feed in table cell.
- TD-163: RenderingBox
- TD-164: Doc::Table/formatted width
- TD-129: Doc::Table/introduce
- TD-132: Doc encode option/introduce.
- TD-133: Doc encode option/no backslash escape in code.
- TD-130: text width
- TD-158: \_\_map { k |-> v }
- TD-159: add doc for scope (doc/syntax.md).
- TD-160: add use statement (doc/syntax.md).
- TD-108: library. vocabulary.
- TD-157: implement prefix op (->) rather than user defined op.
- TD-156: split up PunctOper `=>`, `:`, `->`.
- TD-155: function expr: `fn (arg) ...: t -> body`
- TD-121: update doc/syntax.md.
- TD-151: support post { cond }.
- TD-131: set pragma
- TD-106: type a = b; format should be "b" rather than "b |"
- TD-102: type t = a b
- TD-101: type t a
- OK: state description, `state {}@{- ... -}`
- OK: transition name (arg) (arg:type) ...  // type annotation is optional
- OK: target state
- OK: var name:type
- OK: type x = a | b
- OK: forall , exists, exists1
- OK: process operation
- OK: channel set, target event set.
- OK: event a | b
- OK: invariant { expr; }
- OK: operator =>
- OK: keyword any. global = any;
- OK: [ f a | a : S & a < threshold ]
- OK: [ f a | a in s & a < threshold ]
- OK: newlines in desc
- OK: no \ esc in ``
- (older)
