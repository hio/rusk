---
title: TODO
---

## Tasks

### Near Future

- TD-169: phrase comment { f : T @[- .. -], } at record field, etc.

### Misc

- TD-171: @// style description. (seealso [#Rethinks](#rethinks))
- TD-149: given keyword rather than an identifier.
- TD-103: module description. @!{- ... -}
- TD-126: allow single paren in @(- -)
- RF-122: drop escapes by (( and ))
- TD-161: split up KwOper `in`.
- TD-128: ReST formatter
- TD-168: non functional requirements.
- TD-176: auto detection of set, map, record def.
- TD-140: tuple expression:  `"(" ")" | "(" expr "," (expr <sep-end-by ",">)* ")"`
- TD-166: split up doc/lib.md into each types.
- TD-177: op fn expr, e.g. `(+)`
- TD-178: named op expr, e.g. `` `func` ``
- TD-179: fn stmt for op: `fn (+) a b`

### Backlog

- RF-116: sort scanner::KeywordKind by alphabetical
- RF-117: sort scanner::PunctuationKind by alphabetical
- RF-118: sort scanner::OperatorKind by alphabetical
- TD-119: @(((- -))) or @(-\-- -\--)for fewer escape.
- TD-120: description for containing block. e.g. state{@^{-...-}...}
- TD-124: if/then/else rather than if cond {} else {}
- TD-125: case x|arm... rather than match x{arm...}
- TD-127: unicode block, mathematical operators
- TD-135: Doc encode option/never twice same decoration.
- TD-137: parse markdown in description.
- TD-138: set serde attributes for json formatter.
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
- TD-167: column position of first line in description.
- TD-175: fn stmt in state local.

## Rethinks

- TD-123: alternative operator for external choices (p [] q)
- TD-109: event set without pragma.
- TD-110: event item, @//, \
  (try) `sep ::= "," <@//>?`, \
  (rethink) `item }`
- TD-111: record field, @//
- TD-112: type ; @//
- TD-113: f:T @// ,

- TD-170: conditions and semantics of equations
    - `=`, `==` and `<-`
    - comparison: 1 == 1
    - binding: x == get\_something
    - pattern match: Just _ == maybe
    - binding with destruction: Just elem == list !! ix
    - record construction: Type { field = init } \
      Type { field: init }?
      Type { field := init }?
    - record mutation: obj { field <- new\_value } \
      obj { field <-: new\_value }
      obj { field: new\_value }
      obj { field := init }?
      obj { field = new\_value } \


## Done

- (newer)
- TD-165: move comm ops to lowest prec.
- TD-174: concat multiple post conditions vertically in output doc.
- TD-173: support {} for empty set/map.
- TD-105: fn definition: fn f (arg:type) ...: t = body;
- TD-172: fn declaration: fn f (arg:type) ...: t;
- TD-104: tau (arg) ... guard -\->
- TD-114: nested anon record def.
- RF-115: Option<Box<String>> rather than Box<Option<String>> (31)
- TD-136: remove indentation in description.
- TD-107: when cond\_1 @[- desc 1 -], cond\_2 @[- desc 2 -], --> { ... }
- TD-150: list comprehension [x | elem <- collection] rather than [x | elem in collection].
- RF-139: allow space chars between @ and {- -}
- TD-162: Doc::PlainText, MarkedText
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
