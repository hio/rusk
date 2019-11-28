---
title: TODO
---

## Backlog

near future:

- TD-104 [-/2 TU] tau (arg) ... guard -->
- TD-105 [-/2 TU] fn f (arg) ...: t = body;

lightweight:

- TD-149 [-/1 TU] given pragma/keywod
- TD-103 [-/1 TU] module description.
- TD-126 [-/- TU] allow single paren in @(- -)
- RF-122 [-/- TU] drop escapes by (( ))
- RF-115 [-/- TU] Option<Box<String>> rather than Box<Option<String>> (31)

in someday:

- RF-139 [-/- TU] allow space chars between @ and {- -}
- TD-107 [-/- TU] when { a; b; }@{- desc -}
- TD-108 [-/- TU] library. vocabulary.
- TD-129 [-/- TU] Doc::Table
- TD-130 [-/- TU] text width
- TD-136 [-/- TU] remove indentation in description.
- TD-114 [-/- TU] nested anon record def.
- RF-116 [-/- TU] sort scanner::KeywordKind by alphabetical
- RF-117 [-/- TU] sort scanner::PunctuationKind by alphabetical
- RF-118 [-/- TU] sort scanner::OperatorKind by alphabetical
- TD-119 [-/- TU] @(((- -))) or @(--- ---)for fewer escape.
- TD-120 [-/- TU] description for containing block. e.g. state{@{-...-}...}
- TD-124 [-/- TU] if/then/else rather than if cond {} else {}
- TD-125 [-/- TU] case x|arm... rather than match x{arm...}
- TD-127 [-/- TU] unicode block, mathematical operators
- TD-128 [-/- TU] ReST formatter
- TD-132 [-/- TU] Doc encode option/introduce.
- TD-133 [-/- TU] Doc encode option/no backslash escape in code.
- TD-134 [-/- TU] Doc encode option/no line-feed in table cell.
- TD-135 [-/- TU] Doc encode option/never twice same decoration.
- TD-137 [-/- TU] parse markdown in description.
- TD-138 [-/- TU] set serde attributes for json formatter.
- TD-140 [-/- TU] tuple expression:  `"(" ")" | "(" expr "," (expr <sep-end-by ",">)* ")"`
- TD-141 [-/- TU] ternary expr `expr ?? expr !! expr`
- TD-142 [-/- TU] semantic check.
- TD-143 [-/- TU] type check.
- TD-144 [-/- TU] HTML formatter.
- TD-145 [-/- TU] use clap cli parser.
- TD-146 [-/- TU] use pulldown-cmark markdown parser.
- TD-147 [-/- TU] try combine again without impl type.
- TD-148 [-/- TU] phrase description as a lowest prec expr.
- TD-150 [-/- TU] impl [x | elem <- collection].
- TD-152 [-/- TU] 

blocked/blocks:

- TD-123 [-/- TU] alternative operator for p [] q
- TD-109 [-/- TU] event set without pragma.
- TD-110 [-/- TU] event item, @//, \
  (try) `sep ::= "," <@//>?`, \
  (rethink) `item }`
- TD-111 [-/- TU] field, @//
- TD-112 [-/- TU] type ; @//
- TD-113 [-/- TU] f:T @// ,

## DONE

- (newer)
- TD-121 [-/2 TU] update doc/syntax.md.
- TD-151 [2/2 TU] support post { cond }.
- TD-131 [1/1 TU] set pragma
- TD-106 [1/1 TU] type a = b; format should be "b" rather than "b |"
- TD-102 [4/1 TU] type t = a b
- TD-101 [0/1 TU] type t a
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
