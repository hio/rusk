---
title: Standard Library
---

## Expressions

### Literals

Int: ``0``, ``1``, ``2``, ....

String: ``"hello"``.

Identifier: ``name``, ``hello_world``, ``_``, ``_unused``, ...

List: ``[]``, ``[1]``, ``[1,]``, ...

Set: ``__set {}``, ``__set { 1, 2, 3, }``

Map: ``__map {}``, ``__map { 1 |=> "one", 2 |=> "two", }``

### Operators

``(int + int): int``, ``(int - int): int``, ``(int * int): int``,
``(int / int): ???``, ``(int ** int): int``.

``(int == int): bool``, ``(int /= int): bool``,
``(bool == bool): bool``, ``(bool /= bool): bool``,
``(str == str): bool``, ``(str /= str): bool``,
``(list == list): bool``, ``(list /= list): bool``,

``(int < int): bool``, ``(int <= int): bool``,
``(int > int): bool``, ``(int >= int): bool``.

``(str !! ix): char``, ``(list !! ix): a``, get nth element.

``(str ++ str): str``, ``list ++ list``, concatenate.

``(bool && bool): bool``, ``(bool || bool): bool``, ``(bool => bool): bool``.

### Misc

``( expr )``

``let x = expr, ... in expr``,
``let x = expr, ..., in expr``

``if cond { then_clause } else { else_clause }``

``case expr { guard => clause, ... }``
``case expr { guard => clause, ..., }``

``Record { field: expr, ... }``
``Record { field: expr, ..., }``

``record { field <- expr, ... }``
``record { field <- expr, ..., }``

## type Int

arbitrary-precision arithmetic.
should be set range.


## type Bool

``true``, ``false``.

## type String

Unicode char string.
Same as ``List Char``.


## type Maybe A

### Just a: Maybe A

### Nothing: Maybe _

### maybe.map (f:(fn (a:A): B)): Maybe B


## type List A

### (list !! (index:Int)): Maybe A

get the n-th element.

### list.contains (a:A): bool

### list.head: Maybe A

### list.is\_empty: Bool

### list.length: Int

### list.map (f:(fn (a:A): B)): List B

### list.tail: Maybe (List A)

## type Set A

### (set !! (a:A)): Maybe A 

### set.card: Int

cardinarity of the set.

## type Map K V

### (map !! (k:K)): Maybe V 

### map.card: Int

cardinarity of the map.

### map.keys: Set K

### map.values: Set V
