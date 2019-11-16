---
titie: Standard Library
---

# Standard Library

## Expressions

### Literals

Int: ``0``, ``1``, ``2``, ....

String: ``"hello"``.

List: ``[]``, ``[1]``, ``[1,]``, ...

Identifier: ``name``, ``hello_world``, ``_``, ``_unused``, ...


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


## type List a

### list.head: Maybe a

### list.is\_empty: Bool

### list.length: Int

### list.map (f:(fn (a:a): b)): List b

### list.tail: Maybe (List a)


## type Maybe a

### Just a: Maybe a

### Nothing: Maybe _

### maybe.map (f:(fn (a:a): b)): Maybe b
