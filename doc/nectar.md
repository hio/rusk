---
title: Nectar, the Semantics of Rusk
---

in rusk:

* `x`, `y`: variables.
* `a`, `b`, `c`: values.

in nectar:

* $X$, $Y$: sets.
* $a$, $b$, $c$, $e$, $e_x$, $e_y$, $e_f$: elements.
* $\mathbb {X}$: domain of $x$.

## atomic and (full) equivalence relation

e.g. Int.

+----------------------------------+--------------------------------------------------------------+
| rusk                             | nectar                                                       |
+==================================+==============================================================+
| ``const x = a;``                 | $X' = \{a\}$ |
+----------------------------------+--------------------------------------------------------------+
| ``const x = any;``               | $X' = \mathbb{X} \text{ where } \mathbb{X} = \mathrm{dom}(x)$
+----------------------------------+--------------------------------------------------------------+
| ``f x``                          | $\{ e_f(e_x) | e_x \in X, e_f \in F \}$ |
+----------------------------------+--------------------------------------------------------------+
| ``x : Int``                      | $X' = \mathbb{Z}$ |
+----------------------------------+--------------------------------------------------------------+
| ``x == a;``                      | $X' = X \cap \{a\}$
+----------------------------------+--------------------------------------------------------------+
| ``x /= a;``                      | $X' = X \setminus \{a\}$
+----------------------------------+--------------------------------------------------------------+
| ``x == y;``                      | $X' = Y' = X \cap Y$ |
+----------------------------------+--------------------------------------------------------------+
| ``x /= y;``                      | $(X', Y') = \{ (e_x, e_y) | (e_x, e_y) \in X \times Y, e_x \ne e_y\}$ |
+----------------------------------+--------------------------------------------------------------+

## atomic and (full) ordered

e.g. Int.


+----------------------------------+--------------------------------------------------------------+
| rusk                             | nectar                                                       |
+==================================+==============================================================+
| ``x < a``                        | $X' = \{e | e \in X, e < a\}$ |
+----------------------------------+--------------------------------------------------------------+
| ``x < y``                        | $(X', Y') = \{(e_x, e_y) | (e_x, e_y) \in X \times Y, e_x < e_y\}$ |
+----------------------------------+--------------------------------------------------------------+

## collections

e.g. Set A.

+----------------------------------+--------------------------------------------------------------+
| rusk                             | nectar                                                       |
+==================================+==============================================================+
| ``const s = __set { a, b, c };`` | $S' = \{ \{a, b, c\} \}$, singleton of set of set of A |
+----------------------------------+--------------------------------------------------------------+
| ``s == any``                     | $S' = \mathbb{S} = \mathrm{Power}(\mathbb{A})\text{ where } \mathbb{S} = \mathrm{dom}(s), \mathbb{A} = \mathrm{dom}(A)$
+----------------------------------+--------------------------------------------------------------+
| ``e in s``                       | $E' = \bigcup S$
+----------------------------------+--------------------------------------------------------------+
| ``e not in s``                   | $E' = \{e | e \in \mathbb {A} \land e \notin \bigcup S\}$ |
+----------------------------------+--------------------------------------------------------------+
| ``s1 == s2``                     | $S1' = S2' = S1 \cap S2$
+----------------------------------+--------------------------------------------------------------+

quantifiers:

+----------------------------------+--------------------------------------------------------------+
| rusk                             | nectar                                                       |
+==================================+==============================================================+
| ``s.all(pred)`` \                | $\{T | T \in S, \forall e \in T, pred(e) = true\}$ \
+----------------------------------+--------------------------------------------------------------+
| ``forall [cond_expr | e <- s]``  | $\{T | T \in S, \forall e \in T, cond\_expr = true\}$
+----------------------------------+--------------------------------------------------------------+
| ``s.exists(pred)`` \             | $\{T | T \in S, \exists e \in T, pred(e) = true\}$ \
+----------------------------------+--------------------------------------------------------------+
| ``exists [cond_expr | e <- s]``  | $\{T | T \in S, \exists e \in T, cond\_expr = true\}$
+----------------------------------+--------------------------------------------------------------+
| ``s.exists1(pred)`` \            | $\{T | T \in S, \exists1 e \in T, pred(e) = true\}$ \
+----------------------------------+--------------------------------------------------------------+
| ``exists1 [cond_expr | e <- s]`` | $\{T | T \in S, \exists1 e \in T, cond\_expr = true\}$
+----------------------------------+--------------------------------------------------------------+

## compound structures

e.g. `Just x` of Maybe A.

+----------------------------------+--------------------------------------------------------------+
| rusk                             | nectar                                                       |
+==================================+==============================================================+
| ``Just x == y``                  | $T = \{ e | (\mathtt{Just}\ e) \in Y \land e \in X \}$ \ |
|                                  | $X' = T, Y' = \{\mathtt{Just}\ e | e \in T\}$ |
+----------------------------------+--------------------------------------------------------------+
