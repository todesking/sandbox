# Memo

## Duplicate

* Instance `x <: X` is _duplicatable(X)_ if `∃x' <: X, class(x) != class(x') → ∀E → E(x) === E(x') if all references to x in E(x) has type _ >: X`
* Duplicatable instance `x` is _escaped_ if `x` is ANY of:
  * Referenced from another _escaped_ instance's field
  * Referenced from `E(x')`
  * `this` _leaks_ to other instance
* Instance `x` _leaks_ if:
  * has a instance method `m` in `x` invokes other instance's method/static methods with `this` as argument
  * has a instance method `m` that marked as `synchronized` or used `monitorenter`/`monitorexit` instruction to `this`
  * has a instance method `m` that `putfield`/`putstatic` `this` to some field

`duplicate(x, X, outer_escaped)` is:

* TODO: New interfaces in `_ <<: X`
* TODO: super methods in `_ <<: X`
* TODO: X is interface
* TODO: MethodHandle
* TODO: static methods
* FAIL if ANY of
  * `x` has finalizer
  * `X` is final class
  * `X` have no setter-only constructor(putfield argument or invoke setter-only constructor only)
* create new class `x' <: X`
* for each fields `f` in `x`:
  * FAIL if `f` is non-final and `x` is _escaped_
  * if `f` defined at `_ <<: X`
    * FAIL if
      * `f` has type `_ <<: X`
    * add `f` to `x'`
* for each ALL instance methods/constructors `m` in `x`:
  * FAIL if
    * `m` is abstract
    * `m` takes parameter `_ <<: X`
    * `m` returns `_ <<: X`
    * `m` has non-this reference `_ <<: X`
* for each private members `pm` in `x`:
  * rename `pm` to unique name
* for each visible or self-referenced non-constructor instance methods `m` in `x`:
  * if `m` defined at `_ <<: X`
    * FAIL if
      * `m` is native
      * `m` leaks `this` as `_ <<: X`
    * substitute this-reference class in `m` to `x'`
    * substitute private-member references in `m` to renamed
    * add the method to `x'`
* for each constructor/used super constructor `c` in `x`:
  * FAIL if ANY OF
    * `c` is native
    * `c` may have side-effect
* add field-setter constructor to `x'`:
  * create constructor that args = fields
  * if X have 0-ary setter-only constructor:
    * put all args to fields
  * else X must have n-ary setter-only constructor:
    * call super(a1...an)
    * put rest args to fields
* create instance of `x'` with the constructor and field values of `x`

NOTE: I'm not sure "All references should not have type `_ <<: X`" rule is really required or not.


## Instance fusion

Field `x.y` is _fusionable_ to x if `∀E → E(x) === E(x')` where `x' = fusion(x, y)` if `E(x')` has no reference to `y`

`fusion(x, X, y, Y, outer_escaped)` is:

* Let `duplicate(x, X, outer_escaped)` as `x'`(May FAIL)
* Let `duplicate(y, Y, y_escaped_from_x)` as `y'`(May FAIL)
* for each field `f` in `y'`:
  * unique-rename-copy `f` to `x'`
* For each method `m` in `y'`
  * FAIL if `y` reference not-this members that invisible from `x'`
  * substitute this-member-reference in `y'` to `x'.<renamed>`
  * unique-rename-copy `m` to `x'`
* For each method `m` in `x'`:
  * rewrite method/field reference to `y'` in `m` to rewritten
  * FAIL if some reference may point to `y'` but not sure

## Inlining

Method `y.g` is _inlinable_ into `x.f` if `x.f === inline(x.f, y.g)`

`inline(x.f, x.g)` is:

* FAIL if `g` is native
* inline all invocations of `g` in `f`

`inline(x.f, y.g)` is:

* FAIL if `g` is native
* FAIL if `g` has reference to member that invisible from `x`
* inline all invocations of `g` in `f`

`inline(x.f, y.g(static))` is same as above.

## Field finalization

## Instance elimination

## Self tail recursion

## Generic self recursion

## Generic tail recursion

## Generic recursion
