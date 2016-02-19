# Memo

Instance `x` is _safe rewritable_ if all expression `E(x)` is equals to `E(x')` for some `x'`

Instance `x` is _safe rewritable_ to `x'` iff

* Should not have native method defined at `x <: _ <<: common_superclass(x, x')` // TODO: ok if unused
* Should not have finalizer
* Every reference of `x` in `E(x)` should
  * Has type `_ >: common_superclass(x, x')`
  * Not used to reflection API
* All methods in `x` should
  * Not leak `this`
  * Not receive parameter/return value as type `_ <<: common_superclass(x, x')`
* All fields in `x` should ONE OF these conditions
  * `final`
  * `E(x')` should not contains reference to `x`


Field `y` of instance `x` is _fusionable_ to `x` if `E(x)` is qeuals to `E(x')` where `x' = instance_fusion(x, y)`

Field `y` of instance `x` is _fusionable_ to `x` iff

* `y` is _safe rewritable_ to `y'`
* All methods in `y` should
  * Not access `x`-invisible members

Method `f` is _inlinable_ into `g` if `g` equals to `inlining(g, f)`

Static method `f` is _inlinable_ into `x.g` iff

* `f` is not native
* All field references in `f` should `final`
* All member references in `f` is visible from `g`

Instance method `y.f` is _inlinable_ into `x.g` iff

* Satisfies conditions of static method inlinability
* Or, `y == x`

Field `x.y` is _wirte protectable_ if `x` is _safe rewritable_ to `x'` where `x'.y` is final

???


