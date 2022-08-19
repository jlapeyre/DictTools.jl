# DictTools

[![Build Status](https://github.com/jlapeyre/DictTools.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jlapeyre/DictTools.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/jlapeyre/DictTools.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/jlapeyre/DictTools.jl)

### `count_map`

One main feature of this package is `count_map`. For example `count_map(itr)` returns a `Dictionary` counting
the number of times each item in `itr` occurs.

It differs from the one in `StatsBase` in a few ways.

* It allows adding counts from any iterable rather than only `Vector`s.
* It supports both `Dict` and `Dictionary`.
* It is built on an abstraction that allows you update items via a function call.
* It does not use radix sort to optimize the case that the data are `Int`s.

### Common interface to `Dict` and `Dictionary`

Some tools to support `Dict` and `Dictionary` (and their abstract supertypes.)

We have defined `_AbstractDict{T, V} = Union{AbstractDict{T,V}, AbstractDictionary{T,V}}`.

To make the interfaces more compatible we define the pirate method `Base.Dict(inds, vals) = Dict(zip(inds, vals))`.

### Exported functions and objects

docstrings may be more up to date.

* `_AbstractDict{T,V}`

   Either an `AbstractDict` or an `AbstractDictionary`. A union type.
   Here `_` indicates not a private identifier, but rather differentiates from `AbstractDict`.
* `_Dict{T,V}`

    Either a `Dict` or a `Dictionary`. A union type

*  `baretype(::Type{T})`

    Return the typename of `T`. This will fail for some input. In particular,
    and in general, if `T` is a `UnionAll` type. However, more robust methods for `baretype`
    are defined for `Dict` and `Dictionary`.

*  `baretypeof(x)`

    Return the typename of the type of object `x`. That is, return `baretype(typeof(x))`.


*  `update!(dict::Union{Dict,Dictionary}, _key, func, default)`

    If `dict` has key `_key`, replace the value by the result of calling `func` on the value.
    Otherwise insert `default` for `_key`.

    This function may work if `dict` is some other `_AbstractDict`.

*  `update_map(::Type{T}=Dictionary, _keys, func, default)`

    Like `count_map`, but instead of incrementing an existing value by one, it is replaced
    by the result of calling `func` on it. Furthermore, the default is `default` rather than `1`.

* `count_map([::Type{T}=Dictionary], itr)`

    Return a dictionary of type `T` whose keys are elements of `itr`
    and whose values count how many times each occurs.

    `StatsBase.countmap` differs in that it has an optimization for
    integers and that `itr` of indetermiate length is first collected
    internally.

*  `add_counts!(dict::_AbstractDict{<:Any,V}, itr, ncounts=one(V)) where V`

    Add `ncounts` counts to `dict` for each key in `itr`. If `ncounts` is ommited,
    add one count for each key.

*  `hist_to_dist(dict)`

    `dict` is a dictionary of counts. The output is a dictionary
    with the same keys, and counts normalized to a probability distribution.
