# DictTools

[![Build Status](https://github.com/jlapeyre/DictTools.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jlapeyre/DictTools.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/jlapeyre/DictTools.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/jlapeyre/DictTools.jl)

### `count_map`

The main feature of this package is `count_map`. For example `count_map(itr)` returns a `Dictionary` counting
the number of times each item in `itr` occurs.

It differs from the one in `StatsBase` in a few ways.

* It allows adding counts from any iterable rather than only `Vector`s.
* It supports both `Dict` and `Dictionary`.
* It is built on an abstraction that allows you update items via a function call.
* It does not use radix sort to optimize the case that the data are `Int`s.

### Common interface to `Dict` and `Dictionary`

Some tools to support `Dict` and `Dictionary` (and their abstract supertypes.)
In general, it is often not worth the trouble to support both `Dict` and `Dictionary` with a single interface.
But, sometimes it is, and these tools can be useful.

We defined `_AbstractDict{T, V} = Union{AbstractDict{T,V}, AbstractDictionary{T,V}}`.

To make the interfaces more compatible one *could* define the pirate method `Base.Dict(inds, vals) = Dict(zip(inds, vals))`.
But instead `DictTools` defines `construct(::Type{T<:_AbstractDict}, inds, vals)` which provides a common interface for
construction. The latter is less convenient, but is not piracy.

### Exported functions and objects

docstrings may be more up to date than what appears below.

* `_AbstractDict{T,V}`

   Either an `AbstractDict` or an `AbstractDictionary`. A union type.
   Here `_` indicates not a private identifier, but rather differentiates from `AbstractDict`.
* `_Dict{T,V}`

   Either a `Dict` or a `Dictionary`. A union type

* `count_map([::Type{T}=Dictionary], itr)`

    Return a dictionary of type `T` whose keys are elements of `itr`
    and whose values count how many times each occurs.

    `StatsBase.countmap` differs in that it has an optimization for
    integers and that `itr` of indetermiate length is first collected
    internally.

* `add_counts!(dict::_AbstractDict{<:Any,V}, itr, ncounts=one(V)) where V`

    Add `ncounts` counts to `dict` for each key in `itr`. If `ncounts` is ommited,
    add one count for each key.

* `update!(dict::Union{Dict,Dictionary}, _key, func, default)`

    If `dict` has key `_key`, replace the value by the result of calling `func` on the value.
    Otherwise insert `default` for `_key`.

    This function may work if `dict` is some other `_AbstractDict`.

    In addition to dictionaries, `update!` and `add_counts!` also accept (some) `AbstractVector` types

* `update_map(::Type{T}=Dictionary, _keys, func, default)`

    Like `count_map`, but instead of incrementing an existing value by one, it is replaced
    by the result of calling `func` on it. Furthermore, the default is `default` rather than `1`.

* `DictTools.normalize(dict)`

    `dict` is a dictionary of counts. The output is a dictionary with the same keys, and counts normalized to a probability distribution.

* Specialized method for `Dictionary` for the [ZChop.jl](https://github.com/jlapeyre/ZChop.jl) package.

* `construct` to construct a `Dict` and `Dictionary` with one API

* `_convert` to convert a sparse vector represented by an `_AbstractDict` to a `Vector`.
