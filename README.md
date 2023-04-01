# DictTools

[![Build Status](https://github.com/jlapeyre/DictTools.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jlapeyre/DictTools.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/jlapeyre/DictTools.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/jlapeyre/DictTools.jl)
[![Aqua QA](https://raw.githubusercontent.com/JuliaTesting/Aqua.jl/master/badge.svg)](https://github.com/JuliaTesting/Aqua.jl)
[![JET QA](https://img.shields.io/badge/JET.jl-%E2%9C%88%EF%B8%8F-%23aa4444)](https://github.com/aviatesk/JET.jl)
[![deps](https://juliahub.com/docs/DictTools/deps.svg)](https://juliahub.com/ui/Packages/DictTools/2Dg1l?t=2)
[![version](https://juliahub.com/docs/DictTools/version.svg)](https://juliahub.com/ui/Packages/DictTools/2Dg1l)

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

* `count_map([::Type{T}=Dictionary], itr, filt = x -> true)`

   Return a dictionary of type `T` whose keys are elements of `itr` and whose values count
   how many times each occurs. Only elements of `itr` for which `filt` returns `true` are
   counted.

   `count_map` has been tested for `T` being either `Dict` or `Dictionary`.

   `StatsBase.countmap` differs in that it has an optimization for integers and that `itr` of
   indetermiate length is first collected internally.

* `add_counts!(dict::_AbstractDict{<:Any,V}, itr, ncounts=one(V)) where V`

    Add `ncounts` counts to `dict` for each key in `itr`. If `ncounts` is ommited,
    add one count for each key.

* `_AbstractDict{T,V}`

   Either an `AbstractDict` or an `AbstractDictionary`. A union type.
   Here `_` indicates not a private identifier, but rather differentiates from `AbstractDict`.
* `_Dict{T,V}`

   Either a `Dict` or a `Dictionary`. A union type

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


* `map_keys`, `map_keys!`
     map_keys(dict::Dictionary, keymap_func, combine_func = +)


    Return a `Dictionary` whose keys are the image of `keys(dict)` under `keymap_func` and whose values
    are created by accumulating with `combine_func` the values from the preimage of each key in the image of `keymap_func`.

    For example, suppose `combine_func` is `+`, and `keymap_func` is `iseven`, and the only even keys in `dict` are in key-value pairs
    `(2, 9)`, `(4, 9)`, `(6, 9)`. Then the output `Dictionary` will contain the key-value pair `(true, 27)`.

    `map_keys` is useful for computing a marginal probability distribution.
    If `dict` represents counts or a probability distribution, and `combine_func` is `+` and `keymap_func` is many-to-one for
    some keys, then `map_keys` effects marginalization of the distribution.


* `collect_sparse`
    Convert `dict` representing a sparse vector to a dense `Vector`. See doc string
