"""
    module DictTools

Some tools to support Dict and Dictionary (and their abstract supertypes.)

`_AbstractDict` is a `Union`. A few pirate methods are made so that their
APIs are closer.

Applications are `count_map`, `update_map`, `update!`.
* `hist_to_dist`
* `add_counts!`
"""
module DictTools

export count_map, add_counts!, update!, update_map, baretype, baretypeof, add_counts!, normalize,
    construct

export combine_values!, map_keys!, map_keys, collect_sparse

export SparseArray, AbstractSparseArray, SparseVector, nnz, neutral_element

import Dictionaries
using Dictionaries: AbstractDictionary, Dictionary, gettoken, gettokenvalue, settokenvalue!
import ZChop


const _TRUE_FUNC = _ -> true

# Add a layer to handle both kinds of dictionaries

"""
    _AbstractDict{T, V}

Either an `AbstractDict` or an `AbstractDictionary`. A union type
"""
const _AbstractDict{T, V} = Union{AbstractDict{T,V}, AbstractDictionary{T,V}}

"""
    _AbstractDictOrVector{V}

Either an `_AbstractDict` of `AbstractVector` with key type `V`.
"""
const _AbstractDictOrVector{V} = Union{_AbstractDict{<:Any, V}, AbstractVector{V}}

"""
    _Dict{T, V}

Either a `Dict` or a `Dictionary`. A union type
"""
const _Dict{T, V} = Union{Dict{T,V}, Dictionary{T,V}}

# Type piracy to give Dict the same construction interface as Dictionary
# Base.Dict(inds, vals) = Dict(zip(inds, vals))

"""
    construct(::Type{T<:_AbstractDict}, inds, vals)

Construct either an `AbstractDict` or `AbstractDictionary` from `inds` and `vals`.

This is intended to give a common interface. It would be more convenient to commit type piracy with
`Base.Dict(inds, vals) = Dict(zip(inds, vals))`.
"""
construct(::Type{T}, inds, vals) where {T<:AbstractDict} = T(zip(inds, vals))
construct(::Type{T}, inds, vals) where {T<:AbstractDictionary} = T(inds, vals)

# TODO: Some examples would make this more clear
"""
    baretype(::Type{T})

Note: There are better solutions than `baretype` for many (perhaps all) use cases.
Return the typename of `T`. This will fail for some input. In particular,
and in general, if `T` is a `UnionAll` type. However, more robust methods for `baretype`
are defined for `Dict` and `Dictionary`.
"""
baretype(::Type{T}) where {T} = isempty(T.parameters) ? T : T.name.wrapper
baretype(::Type{<:Dict}) = Dict
baretype(::Type{<:Dictionary}) = Dictionary

# TODO: Use empty, similar, etc. instead of this.
"""
    baretypeof(x)

Return the typename of the type of object `x`. That is, return `baretype(typeof(x))`.
"""
baretypeof(x) = baretype(typeof(x))

### Common interface for common functions for Dict, Dictionaries, AbstractVector

"""
    _insert!(d::Union{_AbstractDict, AbstractVector}, k, v)

Insert the key value pair (`k`, `v`) in `d`. For `Dictionary`, this calls
`insert!`. Otherwise, it calls `setindex!.
"""
_insert!(d::AbstractDict, k, v) = (d[k] = v)
function _insert!(d::AbstractVector, k, v)
    @boundscheck checkbounds(d, k)
    @inbounds d[k] = v
end

_insert!(d::Dictionaries.AbstractDictionary, k, v) = Dictionaries.insert!(d, k, v)

"""
    _set!(d::Union{_AbstractDict, AbstractVector}, k, v)

Set the key value pair (`k`, `v`) in `d`.
"""
_set!(d::AbstractDict, k, v) = (d[k] = v)
function _set!(d::AbstractVector, k, v)
    @boundscheck checkbounds(d, k)
    @inbounds d[k] = v
end

_set!(d::Dictionaries.AbstractDictionary, k, v) = Dictionaries.set!(d, k, v)

"""
    empty_or_similar(d::Union{_AbstractDict, AbstractVector}, ::Type{KeyT}, ::Type{ValT})

For `Dict` call `empty`. For `AbstractVector` and `Dictionary` call similar. They key type for
`AbstractVector` must be `Int.
"""
empty_or_similar(d::AbstractDict, ::Type{KeyT}, ::Type{ValT}) where {KeyT, ValT} = empty(d, KeyT, ValT)
empty_or_similar(d::AbstractDictionary{KeyT}, ::Type{KeyT}, ::Type{ValT}) where {KeyT, ValT} = similar(d, ValT)
empty_or_similar(d::AbstractVector, ::Type{Int}, ::Type{ValT}) where {ValT} = similar(d, ValT)

# Maybe don't need to use this. Prefer using values at call site ?
_sum(d::Union{AbstractDict, AbstractVector}, args...) = sum(d, args...)
_sum(d::AbstractDict, args...) = sum(values(d), args...)

# These functions should be collected elsewhere.
# They may be actually collected elsewhere.

# The parameter `F` is actually necessary to force specialization
"""
    update!(dict::_AbstractDict, _key, func, default)

If `dict` has key `_key`, replace the value by the result of calling `func` on the value.
Otherwise insert `default` for `_key`.

This function supports some subtypes of `_AbstractDict`.
"""
@inline function _update!(dict::AbstractDictionary{T, V}, _key::T, func::F, default) where {F, T, V}
    (hasval, token) = gettoken(dict, _key)
    if hasval
        settokenvalue!(dict, token, func(gettokenvalue(dict, token)))
    else
        insert!(dict, _key, default)
    end
end

# Stolen from StatsBase. This is faster than naive method
@inline function _update!(dict::Dict{T, V}, _key::T, func::F, default) where {F, T, V}
    index = Base.ht_keyindex2!(dict, _key)
    if index > 0
        @inbounds dict.vals[index] = func(dict.vals[index])
    else
        @inbounds Base._setindex!(dict, default, _key, -index)
    end
end

# This is a bit slower than both specialized methods
@inline function _update!(dict::_AbstractDict{T,V}, _key::T, func::F, default) where {F, T, V}
    if haskey(dict, _key)
        dict[_key] = func(dict[_key])
    else
        dict[_key] = default
    end
end

@inline function update!(dict::_AbstractDict{T,V}, _key::T, func::F, default,
                         filt::F2 = _TRUE_FUNC) where {F, T, V, F2}
    filt(_key) || return
    _update!(dict, _key, func, default)
end

function update!(dict::_AbstractDict{T}, _keys, func::F, default,
                 filt::F2 = _TRUE_FUNC) where {F, T, F2}
    for _key::T in _keys
        filt(_key) || continue
        update!(dict, _key, func, default)
    end
    return dict
end

"""
    update!(v::AbstractVector, _key::Int, func, _ignore=nothing)

Update the value of `v` at index `_key` by calling `func` on it. That is,
set `v[_key] = func(v[_key])`.
"""
function update!(v::AbstractVector{V}, _key::Int, func::F, _ignore=nothing,
                 filt::F2 = _TRUE_FUNC) where {F, V, F2}
    filt(_key) || return
    @boundscheck checkbounds(v, _key)
    @inbounds v[_key] = func(v[_key])
end

"""
    update!(v::AbstractVector, _keys, func, _ignore=nothing)

Call `update!(v, k, func)` for each `k::Int` in iterable `_keys`.
"""
function update!(v::AbstractVector{V}, _keys, func::F, _ignore=nothing,
                 filt::F2 = _TRUE_FUNC) where {F, V, F2}
    for k::Int in _keys
        filt(k) || continue
        update!(v, k, func, _ignore)
    end
    return v
end

function update_map(_keys, func::F, default, filt::F2 = _TRUE_FUNC) where {F, F2}
    update_map(Dictionary, _keys, func, default, filt)
end

"""
    update_map(::Type{T}=_AbstractDict, _keys, func, default)

Like `count_map`, but instead of incrementing an existing value by one, it is replaced
by the result of calling `func` on it. Furthermore, the default is `default` rather than `1`.
"""
update_map(::Type{DictT}, _keys, func::F, default,
           filt::F2 = _TRUE_FUNC) where {F, DictT, F2} =
    update!(DictT{typeof(first(_keys)), typeof(default)}(), _keys, func, default, filt)

"""
    count_map([::Type{T}=Dictionary], itr, filt = x -> true)

Return a dictionary of type `T` whose keys are elements of `itr` and whose values count
how many times each occurs. Only elements of `itr` for which `filt` returns `true` are
counted.

`count_map` has been tested for `T` being either `Dict` or `Dictionary`.

`StatsBase.countmap` differs in that it has an optimization for integers and that `itr` of
indetermiate length is first collected internally.
"""
count_map(::Type{DictT}, itr, filt::F2 = _TRUE_FUNC) where {DictT, F2} = update_map(DictT, itr, v -> (v+1), 1, filt)
#count_map(args...) = update_map(args..., v -> (v + 1), 1)

count_map(itr, filt::F2 = _TRUE_FUNC) where {F2} = count_map(Dictionary, itr, filt)


"""
    add_counts!(counter::Union{_AbstractDict{<:Any,V}, AbstractVector{V}}, itr, ncounts=one(V)) where V

Add `ncounts` counts to `counter` for each key in `itr`. If `ncounts` is ommited,
add one count for each key.

If `counter` is an `AbstractVector`, then `itr` must produce `Int` indices into `counter`.
"""
function add_counts!(counter::_AbstractDictOrVector{V}, itr, ncounts) where V
    update!(counter, itr, v -> (v + V(ncounts)), V(ncounts))
end

# Separating this seems slightly more efficient than using a default value for
# counts above. This should not be the case.
add_counts!(counter::_AbstractDictOrVector{V}, itr) where {V} =
    update!(counter, itr, v -> (v + one(V))::V, one(V))

# TODO: Implement for Dict
"""
    combine_values!(dest::Dictionary, combine_func, _key, val, defaultval=val)

If `dest` contains `_key`, replace the corresponding value `old_val` with `combine_func(val, old_val)`.
Otherwise, set the value for `_key` to `defaultval`.
"""
function combine_values!(dest::Dictionary{K, V}, combine_func::F, _key::K, val, defaultval=val) where {K, V, F}
    (hasval, token) = gettoken(dest, _key)
    if hasval
        settokenvalue!(dest, token, combine_func(val, gettokenvalue(dest, token)))
    else
        insert!(dest, _key, defaultval)
    end
    return dest
end

function combine_values!(dest::AbstractVector{V}, combine_func::F, _key::Int, val, unused_defaultval=nothing) where {V, F}
    @boundscheck checkbounds(dest, _key)
    @inbounds dest[_key] = combine_func(val, dest[_key])
    return dest
end

"""
    map_keys!(dest::Dictionary, src::Dictionary, keymap_func, combine_func = +)

This is the same as `map_keys` except that the destination `dest` is passed as input. Existing values
in `dest` will be combined with `combine_func`. However `dest` is typically empty when passed to `map_keys!`.
"""
function map_keys!(dest, src, keymap_func::FK, combine_func::FC = +) where {FK,FC}
#function map_keys!(dest::Dictionary, src::Dictionary, keymap_func::FK, combine_func::FC = +) where {FK,FC}
#function map_keys!(dest::Dictionary{KD, VD}, src::Dictionary{KS, VS}, keymap_func::FK, combine_func::FC = +) where {KD,VD,KS,VS,FK,FC}
    for (k, v) in pairs(src)
        mapped_key = keymap_func(k)
        combine_values!(dest, combine_func, mapped_key, v)
    end
    return dest
end


# TODO: Implement for Array. Probably generate all keys in an array, then do the combination of values
# Can also get the max val and use this to allocate just one array
# The performance difference is often not great
"""
    map_keys(dict::Dictionary, keymap_func, combine_func = +)


Return a `Dictionary` whose keys are the image of `keys(dict)` under `keymap_func` and whose values
are created by accumulating with `combine_func` the values from the preimage of each key in the image of `keymap_func`.

For example, suppose `combine_func` is `+`, and `keymap_func` is `iseven`, and the only even keys in `dict` are in key-value pairs
`(2, 9)`, `(4, 9)`, `(6, 9)`. Then the output `Dictionary` will contain the key-value pair `(true, 27)`.

`map_keys` is useful for computing a marginal probability distribution.
If `dict` represents counts or a probability distribution, and `combine_func` is `+` and `keymap_func` is many-to-one for
some keys, then `map_keys` effects marginalization of the distribution.
"""
function map_keys(src::Dictionary{KS, VS}, keymap_func::FK, combine_func::FC = +) where {KS,VS,FK,FC}
    KD = typeof(keymap_func(first(keys(src))))
    VD = Base.promote_op(combine_func, VS, VS) # fragile. perhaps best we can do
    dest = Dictionary{KD, VD}()
    return map_keys!(dest, src, keymap_func, combine_func)
end

###
### normalize, normalize!
###

# TODO: container::Dictionary is much slower than it should be.
"""
    normalize(container::Union{_AbstractDict, AbstractVector})

Return a container similar to `container` but with values normalized so that they sum to one.
This can be used to convert a histogram (count map) to a probability distribution.
"""
function normalize(container)
    ValueT = valtype(container)
    DataT = Base.promote_op(/, ValueT, Base.promote_op(+, ValueT, ValueT))
    return _normalize!(empty_or_similar(container, keytype(container), DataT), container)
end

_normalize!(dest, src) = normalize!(dest, src)

function _normalize!(dest::Dictionary, src::Dictionary)
    thesum = sum(values(src))
    for i in eachindex(src.values)
        @inbounds dest.values[i] = src.values[i] / thesum
    end
    return dest
end

"""
    normalize!(d::Dictionary)

Normalize `d` in place. That is, scale the values of `d` so that they sum
to one.
"""
normalize!(d::Dictionary) = _normalize!(d, d)

# TODO: for Vectors normalize!(v, v) is too slow
"""
    normalize!(dest, src)

Normalize `src`, writing the result to `dest`.
Both `dest` and `src` are of type `Union{_AbstractDict, AbstractVector}`.
"""
function normalize!(dest, src)
    thesum = sum(values(src))
    for (k, v) in pairs(src) # @inbounds important for Vectors
        @inbounds _set!(dest, k, v / thesum)
    end
    return dest
end

"""
    normalize!(container::Union{_AbstractDict, AbstractVector})

Normalize `container` in place.

If the normalized value cannot be converted to the value type of `container`, an error, such as `InexactError` will be thrown.
For example if `valtype(container)` is `Int`, an `InexactError` will be thrown.
"""
normalize!(container) = normalize!(container, container)

"""
    collect_sparse(dict::Dictionary{K,V}; transform=identity, neutral_element=Val(zero(V)),
                                max_ind::Union{Nothing, Int}=nothing) where {K, V}

Convert `dict` representing a sparse vector to a dense `Vector`.

Missing keys in `dict` are set to `neutral_element` in the returned vector. `transform_key` transforms keys of type
`K` to type `Int`, so that they are valid indexes for `Vector`.

`max_ind`, if not equal to `nothing`, is the length of the output `Vector`, this must be large enough for all of they keys
in `dict`. If `max_ind` is `nothing`, then the length will be computed. This will cost of some additional allocation
if `transform` is not identity.
"""
function collect_sparse(dict::Dictionary{K, V}; transform::F = identity, neutral_element=Val(zero(V)),
                        max_ind=nothing
                        ) where {K, V, F}
    max_ind === nothing && return _collect_sparse(dict, transform, neutral_element)
    return _collect_sparse(max_ind, dict, transform, neutral_element)
end

# if transform is identity, we don't need to store transformed keys to avoid transforming twice
function _collect_sparse(dict::Dictionary{K, V}, ::typeof(identity), ::Val{neutral_element}=Val(zero(V))) where {K, V, neutral_element}
    ks = dict.indices.values
    (min_ind, max_ind) = extrema(ks)
    min_ind < 0 && throw(DomainError(min_ind, "Invalid index for Vector."))
    return _make_dense_vector(max_ind, identity, ks, dict.values, Val(neutral_element))
end

# transform_key is not identity and we don't know max_ind. So we compute it. And assume transforming is costly, so we only do it
# once and store the results.
function _collect_sparse(dict::Dictionary{K, V}, transform_key::F = identity, ::Val{neutral_element}=Val(zero(V))) where {K, V, F, neutral_element}
    ks = dict.indices.values
    int_inds = _get_int_inds(ks, transform_key)
    (min_ind, max_ind) = extrema(int_inds)
    min_ind < 0 && throw(DomainError(min_ind, "Invalid index for Vector."))
    return _make_dense_vector(max_ind, int_inds, dict.values, Val(neutral_element))
end

# Transform the dictionary keys to Vector indices
function _get_int_inds(ks, transform_key_func::F) where {F}
    int_inds = Vector{Int}(undef, length(ks))
    @inbounds for i in eachindex(ks)
        int_inds[i] = transform_key_func(ks[i])
    end
    return int_inds
end

function _make_dense_vector(max_ind::Int, int_inds::Vector, vals, ::Val{neutral_element}) where {neutral_element}
    return _fill_dense_vector!(fill(neutral_element, max_ind), int_inds, vals)
end

function _fill_dense_vector!(vec, int_inds, vals)
    for i in eachindex(int_inds)
        @inbounds vec[int_inds[i]] = vals[i]
    end
    return vec
end

###

function _collect_sparse(max_ind::Int, dict::Dictionary{K, V}, transform_key::F = identity, ::Val{neutral_element}=Val(zero(V))) where {K, V, F, neutral_element}
    ks = dict.indices.values
    return _make_dense_vector(max_ind, transform_key, ks, dict.values, Val(neutral_element))
end

function _make_dense_vector(max_ind::Int, transform_key::F, ks, vals, ::Val{neutral_element}) where {neutral_element, F}
    vec = fill(neutral_element, max_ind)
    return _fill_dense_vector!(vec, ks, vals, transform_key)
end

# Don't use @inbounds for vec[ind] because the size of vec was set via user input, which may be wrong
function _fill_dense_vector!(vec, ks, vals, transform_key::F) where {F}
    for i in eachindex(vals)
        @inbounds ind = transform_key(ks[i])
        @inbounds val = vals[i]
        vec[ind] = val
    end
    return vec
end

ZChop._copy(d::Dictionary{<:Any, <:Number}) = empty(d)

"""
    zchop(d::Dictionary, args...)
    zchop!(d::Dictionary, args...)
    nchop(d::Dictionary, args...; kwargs...)
    nchop!(d::Dictionary, args...; kwargs...)

Apply `zchop`, `zchop!`, `nchop`, `nchop!` to each value of `d`. They keys
are not altered.
"""
ZChop.zchop

import ZChop: zchop, zchop!, nchop!, nchop

# The fallback method for zchop(::Dictionary) is very slow. This is very fast.
function ZChop.applyf!(func, dict::Dictionary, args...; kwargs...)
    ZChop.applyf!(func, dict.values, args...; kwargs...)
    return dict
end

include("dok.jl")

end # module DictTools
