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

export count_map, add_counts!, update!, update_map, baretype, baretypeof, add_counts!, normalize, _convert,
    construct

export combine_values!, map_keys!, map_keys

import Dictionaries
using Dictionaries: AbstractDictionary, Dictionary, gettoken, gettokenvalue, settokenvalue!
import ZChop

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
empty_or_similar(d::AbstractDictionary{KeyT, ValT}, ::Type{KeyT}, ::Type{ValT}) where {KeyT, ValT} = similar(d)
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
@inline function update!(dict::AbstractDictionary{T, V}, _key::T, func::F, default) where {F, T, V}
    (hasval, token) = gettoken(dict, _key)
    if hasval
        settokenvalue!(dict, token, func(gettokenvalue(dict, token)))
    else
        insert!(dict, _key, default)
    end
end

# Stolen from StatsBase. This is faster than naive method
@inline function update!(dict::Dict{T, V}, _key::T, func::F, default) where {F, T, V}
    index = Base.ht_keyindex2!(dict, _key)
    if index > 0
        @inbounds dict.vals[index] = func(dict.vals[index])
    else
        @inbounds Base._setindex!(dict, default, _key, -index)
    end
end

# This is a bit slower than both specialized methods
@inline function update!(dict::_AbstractDict{T,V}, _key::T, func::F, default) where {F, T, V}
    if haskey(dict, _key)
        dict[_key] = func(get(dict, _key))
    else
        dict[_key] = default
    end
end

function update!(dict::_AbstractDict{T}, _keys, func::F, default) where {F, T}
    for _key::T in _keys
        update!(dict, _key, func, default)
    end
    return dict
end

"""
    update!(v::AbstractVector, _key::Int, func, _ignore=nothing)

Update the value of `v` at index `_key` by calling `func` on it. That is,
set `v[_key] = func(v[_key])`.
"""
function update!(v::AbstractVector{V}, _key::Int, func::F, _ignore=nothing) where {F, V}
    @boundscheck checkbounds(v, _key)
    @inbounds v[_key] = func(v[_key])
end

"""
    update!(v::AbstractVector, _keys, func, _ignore=nothing)

Call `update!(v, k, func)` for each `k::Int` in iterable `_keys`.
"""
function update!(v::AbstractVector{V}, _keys, func::F, _ignore=nothing) where {F, V}
    for k::Int in _keys
        update!(v, k, func, _ignore)
    end
    return v
end

update_map(_keys, func::F, default) where F = update_map(Dictionary, _keys, func, default)

"""
    update_map(::Type{T}=_AbstractDict, _keys, func, default)

Like `count_map`, but instead of incrementing an existing value by one, it is replaced
by the result of calling `func` on it. Furthermore, the default is `default` rather than `1`.
"""
update_map(::Type{DictT}, _keys, func::F, default) where {F, DictT} =
    update!(DictT{typeof(first(_keys)), typeof(default)}(), _keys, func, default)

"""
    count_map([::Type{T}=_AbstractDict], itr)

Return a dictionary of type `T` whose keys are elements of `itr`
and whose values count how many times each occurs.

`StatsBase.countmap` differs in that it has an optimization for
integers and that `itr` of indetermiate length is first collected
internally.
"""
count_map(args...) = update_map(args..., v -> (v + 1), 1)

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

"""
    map_keys!(dest::Dictionary, src::Dictionary, keymap_func, combine_func = +)

This is the same as `map_keys` except that the destination `dest` is passed as input. Existing values
in `dest` will be combined with `combine_func`. However `dest` is typically empty when passed to `map_keys!`.
"""
function map_keys!(dest::Dictionary{KD, VD}, src::Dictionary{KS, VS}, keymap_func::FK, combine_func::FC = +) where {KD,VD,KS,VS,FK,FC}
    for (k, v) in pairs(src)
        mapped_key = keymap_func(k)
        combine_values!(dest, combine_func, mapped_key, v)
    end
    return dest
end

"""
    map_keys(src::Dictionary, keymap_func, combine_func = +)

Return a `Dictionary` whose keys are the image of `keys(src)` under `keymap_func` and whose values
are created by accumulating with `combine_func` the values from the preimage of each key in the image of `keymap_func`.

For example, suppose `combine_func` is `+`, and `keymap_func` is `iseven`, and the only even keys in `src` are in key-value pairs
`(2, 9)`, `(4, 9)`, `(6, 9)`. Then the output `Dictionary` will contain the key-value pair `(true, 27)`.
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
    _convert(::Type{<:Vector}, d::_AbstractDict{Int,V}, neutral_element=zero(V)) where {V}

Convert `d` to a `Vector`. Missing keys in `d` are set to `neutral_element` in the returned vector.

This essentially converts a sparse representation to a dense representation.
"""
function _convert(::Type{VT}, d::_AbstractDict{Int,V}, neutral_element=zero(V)) where {V, VT<:Vector}
    vec = VT(undef, maximum(keys(d)))
    fill!(vec, neutral_element)
    for (k, v) in pairs(d)
        vec[k] = v
    end
    return vec
end

_convert(::Type{Vector}, d::_AbstractDict{Int,V}, neutral_element=zero(V)) where {V} =
    _convert(Vector{V}, d, neutral_element)

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

end # module DictTools
