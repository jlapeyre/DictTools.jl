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

For dictionaries call `empty`. For `AbstractVector` call similar. They key type for
`AbstractVector` must be `Int.
"""
empty_or_similar(d::_AbstractDict, ::Type{KeyT}, ::Type{ValT}) where {KeyT, ValT} = empty(d, KeyT, ValT)
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

"""
    normalize(container::Union{_AbstractDict, AbstractVector})

Return a container similar to `container` but with values normalized so that they sum to one.
This can be used to convert a histogram (count map) to a probability distribution.
"""
function normalize(container)
    VT = valtype(container)
    DT = Base.promote_op(/, VT, Base.promote_op(+, VT, VT))
    return normalize!(empty_or_similar(container, keytype(container), DT), container)
end

"""
    normalize!(dest, src)

Normalize `src`, writing the result to `dest`.
Both `dest` and `src` are of type `Union{_AbstractDict, AbstractVector}`.
"""
function normalize!(dest, src)
    thesum = sum(values(src))
    for (k, v) in pairs(src)
        _set!(dest, k, v / thesum)
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


function ZChop.applyf!(func, dict::Dictionary, args...; kwargs...)
    ZChop.applyf!(func, dict.values, args...; kwargs...)
    return dict
end

end # module DictTools
