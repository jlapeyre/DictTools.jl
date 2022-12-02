abstract type AbstractSparseArray{T,N} <: AbstractArray{T,N} end

# Partially implemented, but provides some useful functionality
"""
    SparseArray{T, N, DT, K, F, NT} <: AbstractSparseArray{T,N}

This sparse array wraps a dictionary. It may be used as an sparse array, but also
is intended to function as an array view on a dictionary. As such, it is a means to convert the dictionary to an
`Array`.

Only `N=1`, with alias `SparseVector` is supported at the moment.

# Fields
* data{K, V} - the dictionary
* dims - NTuple{N, Int}
* transform_key::F a function that takes an object of type `K` as input and returns `i::Int`, an
  index into a corresponding `AbstractArray`.

We currently don't store a function to go the other way, that is, convert an `Int` to a key of type `K`.
But, this is probably necessary for better functionality.
"""
struct SparseArray{T, N, DT, K, F, NT} <: AbstractSparseArray{T,N}
    data::DT
    dims::NTuple{N,Int64}
    transform_key::F

    function SparseArray{T, 1}(d::Dictionary{K, V}, transform_key::F = identity, neutral_element=zero(T)
                               ) where {T, K, V, F}
        (min_ind, max_ind) = extrema(transform_key(k)::Int for k in keys(d))
        any(<(0), min_ind) && throw(DomainError(min_ind, "Invalid index for Vector."))
        dims = (max_ind,)
        return new{T, 1, Dictionary{K, V}, K, F, neutral_element}(d, dims, transform_key)
    end

    # function SparseArray{T,N}(::UndefInitializer, dims::Dims{N}) where {T,N}
    #     return new{T,N}(Dict{CartesianIndex{N},T}(), dims)
    # end
    # function SparseArray(a::SparseArray{T,N}) where {T,N}
    #     new{T,N}(copy(a.data), a.dims)
    # end
end

const SparseVector{T, DT, K, F, NT} = SparseArray{T, 1, DT, K, F, NT}

neutral_element(::SparseArray{<:Any, <:Any, <:Any, <:Any, <:Any, NT}) where {NT} = NT

#Base.collect(sa::SparseArray) = @inbounds __convert(sa.data, sa.dims[1], sa.transform_key, neutral_element(sa))
Base.collect(sa::SparseArray) = @inbounds collect_sparse(sa.data)

@inline Base.getindex(sa::SparseVector{<:Any, <:Any, K, <:Any, NT}, k::K) where {K, NT} = get(sa.data, k, NT)

Base.size(sa::SparseArray) = sa.dims

nnz(sa::SparseArray) = length(sa.data)
