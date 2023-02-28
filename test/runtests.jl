using DictTools
using DictTools: _AbstractDict, _insert!, _set!, empty_or_similar, collect_sparse
using Dictionaries: Dictionary, IndexError

using Test


@testset "collect_sparse, map_keys" begin
    d = Dictionary([2, 4, 6], [1.0, 2.0, 3.1])
    v = collect_sparse(d)
    @test v == [0.0, 1.0, 0.0, 2.0, 0.0, 3.1]
    @test_throws MethodError collect_sparse(Dictionary{Int, Int}())
    v = collect_sparse(d; neutral_element=Val(7.2))
    @test v == [7.2, 1.0, 7.2, 2.0, 7.2, 3.1]
    v = collect_sparse(d; transform = x -> x + 1)
    @test v == [0.0, 0.0, 1.0, 0.0, 2.0, 0.0, 3.1]

    d = Dictionary([2, 4, 5, 6, 7], [9, 9, 3, 9, 7])
    dm = map_keys(d, iseven)
    @test dm == Dictionary([true, false], [27, 10])

end

@testset "DictTools.jl" begin

    @test Dict{Int, String} <: _AbstractDict{Int, String}
    @test !(Dict{Int, String} <: _AbstractDict{Int, Int})
    @test Dictionary{Int, String} <: _AbstractDict{Int, String}
    @test !(Dictionary{Int, String} <: _AbstractDict{Int, Int})

    items = [0, 1, 2, 0, 1]
    for DT in (Dictionary, Dict)
        cm = count_map(DT, items)
        # DT([0, 1, 2], [2, 2, 1]) ; disabled this because piracy
        exp_cm = DictTools.construct(DT, [0, 1, 2], [2, 2, 1])
        @test cm == exp_cm
        @test count_map(DT, tuple(items...)) == exp_cm
        @test count_map(DT, (x for x in items)) == exp_cm
        @test normalize(cm) == DictTools.construct(DT, [0,1,2], [0.4, 0.4, 0.2])
        add_counts!(cm, [0, 1, 2])
        @test cm == DictTools.construct(DT, [0, 1, 2], [3, 3, 2])
        add_counts!(cm, [0, 1], 3)
        @test cm == DictTools.construct(DT, [0, 1, 2], [6, 6, 2])
    end

    ks = ["a", "b", "d"]
    vs = [1.0, 2.0, 3.0]
    d1 = construct(Dict, ks, vs)
    d2 = construct(Dictionary, ks, vs)
    _insert!(d1, "c", 4.0)
    @test d1["c"] === 4.0
    _insert!(d2, "c", 4.0)
    @test d2["c"] === 4.0
    v = [1.0, 2.0, 0.0, 3.0]
    _insert!(v, 3, 4.0)
    @test v[3] === 4.0

    _set!(d1, "c", 5.0)
    @test d1["c"] === 5.0
    _set!(d2, "c", 5.0)
    @test d2["c"] === 5.0
    _set!(v, 3, 5.0)
    @test v[3] === 5.0

    @test_throws IndexError _insert!(d2, "c", 4.0)

    d1_e = empty_or_similar(d1, keytype(d1), valtype(d1))
    @test isempty(d1_e)
    @test isa(d1_e, Dict{String, Float64})
    d2_e = empty_or_similar(d2, keytype(d2), valtype(d2))
    @test ! isempty(d2_e)
    @test isa(d2_e, Dictionary{String, Float64})
    v_e = empty_or_similar(v, Int, eltype(v))
    @test ! isempty(v_e)
    @test isa(v_e, Vector{Float64})
end
