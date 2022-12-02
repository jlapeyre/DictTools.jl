using DictTools
using DictTools: _AbstractDict
using Dictionaries: Dictionary

using Test

@testset "DictTools.jl" begin

    @test Dict{Int, String} <: _AbstractDict{Int, String}
    @test !(Dict{Int, String} <: _AbstractDict{Int, Int})

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
end
