using DictTools
using Dictionaries: Dictionary
using Test

@testset "DictTools.jl" begin
    items = [0, 1, 2, 0, 1]
    cm = count_map(items)
    exp_cm = Dictionary([0, 1, 2], [2, 2, 1])
    @test cm == exp_cm
    @test count_map(tuple(items...)) == exp_cm
    @test count_map((x for x in items)) == exp_cm
    @test normalize(cm) == Dictionary([0,1,2], [0.4, 0.4, 0.2])

    add_counts!(cm, [0, 1, 2])
    @test cm == Dictionary([0, 1, 2], [3, 3, 2])
    add_counts!(cm, [0, 1], 3)
    @test cm == Dictionary([0, 1, 2], [6, 6, 2])
end
