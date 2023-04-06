@testitem "for testing testing" begin
    using DictTools

    @test length(count_map([1,1,2,2])) == 2
end
