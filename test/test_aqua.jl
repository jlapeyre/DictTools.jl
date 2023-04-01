using DictTools
using Aqua: Aqua

@testset "aqua project toml formatting" begin
    Aqua.test_project_toml_formatting(DictTools)
end

@testset "aqua unbound_args" begin
    Aqua.test_unbound_args(DictTools)
end

@testset "aqua undefined exports" begin
    Aqua.test_undefined_exports(DictTools)
end

@testset "aqua test ambiguities" begin
    Aqua.test_ambiguities([DictTools, Core, Base])
end

@testset "aqua piracy" begin
    Aqua.test_piracy(DictTools)
end

@testset "aqua project extras" begin
    Aqua.test_project_extras(DictTools)
end

@testset "aqua state deps" begin
    Aqua.test_stale_deps(DictTools)
end

@testset "aqua deps compat" begin
    Aqua.test_deps_compat(DictTools)
end
