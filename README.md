# DictTools

[![Build Status](https://github.com/jlapeyre/DictTools.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jlapeyre/DictTools.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/jlapeyre/DictTools.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/jlapeyre/DictTools.jl)


    module DictTools

Some tools to support Dict and Dictionary (and their abstract supertypes.)

`_AbstractDict` is a `Union`. A few pirate methods are made so that their
APIs are closer.

Applications are `count_map`, `count_map!`, `update_map`, `update!`.
* `hist_to_dist`
* `add_ncounts!`
