module AustralianMonetaryFiscalGame

# The research scripts remain at the project root so they are easy to compare
# with the original files in SI_game.  This small package entry point allows
# Julia's package manager to instantiate and precompile the local project.
include(joinpath(@__DIR__, "..", "policy_game.jl"))
include(joinpath(@__DIR__, "..", "data_pipeline.jl"))

using .PolicyGame
using .AustralianData

export PolicyGame, AustralianData

end
