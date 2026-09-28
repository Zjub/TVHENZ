module AustralianEndogenousLeadershipGame

# The executable scripts include the model module directly so that the research
# code remains easy to read.  This wrapper makes the project a valid Julia
# package for reproducible dependency management.
include(joinpath(@__DIR__, "..", "endogenous_leadership.jl"))

end
