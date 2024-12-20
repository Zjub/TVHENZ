### Toy attempt at a SAM pandemic model

#using Pkg
#Pkg.add("SymPy")
#Pkg.add("Optim")
#Pkg.add("Plots")
#Pkg.add("Parameters")
#Pkg.add("Distributions")
#Pkg.add("LinearAlgebra")
#Pkg.add("ForwardDiff")
#Pkg.add("NLsolve")

using Parameters
using NLsolve

@with_kw struct PandemicSearchModel
    β::Float64 = 0.99           # Discount factor
    η_pandemic::Float64         # Target labor supply elasticity during pandemic
    f_pandemic::Float64         # Job finding rate during pandemic
    s_pandemic::Float64         # Separation rate during pandemic
    f_normal::Float64           # Job finding rate outside pandemic
    s_normal::Float64           # Separation rate outside pandemic
    ρ_pandemic::Float64         # Replacement rate during pandemic
    ρ_normal::Float64           # Replacement rate in normal times
    α::Float64 = 0.5            # Matching elasticity
    κ::Float64 = 1.0            # Matching efficiency
end

function matching_function(U::Float64, V::Float64, model::PandemicSearchModel)
    return model.κ * U^model.α * V^(1 - model.α)
end

function equilibrium_conditions!(F, x, model::PandemicSearchModel, f::Float64, s::Float64, ρ::Float64, η::Float64)
    U = x[1]   # Unemployment rate
    V = x[2]   # Vacancy rate

    # Steady-state unemployment condition
    F[1] = U - (s * (1 - U) / (s + f))

    # Vacancy market clearing condition
    F[2] = V - f * U / (1 - U)
end

function solve_steady_state(model::PandemicSearchModel, f::Float64, s::Float64, ρ::Float64, η::Float64)
    initial_guess = [0.05, 0.05]
    solution = nlsolve((F, x) -> equilibrium_conditions!(F, x, model, f, s, ρ, η), initial_guess)
    return solution.zero
end

function compute_elasticity(U::Float64, ρ::Float64, η::Float64)
    welfare_benefit = ρ * (1 - U)
    return η * (welfare_benefit / ρ - 1)
end

function calibrate_model(model::PandemicSearchModel)
    # Calibrate η to match the pandemic elasticity target
    result = nlsolve((η -> begin
        pandemic_state = solve_steady_state(model, model.f_pandemic, model.s_pandemic, model.ρ_pandemic, η[1])
        compute_elasticity(pandemic_state[1], model.ρ_pandemic, η[1]) - model.η_pandemic
    end), [model.η_pandemic])

    η_calibrated = result.zero[1]

    # Solve for both states
    pandemic_state = solve_steady_state(model, model.f_pandemic, model.s_pandemic, model.ρ_pandemic, η_calibrated)
    normal_state = solve_steady_state(model, model.f_normal, model.s_normal, model.ρ_normal, η_calibrated)

    # Compute elasticities
    pandemic_elasticity = compute_elasticity(pandemic_state[1], model.ρ_pandemic, η_calibrated)
    normal_elasticity = compute_elasticity(normal_state[1], model.ρ_normal, η_calibrated)

    # Print results
    println("Calibrated η: ", η_calibrated)
    println("Target Pandemic Labor Supply Elasticity: ", model.η_pandemic)
    println("Implied Pandemic Labor Supply Elasticity: ", pandemic_elasticity)
    println("Implied Normal State Labor Supply Elasticity: ", normal_elasticity)
    println("Normal State Unemployment Rate: ", normal_state[1])
    println("Pandemic State Unemployment Rate: ", pandemic_state[1])
end

# Example Data
model = PandemicSearchModel(
    η_pandemic = -0.21,   # Known pandemic elasticity
    f_pandemic = 0.5,     # Known pandemic job finding rate
    s_pandemic = 0.12,     # Known pandemic separation rate
    f_normal = 0.10,       # Known normal job finding rate
    s_normal = 0.05,      # Known normal separation rate
    ρ_pandemic = 0.9,     # Pandemic replacement rate
    ρ_normal = 0.5        # Normal replacement rate
)

calibrate_model(model)
