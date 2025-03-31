# --------------------------------------
# 1. Define Data Structures for Parameters
# --------------------------------------
using Parameters

@with_kw struct WorkerTypeParams
    replacement_rate::Float64
end

@with_kw struct ModelParams
    β::Float64
    s::Float64
    μ::Float64
    α::Float64
    φ::Float64
    c::Float64
    y::Float64
    eligible::WorkerTypeParams
    ineligible::WorkerTypeParams
end

# --------------------------------------
# 2. Helper Functions for Matching and Wages
# --------------------------------------
"""
Compute job-finding rate f and job-filling rate q given tightness θ and matching efficiency params.
Ensures rates are between 0 and 1.
"""
function matching_rates(θ::Float64, params::ModelParams)
    if θ < 0
        θ = 0.0  # tightness cannot be negative
    end
    # Cobb-Douglas matching function: f = μ * θ^α, q = μ * θ^(α-1)
    # Ensure we don't return >1 probabilities
    let μ = params.μ, α = params.α
        # If no unemployment or no vacancies, handle edge cases:
        if θ == 0
            # θ = 0 means V=0 or U infinite; here no vacancies -> no matching
            return (f = 0.0, q = 0.0)
        elseif θ == Inf
            # θ infinite (very large V relative to U): all unemployed definitely find jobs, but fill probability -> 0
            return (f = 1.0, q = 0.0)
        end
        # Compute raw rates
        local f = μ * θ^α
        local q = μ * θ^(α-1)
        # Cap at 1.0
        if f > 1.0
            f = 1.0
        end
        if q > 1.0
            q = 1.0
        end
        return (f = f, q = q)
    end
end

"""
Compute the Nash-bargained wage for a given worker type.
- `y`: productivity
- `b`: unemployment benefit for this type (absolute amount per period)
- `φ`: worker's bargaining power
Returns the negotiated wage.
"""
function wage_nash(y::Float64, b::Float64, φ::Float64)
    # We use w = φ*y + (1-φ)*b 
    # (This is a simplified wage equation assuming the worker's outside option value ~ b.)
    return φ * y + (1 - φ) * b
end

# --------------------------------------
# 3. Equilibrium Solver for a Given State
# --------------------------------------
"""
Solve for equilibrium tightness (θ) and wage for a given worker type segment.
Inputs:
 - params: ModelParams (includes current y, μ, etc.)
 - worker: WorkerTypeParams for the segment (to get replacement_rate)
Output:
 - (θ, w) tuple: equilibrium labor market tightness and wage for that segment.
"""
function solve_equilibrium_segment(params::ModelParams, worker::WorkerTypeParams)
    # Compute the worker's unemployment benefit level b (we take b as replacement_rate * productivity * φ for simplicity).
    # Here we approximate the benefit as replacement_rate * (pre-shock wage or current productivity). 
    # We can use current productivity * replacement_rate as a proxy.
    # (Alternatively, one could keep an absolute benefit level, but we'll derive it from replacement_rate and output.)
    b = worker.replacement_rate * params.y
    
    # Compute wage via Nash bargaining
    w = wage_nash(params.y, b, params.φ)
    
    # Compute the firm's per-match value J = (y - w) / [1 - β * (1 - s)]
    # (Present value of a filled job's profit flow.)
    profit_flow = params.y - w
    J = profit_flow
    if params.β > 0 && params.s < 1
        J = profit_flow / (1 - params.β * (1 - params.s))
    end
    
    # Solve free-entry condition: c = β * q(θ) * J.
    # Rearr: q(θ) = c/(β*J). Using q(θ) = μ * θ^(α-1).
    # => μ * θ^(α-1) = c/(β*J) => θ^(α-1) = c/(β*μ*J).
    θ = 0.0
    if J > 0 && params.β * params.μ * J > 0
        # Compute θ from the above formula (note: α-1 is negative since 0<α<1).
        θ = (params.c / (params.β * params.μ * J)) ^ (1 / (params.α - 1))
        # Guard against extreme values:
        if θ < 1e-8
            θ = 0.0
        elseif θ > 1e8
            θ = Inf
        end
    else
        # If J <= 0, no profitable jobs: θ remains 0 (no vacancies).
        θ = 0.0
    end
    
    return (θ, w)
end

# --------------------------------------
# 4. Simulation Function
# --------------------------------------
"""
Simulate the dynamic path of unemployment for each worker type over T periods.
Parameters:
 - params: ModelParams for pre-shock (baseline) environment.
 - shock_params: ModelParams for shock environment (e.g., with lowered y, μ, changed benefits).
 - T: total number of periods to simulate.
 - shock_start: period index at which shock_params are applied (before this, baseline params).
 - shock_end: period index at which shock ends (after this, revert to baseline params).
Returns:
 - A dictionary with time series (arrays) for U_e (eligible unemployment), U_i (ineligible unemployment),
   and optionally other variables like θ_e, θ_i, wages, etc.
"""
function simulate_model(params::ModelParams, shock_params::ModelParams, 
                        T::Int, shock_start::Int, shock_end::Int)
    # Initialize arrays to store results
    U_e = zeros(Float64, T+1)   # unemployment rate of eligible (as fraction of that group's labor force)
    U_i = zeros(Float64, T+1)   # unemployment rate of ineligible
    θ_e = zeros(Float64, T+1)
    θ_i = zeros(Float64, T+1)
    wage_e = zeros(Float64, T+1)
    wage_i = zeros(Float64, T+1)
    
    # Set initial unemployment to baseline steady-state (solve using baseline params):
    # Solve equilibrium for each group under baseline to get f and then u = s/(s+f).
    (θ0_e, w0_e) = solve_equilibrium_segment(params, params.eligible)
    (θ0_i, w0_i) = solve_equilibrium_segment(params, params.ineligible)
    let rates_e = matching_rates(θ0_e, params), rates_i = matching_rates(θ0_i, params)
        f0_e = rates_e.f
        f0_i = rates_i.f
        U_e[1] = if (f0_e + params.s) > 0 
                    params.s / (params.s + f0_e) 
                 else 
                    0.0 
                 end
        U_i[1] = if (f0_i + params.s) > 0 
                    params.s / (params.s + f0_i) 
                 else 
                    0.0 
                 end
    end
    θ_e[1] = θ0_e;  wage_e[1] = w0_e
    θ_i[1] = θ0_i;  wage_i[1] = w0_i
    
    # Time loop
    for t in 2:(T+1)
        # Determine which parameters apply in this period
        local current_params = params
        if t-1 >= shock_start && t-1 <= shock_end
            current_params = shock_params
        end
        if t-1 > shock_end
            current_params = params  # after shock, back to baseline
        end
        
        # Solve equilibrium tightness and wage for each group with current params
        (θ_e_t, w_e_t) = solve_equilibrium_segment(current_params, current_params.eligible)
        (θ_i_t, w_i_t) = solve_equilibrium_segment(current_params, current_params.ineligible)
        θ_e[t] = θ_e_t;   wage_e[t] = w_e_t
        θ_i[t] = θ_i_t;   wage_i[t] = w_i_t
        
        # Compute matching rates
        let rates_e = matching_rates(θ_e_t, current_params), rates_i = matching_rates(θ_i_t, current_params)
            f_e = rates_e.f
            f_i = rates_i.f
            # Update unemployment using U_next = U + s*(1-U) - f*U
            U_e[t] = U_e[t-1] + current_params.s * (1 - U_e[t-1]) - f_e * U_e[t-1]
            U_i[t] = U_i[t-1] + current_params.s * (1 - U_i[t-1]) - f_i * U_i[t-1]
            # Ensure bounds [0,1]
            if U_e[t] < 0; U_e[t] = 0.0; end
            if U_e[t] > 1; U_e[t] = 1.0; end
            if U_i[t] < 0; U_i[t] = 0.0; end
            if U_i[t] > 1; U_i[t] = 1.0; end
        end
    end
    
    return Dict(
        "U_e" => U_e, "U_i" => U_i,
        "θ_e" => θ_e, "θ_i" => θ_i,
        "wage_e" => wage_e, "wage_i" => wage_i
    )
end

# --------------------------------------
# 5. Example Usage of the Model Scaffold
# --------------------------------------
# Define baseline (pre-pandemic) parameters:
baseline_params = ModelParams(
    β = 0.99,            # discount factor (e.g. quarterly or monthly)
    s = 0.1,             # separation rate per period (10%)
    μ = 0.5,             # matching efficiency 
    α = 0.5,             # matching elasticity
    φ = 0.5,             # bargaining power
    c = 1.5,             # vacancy cost 
    y = 1.0,             # productivity (normalized to 1)
    eligible = WorkerTypeParams(0.28),    # 28% replacement rate
    ineligible = WorkerTypeParams(0.0)    # 0% replacement
)

# Define shock (pandemic) parameters (we copy baseline and modify relevant fields):
shock_params = ModelParams(
    β = baseline_params.β,
    s = baseline_params.s,
    μ = 0.4,             # 20% drop in matching efficiency (0.5 -> 0.4)
    α = baseline_params.α,
    φ = baseline_params.φ,
    c = baseline_params.c,
    y = 0.9 * baseline_params.y,        # 10% drop in productivity
    eligible = WorkerTypeParams(0.56),  # 56% replacement rate during pandemic
    ineligible = WorkerTypeParams(0.0)  # still 0%
)

# Simulate for T periods, with shock from period 1 to 5 (for example)
T = 12
shock_start = 1
shock_end = 5
result = simulate_model(baseline_params, shock_params, T, shock_start, shock_end)

# For demonstration, print unemployment rates over time for each group:
println("Unemployment rate (eligible) over time: ", round.(result["U_e"] .* 100, digits=1), "%")
println("Unemployment rate (ineligible) over time: ", round.(result["U_i"] .* 100, digits=1), "%")
println("Tightness (eligible) over time: ", round.(result["θ_e"], digits=2))
println("Tightness (ineligible) over time: ", round.(result["θ_i"], digits=2))
println("Wages (eligible vs ineligible) over time: ")
for t in 1:(T+1)
    println(" Period $t: w_e = $(round(result["wage_e"][t], digits=2)), w_i = $(round(result["wage_i"][t], digits=2))")
end
