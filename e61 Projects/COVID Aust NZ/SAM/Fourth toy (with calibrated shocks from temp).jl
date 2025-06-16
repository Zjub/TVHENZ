#using Pkg
#Pkg.add("LaTeXStrings")

using Parameters, Plots, LaTeXStrings

# -----------------------------
# 1. Model Definition
# -----------------------------
@with_kw struct ModelParams
    β::Float64      # Discount factor
    s::Float64      # Separation rate - exogenous
    μ::Float64
    α::Float64      # Matching elasticity wrt unemployment
    φ::Float64      # Worker's bargaining power
    c::Float64      # Vacancy posting cost
    y::Float64      # Productivity
    b_e::Float64    # Benefit for eligible
    b_i::Float64    # Benefit for ineligible
    ξ::Float64  
    l_u::Float64    # Value of leisure in unemployment  
end

# f and q represent the job finding rate and vacancy filling rate.

function matching_rates(θ, p::ModelParams)
    f = p.μ * θ^(1 - p.α)
    q = p.μ * θ^(-p.α)
    return (f = min(f, 1.0), q = min(q, 1.0))
end

# The Nash Wage is a function of the reservvation wage = b̄
function nash_wage(y, b̄, φ)
    return φ * y + (1 - φ) * b̄
end

function firm_surplus(y, w, β, s)
    return (y - w) / (1 - β * (1 - s))
end

function solve_tightness(p::ModelParams, w)
    J = firm_surplus(p.y, w, p.β, p.s)
    return ((p.β * p.μ * J) / p.c)^(1 / p.α)
end

# The Nash Wage is determined as a weighted sum of benefit payments - think we should attempt something different. XX 
function steady_state(p::ModelParams, π::Float64)
    b̄ = π * p.b_e + (1 - π) * p.b_i
    w = nash_wage(p.y, b̄, p.φ)
    θ = solve_tightness(p, w)
    rates = matching_rates(θ, p)
    f = rates.f
    u = p.s / (p.s + f)
    return (θ = θ, w = w, f = f, u = u)
end

function simulate(p_base::ModelParams, p_shock::ModelParams, T::Int, π::Float64, shock_window::UnitRange{Int})
    u_e = zeros(T + 1)
    u_i = zeros(T + 1)
    θs = zeros(T + 1)
    ws = zeros(T + 1)
    f_e_arr = zeros(T + 1)
    f_i_arr = zeros(T + 1)

    ss = steady_state(p_base, π)
    u_e[1] = π * ss.u
    u_i[1] = (1 - π) * ss.u
    θs[1] = ss.θ
    ws[1] = ss.w
    f_i_arr[1] = ss.f
    f_e_arr[1] = p_base.ξ * ss.f

    for t in 2:T+1
        p = t - 1 in shock_window ? p_shock : p_base

        u_total = u_e[t-1] + u_i[t-1]
        π_t = u_total > 0 ? u_e[t-1] / u_total : π
        b̄ = π_t * p.b_e + (1 - π_t) * p.b_i

        w = nash_wage(p.y, b̄, p.φ)
        θ = solve_tightness(p, w)
        rates = matching_rates(θ, p)
        f_i = rates.f
        f_e = p.ξ * f_i

        u_e[t] = u_e[t-1] + p.s * (1 - u_total) - f_e * u_e[t-1]
        u_i[t] = u_i[t-1] + p.s * (1 - u_total) - f_i * u_i[t-1]

        θs[t] = θ
        ws[t] = w
        f_i_arr[t] = f_i
        f_e_arr[t] = f_e
    end

    return Dict(
        "u_e" => u_e,
        "u_i" => u_i,
        "θ" => θs,
        "w" => ws,
        "f_i" => f_i_arr,
        "f_e" => f_e_arr
    )
end

# -----------------------------
# 2. Parameter Setup
# -----------------------------
# Add 0.15 to represent value of leisure

l = 0.15

p_base = ModelParams(
    β = 0.99, s = 0.1, μ = 0.5, α = 0.5, l_u = l,
    φ = 0.5, c = 1.0, y = 1.0,
    b_e = 0.28 + l, b_i = 0.0 + l,
    ξ = 1.0
)

p_shock = ModelParams(
    β = 0.99, s = 0.1, μ = 0.4, α = 0.5, l_u = l,
    φ = 0.5, c = 1.0, y = 0.9,
    b_e = 0.56 + l, b_i = 0.0 + l,
    ξ = 0.536
)

p_counter = ModelParams(
    β = 0.99, s = 0.1, μ = 0.4, α = 0.5, l_u = l,
    φ = 0.5, c = 1.0, y = 0.9,
    b_e = 0.28 + l, b_i = 0.0 + l,
    ξ = 1.0
)

# -----------------------------
# 3. Run Simulations
# -----------------------------
T = 20
shock_window = 2:6
π0 = 0.5

res_actual = simulate(p_base, p_shock, T, π0, shock_window)
res_counter = simulate(p_base, p_counter, T, π0, shock_window)

# -----------------------------
# 4. Print Key Differences
# -----------------------------
println("\nDifference in unemployment rate (eligible):")
println(round.(res_actual["u_e"] .- res_counter["u_e"], digits=3))

println("\nDifference in job-finding rate (eligible):")
println(round.(res_actual["f_e"] .- res_counter["f_e"], digits=3))

println("\nDifference in wages:")
println(round.(res_actual["w"] .- res_counter["w"], digits=3))

# -----------------------------
# 5. Plots
# -----------------------------
plot(0:T, res_actual["u_e"], label="Eligible - Actual", lw=2, color=:blue)
plot!(0:T, res_actual["u_i"], label="Ineligible - Actual", lw=2, color=:red)
plot!(0:T, res_counter["u_e"], label="Eligible - Counterfactual", ls=:dash, color=:blue)
plot!(0:T, res_counter["u_i"], label="Ineligible - Counterfactual", ls=:dash, color=:red)
xlabel!("Time (weeks)")
ylabel!("Unemployment rate")
title!("Unemployment Dynamics: Actual vs Counterfactual")

plot(0:T, res_actual["w"], label="Actual", lw=2, color=:green)
plot!(0:T, res_counter["w"], label="Counterfactual", lw=2, ls=:dash, color=:black)
xlabel!("Time")
ylabel!("Wage")
title!("Wage Path: Actual vs Counterfactual")

plot(0:T, res_actual["f_e"], label="Eligible - Actual", lw=2, color=:blue)
plot!(0:T, res_counter["f_e"], label="Eligible - Counterfactual", lw=2, ls=:dash, color=:black)
xlabel!("Time")
ylabel!("Job-Finding Rate")
title!("Job-Finding Rate for Eligible Workers")

# 1. Baseline: everything normal
res_baseline = simulate(p_base, p_base, T, π0, 1:0)  # no shock

# 2. Full shock (μ↓, y↓, b_e↑, ξ↓)
res_full = simulate(p_base, p_shock, T, π0, 2:6)

# 3. Demand-only shock (μ↓, y↓, b_e baseline, ξ = 1)
p_demandonly = ModelParams(
    β = 0.99, s = 0.1, μ = 0.4, α = 0.5, l_u = l,
    φ = 0.5, c = 1.0, y = 0.9,
    b_e = 0.28 + l, b_i = 0.0 + l,
    ξ = 0.644
)
res_demandonly = simulate(p_base, p_demandonly, T, π0, 2:6)

# 4. Compute f_e at the trough (e.g. period 6 or 7)
t_peak = 7
f_e_base = res_baseline["f_e"][t_peak]
f_e_full = res_full["f_e"][t_peak]
f_e_match = res_demandonly["f_e"][t_peak]

# 5. Calculate changes
fall_obs = f_e_full / f_e_base
fall_match = f_e_match / f_e_base

println("\nObserved JFR (f_e) fall at t = $t_peak: ", round(1 - fall_obs, digits=3) * 100, "%")
println("Counterfactual JFR fall due to matching shock only: ", round(1 - fall_match, digits=3) * 100, "%")
println("Implied contribution of UI changes to fall in JFR: ", round((fall_match - fall_obs), digits=3) * 100, "% points (in proportional terms)")

# -----------------------------
# 6. Plot JFRs Across Scenarios
# -----------------------------
plot(0:T, res_baseline["f_e"], label="Baseline (no shock)", lw=2, color=:black)
plot!(0:T, res_full["f_e"], label="Actual (UI + matching)", lw=2, color=:blue)
plot!(0:T, res_demandonly["f_e"], label="Matching only", lw=2, ls=:dash, color=:red)
plot!(0:T, res_counter["f_e"], label="Counterfactual (same gap, baseline UI)", lw=2, ls=:dot, color=:green)
xlabel!("Time (weeks)")
ylabel!("Job-Finding Rate (fₑ)")
title!("Job-Finding Rate for UI Recipients Across Scenarios")

# -----------------------------
# 7. Plot JFR Gaps (f_i - f_e) Across Scenarios
# -----------------------------
gap_baseline = res_baseline["f_i"] .- res_baseline["f_e"]
gap_full     = res_full["f_i"]     .- res_full["f_e"]
gap_demand   = res_demandonly["f_i"] .- res_demandonly["f_e"]
gap_counter  = res_counter["f_i"]  .- res_counter["f_e"]

plot(0:T, gap_baseline, label="Baseline (no shock)", lw=2, color=:black)
plot!(0:T, gap_full, label="Actual (UI + matching)", lw=2, color=:blue)
plot!(0:T, gap_demand, label="Matching only", lw=2, ls=:dash, color=:red)
#plot!(0:T, gap_counter, label="Counterfactual (same gap)", lw=2, ls=:dot, color=:green)
xlabel!("Time (weeks)")
ylabel!("Gap in Job-Finding Rate (fᵢ - fₑ)")
title!("Job-Finding Rate Gaps: Ineligible vs Eligible Workers")
