#using Pkg
#Pkg.add("Plots")
#Pkg.add("LaTeXStrings")

using Parameters, Plots, LaTeXStrings

# -----------------------------
# 1. Model Definition
# -----------------------------
@with_kw struct ModelParams
    β::Float64
    s::Float64
    μ::Float64
    α::Float64
    φ::Float64
    c::Float64
    y::Float64
    b_e::Float64
    b_i::Float64
    ξ::Float64
end

function matching_rates(θ, p::ModelParams)
    f = p.μ * θ^(1 - p.α)
    q = p.μ * θ^(-p.α)
    return (f = min(f, 1.0), q = min(q, 1.0))
end

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
p_base = ModelParams(
    β = 0.99, s = 0.1, μ = 0.5, α = 0.5,
    φ = 0.5, c = 1.0, y = 1.0,
    b_e = 0.28, b_i = 0.0,
    ξ = 1.0
)

p_shock = ModelParams(
    β = 0.99, s = 0.1, μ = 0.4, α = 0.5,
    φ = 0.5, c = 1.0, y = 0.9,
    b_e = 0.56, b_i = 0.0,
    ξ = 0.81
)

p_counter = ModelParams(
    β = 0.99, s = 0.1, μ = 0.4, α = 0.5,
    φ = 0.5, c = 1.0, y = 0.9,
    b_e = 0.28, b_i = 0.0,
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
