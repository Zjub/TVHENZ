using Parameters, Plots

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

function compute_J_series(y_vec, w_vec, β, s, T)
    J_vec = zeros(T + 2)  # one extra for terminal condition
    for t in T+1:-1:1
        J_vec[t] = y_vec[t] - w_vec[t] + β * (1 - s) * J_vec[t+1]
    end
    return J_vec[1:end-1]  # drop terminal condition
end

function steady_state(p::ModelParams, π::Float64)
    b̄ = π * p.b_e + (1 - π) * p.b_i
    w = nash_wage(p.y, b̄, p.φ)
    J = (p.y - w) / (1 - p.β * (1 - p.s))
    θ = ((p.β * p.μ * J) / p.c)^(1 / p.α)
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
    ws[1] = ss.w
    θs[1] = ss.θ
    f_i_arr[1] = ss.f
    f_e_arr[1] = p_base.ξ * ss.f

    # Pre-fill wage and productivity paths for J computation
    w_vec = zeros(T + 1)
    y_vec = zeros(T + 1)

    for t in 1:T+1
        p = (t - 1 in shock_window) ? p_shock : p_base
        u_total = u_e[t] + u_i[t]
        π_t = u_total > 0 ? u_e[t] / u_total : π
        b̄ = π_t * p.b_e + (1 - π_t) * p.b_i
        w_vec[t] = nash_wage(p.y, b̄, p.φ)
        y_vec[t] = p.y
    end

    # Compute forward-looking J(t)
    J_vec = compute_J_series(y_vec, w_vec, p_base.β, p_base.s, T)

    for t in 2:T+1
        p = (t - 1 in shock_window) ? p_shock : p_base

        θ = ((p.β * p.μ * J_vec[t]) / p.c)^(1 / p.α)
        θs[t] = θ
        ws[t] = w_vec[t]

        rates = matching_rates(θ, p)
        f_i = rates.f
        f_e = p.ξ * f_i

        f_i_arr[t] = f_i
        f_e_arr[t] = f_e

        u_total = u_e[t-1] + u_i[t-1]
        u_e[t] = u_e[t-1] + p.s * (1 - u_total) - f_e * u_e[t-1]
        u_i[t] = u_i[t-1] + p.s * (1 - u_total) - f_i * u_i[t-1]
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
# 2. Example Run
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
    ξ = 0.536
)

π0 = 0.5
shock_window = 2:6

# Simulate for 10,000 periods
T = 10_000
res = simulate(p_base, p_shock, T, π0, shock_window)

# Plot only the first 10 periods (i.e. t = 0 to 10)
plot(0:10, res["f_e"][1:11], label="Eligible", lw=2, color=:blue)
plot!(0:10, res["f_i"][1:11], label="Ineligible", lw=2, color=:red)
xlabel!("Time (weeks)")
ylabel!("Job-Finding Rate")
title!("Job-Finding Rate (First 10 Periods)")


