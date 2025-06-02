## Calibrating on two values is not working right now - sort out later

using Parameters, Plots, NLsolve

# -----------------------------
# 1. Model Setup
# -----------------------------
@with_kw struct ModelParams
    β::Float64; s::Float64; μ::Float64; α::Float64
    φ::Float64; c::Float64; y::Float64
    b_e::Float64; b_i::Float64; ξ::Float64
end

function matching_rates(θ, p::ModelParams)
    f = p.μ * θ^(1 - p.α)
    q = p.μ * θ^(-p.α)
    return (f = min(f, 1.0), q = min(q, 1.0))
end

function nash_wage(y, b̄, φ) return φ * y + (1 - φ) * b̄ end
function firm_surplus(y, w, β, s) return (y - w) / (1 - β * (1 - s)) end
function solve_tightness(p::ModelParams, w)
    J = firm_surplus(p.y, w, p.β, p.s)
    return ((p.β * p.μ * J) / p.c)^(1 / p.α)
end

function steady_state(p::ModelParams, π::Float64)
    b̄ = π * p.b_e + (1 - π) * p.b_i
    w = nash_wage(p.y, b̄, p.φ)
    θ = solve_tightness(p, w)
    f = matching_rates(θ, p).f
    u = p.s / (p.s + f)
    return (θ = θ, w = w, f = f, u = u)
end

function simulate(p_base::ModelParams, p_shock::ModelParams, T::Int, π::Float64, shock_window::UnitRange{Int})
    u_e = zeros(T+1); u_i = zeros(T+1); θs = zeros(T+1); ws = zeros(T+1)
    f_e_arr = zeros(T+1); f_i_arr = zeros(T+1)
    ss = steady_state(p_base, π)
    u_e[1] = π * ss.u; u_i[1] = (1 - π) * ss.u
    θs[1] = ss.θ; ws[1] = ss.w; f_i_arr[1] = ss.f; f_e_arr[1] = p_base.ξ * ss.f

    for t in 2:T+1
        p = (t-1 in shock_window) ? p_shock : p_base
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
        θs[t] = θ; ws[t] = w; f_i_arr[t] = f_i; f_e_arr[t] = f_e
    end

    return Dict("u_e" => u_e, "u_i" => u_i, "θ" => θs, "w" => ws, "f_i" => f_i_arr, "f_e" => f_e_arr)
end

# -----------------------------
# 2. Calibration Functions
# -----------------------------
function calibrate_mu_y(p_base::ModelParams, target_f_i::Float64, π::Float64, t_check::Int)
    function objective(params)
        μ_trial, y_trial = params
        p_trial = ModelParams(
            β = p_base.β, s = p_base.s, μ = μ_trial, α = p_base.α,
            φ = p_base.φ, c = p_base.c, y = y_trial,
            b_e = p_base.b_e, b_i = p_base.b_i, ξ = 1.0
        )
        res = simulate(p_base, p_trial, t_check, π, 2:6)
        return [res["f_i"][t_check + 1] - target_f_i]
    end
    return nlsolve(objective, [0.4, 0.9]; method = :anderson, ftol = 1e-6).zero
end

function implied_xi_given_gap(p_base::ModelParams, p_partial::ModelParams, π::Float64, shock_window::UnitRange{Int}, gap::Float64, t_check::Int)
    function objective(ξ_guess)
        ξ = clamp(ξ_guess[1], 1e-4, 0.9999)
        p_test = ModelParams(
            β = p_partial.β, s = p_partial.s, μ = p_partial.μ, α = p_partial.α,
            φ = p_partial.φ, c = p_partial.c, y = p_partial.y,
            b_e = p_partial.b_e, b_i = p_partial.b_i, ξ = ξ
        )
        res = simulate(p_base, p_test, t_check, π, shock_window)
        f_i = res["f_i"][t_check + 1]; f_e = res["f_e"][t_check + 1]
        return [f_e - (f_i - gap)]
    end
    return nlsolve(objective, [0.8]; method = :anderson, ftol = 1e-6).zero[1]
end

# -----------------------------
# 3. Run Calibration and Simulation
# -----------------------------
π0 = 0.5
T = 20
t_check = 6
shock_window = 2:6

p_base = ModelParams(β = 0.99, s = 0.1, μ = 0.5, α = 0.5, φ = 0.5, c = 1.0, y = 1.0, b_e = 0.28, b_i = 0.0, ξ = 1.0)

target_f_i = 0.0823  # from 10.02% baseline minus 1.79ppt drop
μ_cal, y_cal = calibrate_mu_y(p_base, target_f_i, π0, t_check)

p_partial = ModelParams(β = 0.99, s = 0.1, μ = μ_cal, α = 0.5, φ = 0.5, c = 1.0, y = y_cal, b_e = 0.56, b_i = 0.0, ξ = 1.0)
gap = 0.0175  # additional drop for eligibles
ξ_star = implied_xi_given_gap(p_base, p_partial, π0, shock_window, gap, t_check)

println("✅ Calibrated μ = ", round(μ_cal, digits=3), ", y = ", round(y_cal, digits=3))
println("✅ Calibrated ξ to generate 1.75ppt JFR gap = ", round(ξ_star, digits=3))

p_final = ModelParams(β = 0.99, s = 0.1, μ = μ_cal, α = 0.5, φ = 0.5, c = 1.0, y = y_cal, b_e = 0.56, b_i = 0.0, ξ = ξ_star)
res = simulate(p_base, p_final, T, π0, shock_window)

f_i_t = res["f_i"][t_check + 1]
f_e_t = res["f_e"][t_check + 1]
println("\n🔍 JFR at t = $t_check: fᵢ = ", round(f_i_t, digits=4), ", fₑ = ", round(f_e_t, digits=4), ", gap = ", round(f_i_t - f_e_t, digits=4))
println("Relative drop in fₑ = ", round((1 - f_e_t / f_i_t) * 100, digits=1), "%")

plot(0:T, res["f_e"], label="Eligible (fₑ)", lw=2, color=:blue)
plot!(0:T, res["f_i"], label="Ineligible (fᵢ)", lw=2, color=:red)
xlabel!("Time (weeks)"); ylabel!("Job-Finding Rate")
title!("JFR Path: Calibrated to DiD Estimates")
