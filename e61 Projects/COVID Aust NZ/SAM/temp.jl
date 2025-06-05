using Parameters, Plots, NLsolve

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
# 2. Inversion to Find ξ Given Target Drop
# -----------------------------
function implied_xi_for_fe_gap(p_base::ModelParams, p_shock::ModelParams,
    π::Float64, shock_window::UnitRange{Int},
    gap::Float64, t_check::Int)

function objective(ξ_guess)
ξ = clamp(ξ_guess[1], 1e-4, 0.9999)
p_test = ModelParams(
β = p_shock.β, s = p_shock.s, μ = p_shock.μ, α = p_shock.α,
φ = p_shock.φ, c = p_shock.c, y = p_shock.y,
b_e = p_shock.b_e, b_i = p_shock.b_i, ξ = ξ
)
res = simulate(p_base, p_test, t_check, π, shock_window)
f_i = res["f_i"][t_check + 1]
f_e = res["f_e"][t_check + 1]
return [f_e - (f_i - gap)]
end

try
result = nlsolve(objective, [0.8]; method = :anderson, ftol = 1e-6)
return result.zero[1]
catch e
println("Solver failed: ", e)
return 1.0
end
end


# -----------------------------
# 3. Example Usage
# -----------------------------
p_base = ModelParams(
    β = 0.99, s = 0.1, μ = 0.5, α = 0.5,
    φ = 0.5, c = 1.0, y = 1.0,
    b_e = 0.28, b_i = 0.0,
    ξ = 1.0
)

p_shock = ModelParams(
    β = 0.99, s = 0.1, μ = 0.25, α = 0.5,
    φ = 0.5, c = 1.0, y = 0.9,
    b_e = 0.56, b_i = 0.0,
    ξ = 0.81  # placeholder, we'll solve for this
)

π0 = 0.5
T = 20
shock_window = 2:6
target_drop = 0.81  # 19% drop
t_check = 6

ξ_needed = implied_xi_for_fe_gap(p_base, p_shock, π0, shock_window, 0.19, t_check)

println("\\nTo generate a 19% drop in f_e at t = $t_check:")
println("Required ξ (search effectiveness of UI-eligible) = ", round(ξ_needed, digits=3))

# -----------------------------
# 4. Check the actual gap in f_e vs f_i
# -----------------------------
p_shock_calibrated = ModelParams(
    β = p_shock.β, s = p_shock.s, μ = p_shock.μ, α = p_shock.α,
    φ = p_shock.φ, c = p_shock.c, y = p_shock.y,
    b_e = p_shock.b_e, b_i = p_shock.b_i,
    ξ = ξ_needed
)

res = simulate(p_base, p_shock_calibrated, T, π0, shock_window)
f_e_actual = res["f_e"][t_check + 1]
f_i_actual = res["f_i"][t_check + 1]
abs_gap = f_i_actual - f_e_actual
prop_gap = 1 - (f_e_actual / f_i_actual)

println("\\nActual outcomes at t = $(t_check):")
println("f_i (non-recipients) = ", round(f_i_actual, digits=3))
println("f_e (recipients)     = ", round(f_e_actual, digits=3))
println("Absolute gap          = ", round(abs_gap, digits=3))
println("Proportional drop     = ", round(prop_gap * 100, digits=1), "%")

plot(0:T, res["f_e"], label="Eligible", lw=2, color=:blue)
plot!(0:T, res["f_i"], label="Ineligible", lw=2, color=:red)
xlabel!("Time")
ylabel!("Job-Finding Rate")
title!("JFR for Eligible vs Ineligible Workers")
