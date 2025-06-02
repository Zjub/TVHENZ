# Actual model - here we calibrate the cost of search and match efficiency. Want to create a version where we instead include disutility of work directly.

#using Pkg
#Pkg.add("Printf")

using Parameters, Optim, Printf, PrettyTables, Plots, Printf

# -----------------------------
# 1. Model Definition
# -----------------------------
@with_kw struct ModelParams
    β::Float64       # Discount factor per period (e.g. weekly). Reflects how much agents value future payoffs vs current. Typical value: 0.99
    s::Float64       # Separation rate: probability an employed worker loses their job in a given period. Example: 0.1 = 10% weekly separation
    α::Float64       # Matching function elasticity with respect to unemployment (or effective searchers). Usually 0.5–0.7
    φ::Float64       # Worker's Nash bargaining power. Share of the match surplus that goes to the worker. 0.5 = equal split
    c::Float64       # Cost to post a vacancy (per period). Flow cost firms incur when trying to hire. Often normalized to 1.0
    y::Float64       # Output produced per employed worker. Usually normalized to 1.0
    b_e::Float64     # Unemployment benefit for UI-eligible workers (per period). Typically a fraction of y or the wage
    b_i::Float64     # Unemployment benefit for UI-ineligible workers. Often 0
    κ::Float64       # Search cost parameter. Determines the costliness of exerting search effort. Higher κ means less effort
end


function optimal_effort(b, y, φ, κ)
    surplus = φ * y + (1 - φ) * b - b
    return clamp(surplus / κ, 0.01, 1.0)
end

function steady_state_f(p::ModelParams, μ::Float64)
    e_i = optimal_effort(p.b_i, p.y, p.φ, p.κ) #1.0 if normalising
    e_e = optimal_effort(p.b_e, p.y, p.φ, p.κ)
    e_eff = 0.5 * e_i + 0.5 * e_e
    w = p.φ * p.y + (1 - p.φ) * (0.5 * p.b_e + 0.5 * p.b_i)
    J = (p.y - w) / (1 - p.β * (1 - p.s))
    q = p.c / (p.β * J)
    θ = (q / μ)^(-1 / p.α)
    f_i = μ * θ^(1 - p.α)
    f_e = e_e * f_i
    return f_i, f_e
end

function calibrate_params(f_i_target, f_e_target, b_e_target)
    function objective(x)
        μ, κ = x
        p = ModelParams(β=0.99, s=0.1, α=0.5, φ=0.5,
                        c=1.0, y=1.0,
                        b_e=b_e_target, b_i=0.0, κ=κ)
        f_i, f_e = steady_state_f(p, μ)
        return (f_i - f_i_target)^2 + (f_e - f_e_target)^2
    end

    result = optimize(
        objective,
        [0.01, 0.01],   # lower bounds
        [1.0, 20.0],    # upper bounds
        [0.5, 2.0],     # initial guess
        Fminbox(GradientDescent())
    )

    μ_opt, κ_opt = result.minimizer
    return μ_opt, κ_opt
end

# -----------------------------
# 2. Dynamic Simulation
# -----------------------------
function simulate(p::ModelParams, μ::Float64, T::Int)
    f_e = zeros(T+1)
    f_i = zeros(T+1)

    for t in 1:(T+1)
        effort = optimal_effort(p.b_e, p.y, p.φ, p.κ)
        w = p.φ * p.y + (1 - p.φ) * (0.5 * p.b_e + 0.5 * p.b_i)
        J = (p.y - w) / (1 - p.β * (1 - p.s))
        q = p.c / (p.β * J)
        θ = (q / μ)^(-1 / p.α)
        f_i[t] = μ * θ^(1 - p.α)
        f_e[t] = effort * f_i[t]
    end

    return f_i, f_e
end

# -----------------------------
# 3. Calibration Inputs
# -----------------------------
f_i_pre, f_e_pre, b_e_pre = 0.10, 0.0866, 0.28
f_i_post, f_e_post, b_e_post = 0.0821, 0.0312, 0.56

μ_pre, κ_pre = calibrate_params(f_i_pre, f_e_pre, b_e_pre)
μ_post, κ_post = calibrate_params(f_i_post, f_e_post, b_e_post)

p_pre = ModelParams(β=0.99, s=0.1, α=0.5, φ=0.5,
                    c=1.0, y=1.0, b_e=b_e_pre, b_i=0.0, κ=κ_pre)

p_post = ModelParams(β=0.99, s=0.1, α=0.5, φ=0.5,
                     c=1.0, y=1.0, b_e=b_e_post, b_i=0.0, κ=κ_post)

p_counter = ModelParams(β=0.99, s=0.1, α=0.5, φ=0.5,
                        c=1.0, y=1.0, b_e=b_e_post, b_i=0.0, κ=κ_pre)

# -----------------------------
# 4. Simulation
# -----------------------------
T = 20
f_i_pre_path, f_e_pre_path = simulate(p_pre, μ_pre, T)
f_i_post_path, f_e_post_path = simulate(p_post, μ_post, T)
_, f_e_counter_path = simulate(p_counter, μ_post, T)

# -----------------------------
# 5. Print Results Table
# -----------------------------
f_i_model_pre, f_e_model_pre = steady_state_f(p_pre, μ_pre)
f_i_model_post, f_e_model_post = steady_state_f(p_post, μ_post)

header = ["", "Pre-Shock", "Post-Shock", "Change"]
data = [
    ["μ", round(μ_pre, digits=4), round(μ_post, digits=4), round(μ_post - μ_pre, digits=4)],
    ["κ", round(κ_pre, digits=4), round(κ_post, digits=4), round(κ_post - κ_pre, digits=4)],
    ["b_e", round(b_e_pre, digits=4), round(b_e_post, digits=4), round(b_e_post - b_e_pre, digits=4)],
    ["f_i", round(f_i_model_pre, digits=4), round(f_i_model_post, digits=4), round(f_i_model_post - f_i_model_pre, digits=4)],
    ["f_e", round(f_e_model_pre, digits=4), round(f_e_model_post, digits=4), round(f_e_model_post - f_e_model_pre, digits=4)]
]

print(data)

# -----------------------------
# 6. Plot
# -----------------------------
plot(0:T, f_e_pre_path, label="Eligible – Pre-shock", lw=2, color=:black)
plot!(0:T, f_e_post_path, label="Eligible – Post-shock", lw=2, color=:blue)
plot!(0:T, f_e_counter_path, label="Eligible – Counterfactual (Benefit only)", lw=2, color=:green, ls=:dash)

xlabel!("Time")
ylabel!("Job-Finding Rate (fₑ)")
title!("Eligible Job-Finding Rate Dynamics")

# First values for each scenario
fₑ0_pre = f_e_pre_path[1]
fₑ0_post = f_e_post_path[1]
fₑ0_counter = f_e_counter_path[1]

# Labels and values
labels = ["Pre-shock", "Post-shock", "Counterfactual"]
values = [fₑ0_pre, fₑ0_post, fₑ0_counter]

# Define bar colors (any named color or hex code)
my_colors = [:black, :blue, :green]

# Create bar plot with colors
p = bar(labels, values;
    color = my_colors,
    legend = false,
    ylabel = "Job-Finding Rate (fₑ)",
    title = "Job-Finding Rates (Eligible)")

# Add value annotations
annotate!(p, [(i, v + 0.002, text(round(v, digits=3), :center, 8)) for (i, v) in enumerate(values)]...)

p

filename = @sprintf("job_finding_rate_eligible_b%02d.pdf", Int(round(b_e_pre * 100)))
savefig(p, filename)