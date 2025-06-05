# Version with disutility of work and match efficiency - no search cost calibration (need a third target for this)
using Parameters, Optim, PrettyTables, Plots

# -----------------------------
# 1. Model Definition
# -----------------------------
@with_kw struct ModelParams
    β::Float64      # Discount factor
    s::Float64      # Separation rate
    α::Float64      # Matching elasticity wrt unemployment
    φ::Float64      # Worker's bargaining power
    c::Float64      # Vacancy posting cost
    y::Float64      # Productivity
    b_e::Float64    # Benefit for eligible
    b_i::Float64    # Benefit for ineligible
    κ::Float64      # Disutility of effort
end

function optimal_effort(b, y, φ, κ)
    surplus = φ * y + (1 - φ) * b - b
    return clamp(surplus / κ, 0.01, 1.0)
end

function steady_state_f(p::ModelParams, μ::Float64)
    e_i = 1.0
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

# -----------------------------
# 2. Calibration Function
# -----------------------------
function calibrate_mu_kappa(f_i_target, f_e_target, b_e_target)
    function objective(x)
        μ, κ = x
        p = ModelParams(β=0.99, s=0.1, α=0.5, φ=0.5,
                        c=1.0, y=1.0, b_e=b_e_target, b_i=0.0, κ=κ)
        f_i, f_e = steady_state_f(p, μ)
        return (f_i - f_i_target)^2 + (f_e - f_e_target)^2
    end

    result = optimize(
        objective,
        [0.01, 0.01],       # Lower bounds
        [1.0, 20.0],        # Upper bounds
        [0.5, 2.0],         # Initial guess
        Fminbox(GradientDescent())
    )

    μ_opt = result.minimizer[1]
    κ_opt = result.minimizer[2]
    return μ_opt, κ_opt
end

# -----------------------------
# 3. Calibration Targets
# -----------------------------
f_i_pre, f_e_pre, b_e_pre = 0.10, 0.0866, 0.28
f_i_post, f_e_post, b_e_post = 0.0821, 0.0312, 0.56

# -----------------------------
# 4. Calibrate Parameters
# -----------------------------
μ_pre, κ_pre = calibrate_mu_kappa(f_i_pre, f_e_pre, b_e_pre)
μ_post, κ_post = calibrate_mu_kappa(f_i_post, f_e_post, b_e_post)

# -----------------------------
# 5. Define Scenarios
# -----------------------------
p_pre = ModelParams(β=0.99, s=0.1, α=0.5, φ=0.5,
                    c=1.0, y=1.0, b_e=b_e_pre, b_i=0.0, κ=κ_pre)

p_post = ModelParams(β=0.99, s=0.1, α=0.5, φ=0.5,
                     c=1.0, y=1.0, b_e=b_e_post, b_i=0.0, κ=κ_post)

p_counter = ModelParams(β=0.99, s=0.1, α=0.5, φ=0.5,
                        c=1.0, y=1.0, b_e=b_e_post, b_i=0.0, κ=κ_pre)

f_i_model_pre, f_e_model_pre = steady_state_f(p_pre, μ_pre)
f_i_model_post, f_e_model_post = steady_state_f(p_post, μ_post)
f_i_model_counter, f_e_model_counter = steady_state_f(p_counter, μ_pre)

# -----------------------------
# 6. Create Table
# -----------------------------
header = ["", "Pre-Shock", "Post-Shock", "Counterfactual", "Δ Post-Pre"]
data = [
    ["μ", round(μ_pre, digits=4), round(μ_post, digits=4), round(μ_pre, digits=4), round(μ_post - μ_pre, digits=4)],
    ["κ", round(κ_pre, digits=4), round(κ_post, digits=4), round(κ_pre, digits=4), round(κ_post - κ_pre, digits=4)],
    ["bₑ", round(b_e_pre, digits=4), round(b_e_post, digits=4), round(b_e_post, digits=4), round(b_e_post - b_e_pre, digits=4)],
    ["fᵢ", round(f_i_model_pre, digits=4), round(f_i_model_post, digits=4), round(f_i_model_counter, digits=4), round(f_i_model_post - f_i_model_pre, digits=4)],
    ["fₑ", round(f_e_model_pre, digits=4), round(f_e_model_post, digits=4), round(f_e_model_counter, digits=4), round(f_e_model_post - f_e_model_pre, digits=4)]
]

print(data)

# -----------------------------
# 7. Bar Plot
# -----------------------------
bar_data = [
    [f_e_model_pre, f_e_model_post, f_e_model_counter],  # eligible
    [f_i_model_pre, f_i_model_post, f_i_model_counter]   # ineligible
]

# --- Labels and Values ---
labels = ["Pre-shock", "Post-shock", "Counterfactual"]
values = bar_data[1]
my_colors = [:black, :blue, :green]

# --- Simple Side-by-Side Bar Plot ---
bar(labels, values;
    color = my_colors,
    legend = false,
    ylabel = "Job-Finding Rate (fₑ)",
    title = "Job-Finding Rates (Eligible)",
    ylim = (0, 0.1))