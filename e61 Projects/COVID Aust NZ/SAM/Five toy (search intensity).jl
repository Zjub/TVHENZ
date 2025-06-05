#using Pkg
#Pkg.add("PrettyTables")

using Parameters, Optim, Printf, PrettyTables

# -----------------------------
# 1. Model Definition
# -----------------------------
@with_kw struct ModelParams
    β::Float64
    s::Float64
    α::Float64
    φ::Float64
    c::Float64
    y::Float64
    b_e::Float64
    b_i::Float64
    κ::Float64
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
# 2. Calibrate Pre- and Post-Shock
# -----------------------------
f_i_pre, f_e_pre, b_e_pre = 0.10, 0.0866, 0.28
f_i_post, f_e_post, b_e_post = 0.0821, 0.0312, 0.56

μ_pre, κ_pre = calibrate_params(f_i_pre, f_e_pre, b_e_pre)
μ_post, κ_post = calibrate_params(f_i_post, f_e_post, b_e_post)

# -----------------------------
# 3. Display Results in Table
# -----------------------------
p_pre = ModelParams(β=0.99, s=0.1, α=0.5, φ=0.5,
                    c=1.0, y=1.0, b_e=b_e_pre, b_i=0.0, κ=κ_pre)
p_post = ModelParams(β=0.99, s=0.1, α=0.5, φ=0.5,
                     c=1.0, y=1.0, b_e=b_e_post, b_i=0.0, κ=κ_post)

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

pretty_table(data, header)

# Convert to matrix format expected by pretty_table
table_data = [row[i] for row in data, i in 1:length(row)]

# Display the table
pretty_table(reshape(table_data, length(data[1]), :)'; header=header)
