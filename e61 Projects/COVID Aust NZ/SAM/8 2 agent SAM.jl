
# search_matching_model_gridsearch_vacancy_leisure_bargaining_dual_agents.jl
# Full model with:
# - Two separate agent types (recipients & non-recipients)
# - Grid search calibration
# - Bellman fixed-point solutions for W_R, U_R, W_N, U_N
# - CairoMakie 3D plots and dynamic time series plots

using Printf, Plots, CairoMakie

force_surface_plot = true  # Toggle plots

# Parameters
β    = 0.99
δ    = 0.02
p_R  = 0.5
α    = 0.5
φ    = 0.5
y_pre = 1.0
y_post = 1
b_R_pre = 0.28
b_R_post = 2.0 * b_R_pre
b_N = 0.0
l_u = 0.1
c_vac = 0.5

f_N_target_pre = 0.10
f_R_target_pre = 0.0866
f_N_target_post = 0.0821
f_R_target_post = 0.0312

T = 24
shock_duration = 6

function matching_rates(θ, μ)
    f = μ * θ^(1 - α)
    q = μ * θ^(-α)
    return (f=min(f,1.0), q=min(q,1.0))
end

exogenous_wage = true  # Set to false to use Nash bargaining - currently exogenous.

function nash_wage(y, b̄)
    if exogenous_wage
        return 0.8
    else
        return φ * y + (1 - φ) * b̄
    end
end

function firm_surplus(y, w)
    return (y - w) / (1 - β * (1 - δ))
end

function solve_tightness(μ, y, b̄)
    w = nash_wage(y, b̄)
    J = firm_surplus(y, w)
    θ = ((β * μ * J) / c_vac)^(1 / α)
    return θ, w
end

function job_finding_rates(γ, μ, y, b_R)
    b̄ = p_R * b_R + (1 - p_R) * b_N + l_u
    θ, w = solve_tightness(μ, y, b̄)
    rates = matching_rates(θ, μ)
    p_contact = rates.f

    # Fixed-point iteration for recipients
    W_R, U_R = w, b_R + l_u
    for iter in 1:50
        W_R_new = w + β * ((1-δ)*W_R + δ*U_R)
        U_R_new = b_R + l_u + β * (p_contact * W_R + (1-p_contact) * U_R)
        @printf("Iter %d (R): W_R=%.6f, U_R=%.6f, Surplus (W_R-U_R)=%.6f\n",
                iter, W_R_new, U_R_new, W_R_new - U_R_new)
        if abs(W_R_new - W_R) < 1e-8 && abs(U_R_new - U_R) < 1e-8
            break
        end
        W_R, U_R = W_R_new, U_R_new
    end

    # Fixed-point iteration for non-recipients
    W_N, U_N = w, b_N + l_u
    for iter in 1:50
        W_N_new = w + β * ((1-δ)*W_N + δ*U_N)
        U_N_new = b_N + l_u + β * (p_contact * W_N + (1-p_contact) * U_N)
        @printf("Iter %d (N): W_N=%.6f, U_N=%.6f, Surplus (W_N-U_N)=%.6f\n",
                iter, W_N_new, U_N_new, W_N_new - U_N_new)
        if abs(W_N_new - W_N) < 1e-8 && abs(U_N_new - U_N) < 1e-8
            break
        end
        W_N, U_N = W_N_new, U_N_new
    end

    # Search efforts and job-finding rates
    s_R = β * p_contact * (W_R - U_R) / γ
    s_N = β * p_contact * (W_N - U_N) / γ
    f_R = s_R * p_contact
    f_N = s_N * p_contact

    return f_N, f_R, s_N, s_R, W_N, W_R, U_N, U_R, θ, w
end


function makie_surface(y, b_R, f_target, which=:f_N, label="")
    γ_vals = range(0.001, stop=1.0, length=50)
    μ_vals = range(0.05, stop=1.5, length=50)
    Z = zeros(length(γ_vals), length(μ_vals))
    for (i, γ) in enumerate(γ_vals), (j, μ) in enumerate(μ_vals)
        try
            f_N, f_R, _, _, _, _, _ = job_finding_rates(γ, μ, y, b_R)
            Z[i, j] = which == :f_N ? f_N : f_R
        catch
            Z[i, j] = NaN
        end
    end
    fig = Figure()
    ax = Axis3(fig[1, 1], title="$(which) surface — "*label, xlabel="μ", ylabel="γ", zlabel=String(which))
    CairoMakie.surface!(ax, μ_vals, γ_vals, Z, colormap=:viridis)
    display(fig)
end

makie_surface(y_pre, b_R_pre, f_N_target_pre, :f_N, "Pre-COVID")
makie_surface(y_pre, b_R_pre, f_R_target_pre, :f_R, "Pre-COVID")
makie_surface(y_post, b_R_post, f_N_target_post, :f_N, "Post-COVID")
makie_surface(y_post, b_R_post, f_R_target_post, :f_R, "Post-COVID")

# Grid search calibration
function calibrate_grid(y, b_R, f_N_target, f_R_target)
    γ_vals = range(0.001, stop=1.0, length=50)
    μ_vals = range(0.05, stop=1.5, length=50)
    residuals = fill(Inf, length(γ_vals), length(μ_vals))
    best_γ, best_μ, min_residual = NaN, NaN, Inf

    for (i, γ) in enumerate(γ_vals), (j, μ) in enumerate(μ_vals)
        try
            f_N, f_R, _, _, _, _, _, _, _ = job_finding_rates(γ, μ, y, b_R)
            res = sqrt((f_N - f_N_target)^2 + (f_R - f_R_target)^2)
            residuals[i, j] = res
            if res < min_residual
                min_residual = res
                best_γ, best_μ = γ, μ
            end
        catch
            residuals[i, j] = NaN
        end
    end

    println("✅ Best fit: γ=$(best_γ), μ=$(best_μ), residual norm=$(min_residual)")
    return best_γ, best_μ, residuals, γ_vals, μ_vals
end

# Residual surface plots
function plot_residuals(residuals, γ_vals, μ_vals, label)
    Plots.heatmap(μ_vals, γ_vals, residuals, title="Residual norm ||F|| — "*label, xlabel="μ", ylabel="γ", colorbar_title="||F||")
    display(current())
end

function makie_surface(residuals, γ_vals, μ_vals, label)
    fig = Figure()
    ax = Axis3(fig[1,1], title="Residual norm ||F|| — "*label, xlabel="μ", ylabel="γ", zlabel="||F||")
    CairoMakie.surface!(ax, μ_vals, γ_vals, residuals, colormap=:viridis)
    display(fig)
end

# Calibrate pre/post
println("\nCalibrating pre-COVID (grid search)...")
γ_pre, μ_pre, res_pre, γ_grid, μ_grid = calibrate_grid(y_pre, b_R_pre, f_N_target_pre, f_R_target_pre)

println("\nCalibrating post-COVID (grid search)...")
γ_post, μ_post, res_post, _, _ = calibrate_grid(y_post, b_R_post, f_N_target_post, f_R_target_post)

if force_surface_plot
    plot_residuals(res_pre, γ_grid, μ_grid, "Pre-COVID")
    plot_residuals(res_post, γ_grid, μ_grid, "Post-COVID")
    makie_surface(res_pre, γ_grid, μ_grid, "Pre-COVID")
    makie_surface(res_post, γ_grid, μ_grid, "Post-COVID")
end

# Compare Steady States for Pre-COVID, Post-COVID, and Benefit-Only Scenarios
function compare_steady_states()
    println("\n📌 Steady State Comparison Table:")

    # Pre-COVID full calibration
    f_N_pre, f_R_pre, s_N_pre, s_R_pre, W_N_pre, W_R_pre, U_N_pre, U_R_pre, θ_pre, w_pre =
        job_finding_rates(γ_pre, μ_pre, y_pre, b_R_pre)

    # Post-COVID full calibration
    f_N_post, f_R_post, s_N_post, s_R_post, W_N_post, W_R_post, U_N_post, U_R_post, θ_post, w_post =
        job_finding_rates(γ_post, μ_post, y_post, b_R_post)

    # Benefit-only shock: keep γ, μ at pre-COVID levels
    f_N_benefit, f_R_benefit, s_N_benefit, s_R_benefit, W_N_benefit, W_R_benefit, U_N_benefit, U_R_benefit, θ_benefit, w_benefit =
        job_finding_rates(γ_pre, μ_pre, y_post, b_R_post)

    # Print formatted table
    println("| Metric    | Pre-COVID | Post-COVID | Benefit Only |")
    println("|-----------|------------|------------|--------------|")
    @printf("| f_N       | %.4f     | %.4f     | %.4f        |\n", f_N_pre, f_N_post, f_N_benefit)
    @printf("| f_R       | %.4f     | %.4f     | %.4f        |\n", f_R_pre, f_R_post, f_R_benefit)
    @printf("| s_N       | %.4f     | %.4f     | %.4f        |\n", s_N_pre, s_N_post, s_N_benefit)
    @printf("| s_R       | %.4f     | %.4f     | %.4f        |\n", s_R_pre, s_R_post, s_R_benefit)
    @printf("| W_N       | %.4f     | %.4f     | %.4f        |\n", W_N_pre, W_N_post, W_N_benefit)
    @printf("| W_R       | %.4f     | %.4f     | %.4f        |\n", W_R_pre, W_R_post, W_R_benefit)
    @printf("| U_N       | %.4f     | %.4f     | %.4f        |\n", U_N_pre, U_N_post, U_N_benefit)
    @printf("| U_R       | %.4f     | %.4f     | %.4f        |\n", U_R_pre, U_R_post, U_R_benefit)
    @printf("| θ         | %.4f     | %.4f     | %.4f        |\n", θ_pre, θ_post, θ_benefit)
    @printf("| w         | %.4f     | %.4f     | %.4f        |\n", w_pre, w_post, w_benefit)
    println()
end