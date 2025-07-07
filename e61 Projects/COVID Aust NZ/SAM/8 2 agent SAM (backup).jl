
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
δ    = 0.03
p_R  = 0.5
α    = 0.5
φ    = 0.5
y_pre = 1.0
y_post = 0.8
b_R_pre = 0.365
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

# Simulation
println("\n✅ Running simulation...")
b_R_full = [t <= shock_duration ? b_R_post : b_R_pre for t in 1:T+1]
γ_full, μ_full, y_full = [t <= shock_duration ? γ_post : γ_pre for t in 1:T+1], [t <= shock_duration ? μ_post : μ_pre for t in 1:T+1], [t <= shock_duration ? y_post : y_pre for t in 1:T+1]

s_N, s_R, f_N, f_R = zeros(T+1), zeros(T+1), zeros(T+1), zeros(T+1)
u_N, u_R, E = zeros(T+1), zeros(T+1), zeros(T+1)
u_R[1], u_N[1], E[1] = 0.02, 0.05, 1.0 - 0.02 - 0.05
WU_N, WU_R = zeros(T+1), zeros(T+1)
w_series = zeros(T+1)
b̄_series = zeros(T+1)


for t in 1:T
    b̄ = p_R * b_R_full[t] + (1 - p_R) * b_N + l_u
    θ, w = solve_tightness(μ_full[t], y_full[t], b̄)
    rates = matching_rates(θ, μ_full[t])
    p_contact = rates.f
    #Δ_N, Δ_R = y_full[t] - w, y_full[t] - w
    #s_N[t] = β * p_contact * Δ_N / γ_full[t]
    #s_R[t] = β * p_contact * Δ_R / γ_full[t]
    
        # Call the function to get all relevant values for the period
    f_N_temp, f_R_temp, s_N_temp, s_R_temp, W_N, W_R, U_N, U_R, θ, w = job_finding_rates(γ_full[t], μ_full[t], y_full[t], b_R_full[t])

    # Directly assign the correct, calculated values to the storage arrays
    s_N[t] = s_N_temp
    s_R[t] = s_R_temp
    f_N[t] = min(f_N_temp, 0.999) 
    f_R[t] = min(f_R_temp, 0.999) 

    # Flows: separations before hires
    sep = δ * E[t]
    hires_N, hires_R = f_N[t] * u_N[t], f_R[t] * u_R[t]
    u_N[t+1] = u_N[t] + (1-p_R)*sep - hires_N
    u_R[t+1] = u_R[t] + p_R*sep - hires_R
    u_N[t+1] = max(u_N[t+1], 0.0)
    u_R[t+1] = max(u_R[t+1], 0.0)
    E[t+1] = 1.0 - u_N[t+1] - u_R[t+1]
    w_series[t] = w
    b̄_series[t] = b̄

    # Store the worker surplus (W-U) for plotting
    WU_N[t] = W_N - U_N
    WU_R[t] = W_R - U_R

    w_series[t] = w
    @printf("Month %d: w=%.4f, ȳ=%.4f, y=%.4f\n", t, w, b̄, y_full[t])

    @printf("Month %d: δE=%.4f, hires_N=%.4f, hires_R=%.4f, u_N=%.4f, u_R=%.4f\n",
            t, sep, hires_N, hires_R, u_N[t+1], u_R[t+1])
end

println("\nMonth | f_N(%) | f_R(%) | s_N | s_R | u_N(%) | u_R(%)")
for t in 1:T+1
    @printf("%5d | %6.2f | %6.2f | %.3f | %.3f | %6.2f | %6.2f\n",
            t-1, 100*f_N[t], 100*f_R[t], s_N[t], s_R[t],
            100*u_N[t], 100*u_R[t])
end

# Dynamic plots
Plots.plot(0:T, u_N .* 100, label="u_N (%)", lw=2)
Plots.plot!(0:T, u_R .* 100, label="u_R (%)", lw=2)
Plots.xlabel!("Month"); Plots.ylabel!("Unemployment Rate (%)")
Plots.title!("Unemployment Dynamics")
display(current())

Plots.plot(0:T, f_N .* 100, label="f_N (%)", lw=2)
Plots.plot!(0:T, f_R .* 100, label="f_R (%)", lw=2)
Plots.xlabel!("Month"); Plots.ylabel!("Job-Finding Rate (%)")
Plots.title!("Job-Finding Rates Dynamics")
display(current())


# Plot W - U_N and W - U_R over time
Plots.plot(0:T, WU_N, label="W - U_N", lw=2)
Plots.plot!(0:T, WU_R, label="W - U_R", lw=2, ls=:dash)
Plots.xlabel!("Month"); Plots.ylabel!("Value Difference")
Plots.title!("Difference between Employed and Unemployed Values")
display(current())

Plots.plot(0:T, w_series, label="Wage (w)", lw=2)
Plots.plot!(0:T, b̄_series, label="Average Benefit (ȳ)", lw=2, ls=:dash)
Plots.plot!(0:T, y_full, label="Productivity (y)", lw=2, ls=:dot)
Plots.xlabel!("Month"); Plots.ylabel!("Level")
Plots.title!("Wage, Average Benefit, and Productivity over Time")
display(current())

# Optional: plot wage gap
Plots.plot(0:T, w_series .- b̄_series, label="w - ȳ", lw=2)
Plots.xlabel!("Month"); Plots.ylabel!("Gap")
Plots.title!("Gap between Wage and Average Benefit")
display(current())

Plots.plot(0:T, WU_R, label="Surplus W_R - U_R", lw=2)
Plots.plot!(0:T, WU_N, label="Surplus W_N - U_N", lw=2, ls=:dash)
Plots.xlabel!("Month"); Plots.ylabel!("Surplus (W - U)")
Plots.title!("Surplus Difference for Recipients and Non-Recipients")
display(current())