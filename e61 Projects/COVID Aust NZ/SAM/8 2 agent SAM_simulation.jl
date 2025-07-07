
# 8 2 agent SAM_simulation.jl
# Runs simulations using functions from '8 2 agent SAM.jl'
# Note, this is a myopic agent, but it shows behaviour. Next script is about building a dynamic version

include("8 2 agent SAM.jl")

println("✅ Starting simulation with pre-COVID steady state as initial condition")

# --- Compute Steady States ---
println("\n📌 Steady States:")
f_N_pre, f_R_pre, s_N_pre, s_R_pre, W_N_pre, W_R_pre, U_N_pre, U_R_pre, _, _ = job_finding_rates(γ_pre, μ_pre, y_pre, b_R_pre)
f_N_post, f_R_post, s_N_post, s_R_post, W_N_post, W_R_post, U_N_post, U_R_post, _, _ = job_finding_rates(γ_post, μ_post, y_post, b_R_post)

println("| State         | f_N  | f_R  | s_N  | s_R  | W_N  | W_R  | U_N  | U_R  |")
println("|---------------|------|------|------|------|------|------|------|------|")
@printf("| Pre-COVID     | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f |\n",
    f_N_pre, f_R_pre, s_N_pre, s_R_pre, W_N_pre, W_R_pre, U_N_pre, U_R_pre)
@printf("| Post-COVID    | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f |\n",
    f_N_post, f_R_post, s_N_post, s_R_post, W_N_post, W_R_post, U_N_post, U_R_post)

# --- Simulation Function ---
function run_simulation(full_shock::Bool)
    u_N, u_R, E = zeros(T+1), zeros(T+1), zeros(T+1)
    s_N, s_R, f_N, f_R, WU_N, WU_R, w_series, b̄_series = zeros(T+1), zeros(T+1), zeros(T+1), zeros(T+1), zeros(T+1), zeros(T+1), zeros(T+1), zeros(T+1)
    u_N[1], u_R[1], E[1] = 0.05, 0.02, 1.0 - 0.05 - 0.02

    for t in 1:T
        if 7 <= t <= 13
            γ, μ, y, b_R = full_shock ? (γ_post, μ_post, y_post, b_R_post) : (γ_pre, μ_pre, y_post, b_R_post)
        else
            γ, μ, y, b_R = γ_pre, μ_pre, y_pre, b_R_pre
        end

        f_N_t, f_R_t, s_N_t, s_R_t, W_N_t, W_R_t, U_N_t, U_R_t, θ, w = job_finding_rates(γ, μ, y, b_R)

        f_N[t] = min(f_N_t, 0.999)
        f_R[t] = min(f_R_t, 0.999)
        s_N[t], s_R[t] = s_N_t, s_R_t
        WU_N[t], WU_R[t] = W_N_t - U_N_t, W_R_t - U_R_t
        w_series[t], b̄_series[t] = w, p_R * b_R + (1 - p_R) * b_N + l_u

        sep = δ * E[t]
        hires_N, hires_R = f_N[t] * u_N[t], f_R[t] * u_R[t]
        u_N[t+1] = max(u_N[t] + (1-p_R)*sep - hires_N, 0.0)
        u_R[t+1] = max(u_R[t] + p_R*sep - hires_R, 0.0)
        E[t+1] = 1.0 - u_N[t+1] - u_R[t+1]
    end
    return (u_N=u_N, u_R=u_R, f_N=f_N, f_R=f_R, s_N=s_N, s_R=s_R, 
        WU_N=WU_N, WU_R=WU_R, w_series=w_series, b̄_series=b̄_series)
end

# --- Run Simulations ---
results_full = run_simulation(true)
results_benefit_only = run_simulation(false)

# --- Plot Results ---
Plots.plot(0:T, results_full.u_N .* 100, label="u_N Full Shock", lw=2)
Plots.plot!(0:T, results_benefit_only.u_N .* 100, label="u_N Benefit Only", ls=:dash)
Plots.plot(0:T, results_full.u_N .* 100, label="u_R Full Shock", lw=2)
Plots.plot!(0:T, results_benefit_only.u_N .* 100, label="u_R Benefit Only", ls=:dash)
Plots.xlabel!("Month"); Plots.ylabel!("Unemployment Rate (%)")
Plots.title!("Unemployment: Full Shock vs Benefit Only")
display(current())

Plots.plot(0:T, results_full[3] .* 100, label="f_N Full Shock", lw=2)
Plots.plot!(0:T, results_benefit_only[3] .* 100, label="f_N Benefit Only", ls=:dash)
Plots.plot!(0:T, results_full[4] .* 100, label="f_R Full Shock", lw=2)
Plots.plot!(0:T, results_benefit_only[4] .* 100, label="f_R Benefit Only", ls=:dash)
Plots.xlabel!("Month"); Plots.ylabel!("Job-Finding Rate (%)")
Plots.title!("Job-Finding Rates: Full Shock vs Benefit Only")
display(current())

Plots.plot(0:T, results_full[7], label="WU_N Full Shock", lw=2)
Plots.plot!(0:T, results_benefit_only[7], label="WU_N Benefit Only", ls=:dash)
Plots.plot!(0:T, results_full[8], label="WU_R Full Shock", lw=2)
Plots.plot!(0:T, results_benefit_only[8], label="WU_R Benefit Only", ls=:dash)
Plots.xlabel!("Month"); Plots.ylabel!("Surplus (W - U)")
Plots.title!("Surplus (W - U): Full Shock vs Benefit Only")
display(current())

Plots.plot(0:T, results_full[9], label="Wage Full Shock", lw=2)
Plots.plot!(0:T, results_benefit_only[9], label="Wage Benefit Only", ls=:dash)
Plots.xlabel!("Month"); Plots.ylabel!("Wage Level")
Plots.title!("Wage Path: Full Shock vs Benefit Only")
display(current())


compare_steady_states()