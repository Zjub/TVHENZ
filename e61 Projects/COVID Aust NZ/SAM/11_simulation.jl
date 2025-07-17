# 11_simulation.jl
# Forward simulation with variable leisure, dynamic calibration, and extended outputs - also include appropriate transition dynamics post shock (steady state prior to shock)

using Plots

include("11_Dynamic_newcalibration.jl")  # Make sure to update the path

println("\n🔧 Calibrating parameters...")
γ_pre, μ_pre, γ_post, μ_post, γ_vals, μ_vals =
    calibrate_grid(y_pre, b_R_pre, y_post, b_R_post,
                   f_N_target_pre, f_R_target_pre,
                   f_N_target_post, f_R_target_post,
                   l_u_R_pre, l_u_N_pre,
                   l_u_R_post, l_u_N_post)
println("✅ Calibration complete: γ_pre=$(γ_pre), μ_pre=$(μ_pre), γ_post=$(γ_post), μ_post=$(μ_post)")


println("\n🚀 Computing dynamic value paths...")
value_path = compute_value_path(
    γ_pre, μ_pre, γ_post, μ_post,
    y_pre, y_post, b_R_pre, b_R_post,
    l_u_R_pre, l_u_R_post, l_u_N_pre, l_u_N_post
)
value_path_benefit_only = compute_value_path(
    γ_pre, μ_pre, γ_pre, μ_pre, # keep labor market params constant
    y_pre, y_pre,             # y stays the same
    b_R_pre, b_R_post,         # benefit changes during shock
    l_u_R_pre, l_u_R_pre,     # leisure stays the same
    l_u_N_pre, l_u_N_pre
)

# Forward simulation arrays
u_N, u_R, E = zeros(T+1), zeros(T+1), zeros(T+1)
s_N, s_R, f_N, f_R = zeros(T), zeros(T), zeros(T), zeros(T)
W_U_N, W_U_R = zeros(T), zeros(T)  # Surpluses W - U
u_N[1], u_R[1], E[1] = 0.05, 0.02, 1.0 - 0.05 - 0.02

for t in 1:T
    V_t = value_path[t]
    if shock_start <= t <= shock_end
        γ_t, μ_t, y_t, b_R_t = γ_post, μ_post, y_post, b_R_post
        l_u_R_t, l_u_N_t = l_u_R_post, l_u_N_post
    elseif shock_end > T
        γ_t, μ_t, y_t, b_R_t = γ_pre, μ_pre, y_pre, b_R_pre
        l_u_R_t, l_u_N_t = l_u_R_pre, l_u_N_pre
    else # The pre-period
        γ_t, μ_t, y_t, b_R_t = γ_pre, μ_pre, y_pre, b_R_pre
        l_u_R_t, l_u_N_t = l_u_R_pre, l_u_N_pre
    end

    b̄ = p_R*b_R_t + (1-p_R)*b_N + (p_R*l_u_R_t + (1-p_R)*l_u_N_t)
    θ, w_t = solve_tightness(μ_t, y_t, b̄)
    p_contact = matching_rates(θ, μ_t).f

    s_N[t] = β * p_contact * (V_t.W_N - V_t.U_N) / γ_t
    s_R[t] = β * p_contact * (V_t.W_R - V_t.U_R) / γ_t
    f_N[t] = min(s_N[t] * p_contact, 0.999)
    f_R[t] = min(s_R[t] * p_contact, 0.999)

    # Surpluses (W - U)
    W_U_N[t] = V_t.W_N - V_t.U_N
    W_U_R[t] = V_t.W_R - V_t.U_R

    sep = δ * E[t]
    hires_N, hires_R = f_N[t]*u_N[t], f_R[t]*u_R[t]
    u_N[t+1] = max(u_N[t] + (1-p_R)*sep - hires_N, 0.0)
    u_R[t+1] = max(u_R[t] + p_R*sep - hires_R, 0.0)
    E[t+1] = 1.0 - u_N[t+1] - u_R[t+1]
end

# Forward simulation for benefit-only scenario (similarly extended)
u_N_benefit_only, u_R_benefit_only, E_benefit_only = zeros(T+1), zeros(T+1), zeros(T+1)
s_N_benefit_only, s_R_benefit_only, f_N_benefit_only, f_R_benefit_only = zeros(T), zeros(T), zeros(T), zeros(T)
W_U_N_benefit_only, W_U_R_benefit_only = zeros(T), zeros(T)
u_N_benefit_only[1], u_R_benefit_only[1], E_benefit_only[1] = 0.05, 0.02, 1.0 - 0.05 - 0.02

for t in 1:T
    V_t = value_path_benefit_only[t]
    γ_t, μ_t = γ_pre, μ_pre

    if shock_start <= t <= shock_end
        y_t, b_R_t, l_u_R_t, l_u_N_t = y_post, b_R_post, l_u_R_post, l_u_N_post
    else
        y_t, b_R_t, l_u_R_t, l_u_N_t = y_pre, b_R_pre, l_u_R_pre, l_u_N_pre
    end

    b̄ = p_R*b_R_t + (1-p_R)*b_N + (p_R*l_u_R_t + (1-p_R)*l_u_N_t)
    θ, w_t = solve_tightness(μ_t, y_t, b̄)
    p_contact = matching_rates(θ, μ_t).f

    s_N_benefit_only[t] = β * p_contact * (V_t.W_N - V_t.U_N) / γ_t
    s_R_benefit_only[t] = β * p_contact * (V_t.W_R - V_t.U_R) / γ_t
    f_N_benefit_only[t] = min(s_N_benefit_only[t] * p_contact, 0.999)
    f_R_benefit_only[t] = min(s_R_benefit_only[t] * p_contact, 0.999)

    # Surpluses (W - U)
    W_U_N_benefit_only[t] = V_t.W_N - V_t.U_N
    W_U_R_benefit_only[t] = V_t.W_R - V_t.U_R

    sep = δ * E_benefit_only[t]
    hires_N, hires_R = f_N_benefit_only[t]*u_N_benefit_only[t], f_R_benefit_only[t]*u_R_benefit_only[t]
    u_N_benefit_only[t+1] = max(u_N_benefit_only[t] + (1-p_R)*sep - hires_N, 0.0)
    u_R_benefit_only[t+1] = max(u_R_benefit_only[t] + p_R*sep - hires_R, 0.0)
    E_benefit_only[t+1] = 1.0 - u_N_benefit_only[t+1] - u_R_benefit_only[t+1]
end

# 📈 Plot 1: Unemployment and Job Finding Rates
plt1 = plot(0:T, u_N .* 100, label="u_N Full Shock", lw=2)
plot!(plt1, 0:T, u_N_benefit_only .* 100, label="u_N Benefit Only", ls=:dash)
plot!(plt1, 0:T, u_R .* 100, label="u_R Full Shock", lw=2)
plot!(plt1, 0:T, u_R_benefit_only .* 100, label="u_R Benefit Only", ls=:dash)
xlabel!("Week"); ylabel!("Unemployment Rate (%)")
title!("Unemployment Dynamics: Full Shock vs Benefit Only")
display(plt1)
savefig(plt1, "unemployment_dynamics.png")

# 📈 Plot 2: Job-Finding Rates
plt2 = plot(1:T, f_N .* 100, label="f_N Full Shock", lw=2)
plot!(plt2, 1:T, f_N_benefit_only .* 100, label="f_N Benefit Only", ls=:dash)
plot!(plt2, 1:T, f_R .* 100, label="f_R Full Shock", lw=2)
plot!(plt2, 1:T, f_R_benefit_only .* 100, label="f_R Benefit Only", ls=:dash)
xlabel!("Week"); ylabel!("Job-Finding Rate (%)")
title!("Job-Finding Rates: Full Shock vs Benefit Only")
display(plt2)
savefig(plt2, "job_finding_rates.png")

# 📈 Plot 3: Search Efforts
plt3 = plot(1:T, s_N, label="s_N Full Shock", lw=2)
plot!(plt3, 1:T, s_N_benefit_only, label="s_N Benefit Only", ls=:dash)
plot!(plt3, 1:T, s_R, label="s_R Full Shock", lw=2)
plot!(plt3, 1:T, s_R_benefit_only, label="s_R Benefit Only", ls=:dash)
xlabel!("Week"); ylabel!("Search Effort")
title!("Search Effort: Full Shock vs Benefit Only")
display(plt3)
savefig(plt3, "search_effort.png")

# 📈 Plot 4: Difference in Search Effort (s_R - s_N)
Δs_full = s_R .- s_N
Δs_benefit_only = s_R_benefit_only .- s_N_benefit_only

plt4 = plot(1:T, Δs_full, label="Full Shock", lw=2, color=:blue)
plot!(plt4, 1:T, Δs_benefit_only, label="Benefit Only", ls=:dash, lw=2, color=:red)
xlabel!("Week"); ylabel!("Difference in Search Effort")
title!("Difference in Search Effort (s_R - s_N)")
display(plt4)
savefig(plt4, "search_effort_difference.png")

# 📈 Plot 5: Difference in Job-Finding Rates (f_R - f_N)
Δf_full = f_R .- f_N
Δf_benefit_only = f_R_benefit_only .- f_N_benefit_only

plt5 = plot(1:T, Δf_full .* 100, label="Full Shock", lw=2, color=:blue)
plot!(plt5, 1:T, Δf_benefit_only .* 100, label="Benefit Only", ls=:dash, lw=2, color=:red)
xlabel!("Week"); ylabel!("Difference in Job-Finding Rate (p.p.)")
title!("Difference in Job-Finding Rates (f_R - f_N)")
display(plt5)
savefig(plt5, "job_finding_difference.png")

# 📈 Plot 6: Difference-in-Differences for Search Effort
Δs_DiD_full = Δs_full .- Δs_full[1]  # Subtract initial difference
Δs_DiD_benefit_only = Δs_benefit_only .- Δs_benefit_only[1]

plt6 = plot(1:T, Δs_DiD_full, label="Full Shock", lw=2, color=:blue)
plot!(plt6, 1:T, Δs_DiD_benefit_only, label="Benefit Only", ls=:dash, lw=2, color=:red)
xlabel!("Month")
ylabel!("Difference-in-Differences (s_R - s_N)")
title!("Difference-in-Differences: Search Effort")
display(plt6)
savefig(plt6, "did_search_effort.png")

# 📈 Plot 7: Difference-in-Differences for Job-Finding Rates
Δf_DiD_full = Δf_full .- Δf_full[1]
Δf_DiD_benefit_only = Δf_benefit_only .- Δf_benefit_only[1]

plt7 = plot(1:T, Δf_DiD_full .* 100, label="Full Shock", lw=2, color=:blue)
plot!(plt7, 1:T, Δf_DiD_benefit_only .* 100, label="Benefit Only", ls=:dash, lw=2, color=:red)
xlabel!("Month")
ylabel!("Difference-in-Differences (p.p.)")
title!("Difference-in-Differences: Job-Finding Rates")
display(plt7)
savefig(plt7, "did_job_finding_rates.png")


# 📊 Table: Average Job-Finding Rates
average_job_finding_rates(f_N, f_R)
average_job_finding_rates(f_N_benefit_only, f_R_benefit_only)

# 📊 Table: Average W - U Surpluses
function average_surpluses(WU_N, WU_R)
    println("\n📊 Average W-U Surpluses:")
    println("| Period     | W-U_N  | W-U_R  |")
    println("|------------|--------|--------|")
    @printf("| Pre-shock  | %6.4f | %6.4f |\n", mean(WU_N[1:shock_start-1]), mean(WU_R[1:shock_start-1]))
    @printf("| Shock      | %6.4f | %6.4f |\n", mean(WU_N[shock_start:shock_end]), mean(WU_R[shock_start:shock_end]))
    @printf("| Post-shock | %6.4f | %6.4f |\n", mean(WU_N[shock_end+1:end]), mean(WU_R[shock_end+1:end]))
end

average_surpluses(W_U_N, W_U_R)
average_surpluses(W_U_N_benefit_only, W_U_R_benefit_only)

# 📌 Steady States
compare_steady_states()

using DataFrames, CSV

# Create DataFrame
did_job_finding_df = DataFrame(
    Month = 1:T,
    DiD_Full_Shock = Δf_DiD_full .* 100,          # Convert to percentage points
    DiD_Benefit_Only = Δf_DiD_benefit_only .* 100
)

# Save as CSV
CSV.write("did_job_finding_rates.csv", did_job_finding_df)