
# Includes dynamic model and runs forward simulation
# Here we have included anticipation and added dynamic paths
# No Nash Bargaining, which removes interaction between agents through wages - reintroduce in next version

include("9 Dynamic 2 agent.jl")

println("\n🚀 Computing dynamic value path...")
value_path = compute_value_path()
value_path_benefit_only = compute_value_path_benefit_only()

## Values if everything is fully anticipated
#value_path = compute_value_path_benefit_only_anticipated(γ_pre, μ_pre, γ_post, μ_post)
#value_path_benefit_only = compute_value_path_benefit_only_anticipated()

u_N, u_R, E = zeros(T+1), zeros(T+1), zeros(T+1)
s_N, s_R, f_N, f_R = zeros(T+1), zeros(T+1), zeros(T+1), zeros(T+1)
u_N[1], u_R[1], E[1] = 0.05, 0.02, 1.0 - 0.05 - 0.02

for t in 1:T
    V_t = value_path[t]
    if shock_start <= t <= shock_end
        γ_t, μ_t, y_t, b_R_t = γ_post, μ_post, y_post, b_R_post
    else
        γ_t, μ_t, y_t, b_R_t = γ_pre, μ_pre, y_pre, b_R_pre
    end

    b̄ = p_R * b_R_t + (1 - p_R)*b_N + l_u
    θ, w_t = solve_tightness(μ_t, y_t, b̄)
    p_contact = matching_rates(θ, μ_t).f

    s_N[t] = β * p_contact * (V_t.W_N - V_t.U_N) / γ_t
    s_R[t] = β * p_contact * (V_t.W_R - V_t.U_R) / γ_t
    f_N[t] = min(s_N[t] * p_contact, 0.999)
    f_R[t] = min(s_R[t] * p_contact, 0.999)

    sep = δ * E[t]
    hires_N, hires_R = f_N[t]*u_N[t], f_R[t]*u_R[t]
    u_N[t+1] = max(u_N[t] + (1-p_R)*sep - hires_N, 0.0)
    u_R[t+1] = max(u_R[t] + p_R*sep - hires_R, 0.0)
    E[t+1] = 1.0 - u_N[t+1] - u_R[t+1]
end

# Forward simulation for benefit-only shock
u_N_benefit_only, u_R_benefit_only, E_benefit_only = zeros(T+1), zeros(T+1), zeros(T+1)
s_N_benefit_only, s_R_benefit_only, f_N_benefit_only, f_R_benefit_only = zeros(T+1), zeros(T+1), zeros(T+1), zeros(T+1)
u_N_benefit_only[1], u_R_benefit_only[1], E_benefit_only[1] = 0.05, 0.02, 1.0 - 0.05 - 0.02

for t in 1:T
    V_t = value_path_benefit_only[t]
    # γ and μ are fixed pre-COVID values
    γ_t, μ_t = γ_pre, μ_pre

    if shock_start <= t <= shock_end
        y_t, b_R_t = y_post, b_R_post
    else
        y_t, b_R_t = y_pre, b_R_pre
    end

    b̄ = p_R * b_R_t + (1 - p_R)*b_N + l_u
    θ, w_t = solve_tightness(μ_t, y_t, b̄)
    p_contact = matching_rates(θ, μ_t).f

    s_N_benefit_only[t] = β * p_contact * (V_t.W_N - V_t.U_N) / γ_t
    s_R_benefit_only[t] = β * p_contact * (V_t.W_R - V_t.U_R) / γ_t
    f_N_benefit_only[t] = min(s_N_benefit_only[t] * p_contact, 0.999)
    f_R_benefit_only[t] = min(s_R_benefit_only[t] * p_contact, 0.999)

    sep = δ * E_benefit_only[t]
    hires_N, hires_R = f_N_benefit_only[t]*u_N_benefit_only[t], f_R_benefit_only[t]*u_R_benefit_only[t]
    u_N_benefit_only[t+1] = max(u_N_benefit_only[t] + (1-p_R)*sep - hires_N, 0.0)
    u_R_benefit_only[t+1] = max(u_R_benefit_only[t] + p_R*sep - hires_R, 0.0)
    E_benefit_only[t+1] = 1.0 - u_N_benefit_only[t+1] - u_R_benefit_only[t+1]
end

# Plot results
Plots.plot(0:T, u_N .* 100, label="u_N (%)", lw=2)
Plots.plot!(0:T, u_R .* 100, label="u_R (%)", lw=2)
Plots.title!("Unemployment Dynamics")
Plots.xlabel!("Month"); Plots.ylabel!("Unemployment Rate (%)")
display(current())

Plots.plot(0:T, f_N .* 100, label="f_N (%)", lw=2)
Plots.plot!(0:T, f_R .* 100, label="f_R (%)", lw=2)
Plots.title!("Job-Finding Rates")
Plots.xlabel!("Month"); Plots.ylabel!("Job-Finding Rate (%)")
display(current())


Plots.plot(0:T, u_N .* 100, label="u_N Full Shock", lw=2)
Plots.plot!(0:T, u_N_benefit_only .* 100, label="u_N Benefit Only", ls=:dash)
Plots.plot!(0:T, u_R .* 100, label="u_R Full Shock", lw=2)
Plots.plot!(0:T, u_R_benefit_only .* 100, label="u_R Benefit Only", ls=:dash)
Plots.xlabel!("Month"); Plots.ylabel!("Unemployment Rate (%)")
Plots.title!("Unemployment: Full Shock vs Benefit Only")
display(current())

Plots.plot(0:T, f_N .* 100, label="f_N Full Shock", lw=2)
Plots.plot!(0:T, f_N_benefit_only .* 100, label="f_N Benefit Only", ls=:dash)
Plots.plot!(0:T, f_R .* 100, label="f_R Full Shock", lw=2)
Plots.plot!(0:T, f_R_benefit_only .* 100, label="f_R Benefit Only", ls=:dash)
Plots.xlabel!("Month"); Plots.ylabel!("Job-Finding Rate (%)")
Plots.title!("JFR: Full Shock vs Benefit Only")
display(current())

compare_steady_states()