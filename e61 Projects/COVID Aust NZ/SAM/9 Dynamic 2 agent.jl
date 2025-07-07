# search_matching_model_dual_agents_dynamic.jl
# Full model: backward induction + forward simulation setup

using Printf, Plots, CairoMakie

# Parameters
β    = 0.99
δ    = 0.02
p_R  = 0.5
α    = 0.5
φ    = 0.5
y_pre = 1.0
y_post = 1.0
b_R_pre = 0.28
b_R_post = 2.0 * b_R_pre
b_N = 0.0
l_u = 0.1
c_vac = 1

f_N_target_pre = 0.10
f_R_target_pre = 0.0866
f_N_target_post = 0.0821
f_R_target_post = 0.0312

T = 30
shock_start, shock_end = 7, 13

# Struct for storing W/U paths
struct ValueTuple
    W_R::Float64
    U_R::Float64
    W_N::Float64
    U_N::Float64
end

# Match function and tightness
function matching_rates(θ, μ)
    f = μ * θ^(1 - α)
    q = μ * θ^(-α)
    return (f=min(f,1.0), q=min(q,1.0))
end

exogenous_wage = true # Set to true to turn off Nash Bargaining. NB not incorporated in optimisation of below (Bellmans need to be solved simultaneously with that)
function nash_wage(y, b̄)
    return exogenous_wage ? 0.8 : φ * y + (1 - φ) * b̄
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

# Backward induction to build value paths
# Core version has unanticipated changes as of the shock, but then anticipation of future reversal
function compute_value_path()
    value_path = Vector{ValueTuple}(undef, T+1)

    # Terminal condition: pre-COVID steady state
    _, _, _, _, W_N_ss, W_R_ss, U_N_ss, U_R_ss, _, _ =
        job_finding_rates(γ_pre, μ_pre, y_pre, b_R_pre)
    value_path[T+1] = ValueTuple(W_R_ss, U_R_ss, W_N_ss, U_N_ss)

    # Post-shock (t = T down to shock_end+1): agents anticipate pre-COVID parameters
    for t in T:-1:(shock_end+1)
        V_next = value_path[t+1]
        γ_t, μ_t, y_t, b_R_t = γ_pre, μ_pre, y_pre, b_R_pre

        b̄ = p_R * b_R_t + (1 - p_R)*b_N + l_u
        θ, w_t = solve_tightness(μ_t, y_t, b̄)
        p_contact = matching_rates(θ, μ_t).f

        W_R_t = w_t + β*((1-δ)*V_next.W_R + δ*V_next.U_R)
        U_R_t = b_R_t + l_u + β*(p_contact*V_next.W_R + (1-p_contact)*V_next.U_R)

        W_N_t = w_t + β*((1-δ)*V_next.W_N + δ*V_next.U_N)
        U_N_t = b_N + l_u + β*(p_contact*V_next.W_N + (1-p_contact)*V_next.U_N)

        value_path[t] = ValueTuple(W_R_t, U_R_t, W_N_t, U_N_t)
    end

    # Shock period (t = shock_end down to shock_start+1): agents anticipate reversal
    for t in shock_end:-1:(shock_start+1)
        V_next = value_path[t+1]
        γ_t, μ_t, y_t, b_R_t = γ_post, μ_post, y_post, b_R_post

        b̄ = p_R * b_R_t + (1 - p_R)*b_N + l_u
        θ, w_t = solve_tightness(μ_t, y_t, b̄)
        p_contact = matching_rates(θ, μ_t).f

        W_R_t = w_t + β*((1-δ)*V_next.W_R + δ*V_next.U_R)
        U_R_t = b_R_t + l_u + β*(p_contact*V_next.W_R + (1-p_contact)*V_next.U_R)

        W_N_t = w_t + β*((1-δ)*V_next.W_N + δ*V_next.U_N)
        U_N_t = b_N + l_u + β*(p_contact*V_next.W_N + (1-p_contact)*V_next.U_N)

        value_path[t] = ValueTuple(W_R_t, U_R_t, W_N_t, U_N_t)
    end

    # At shock_start (agents did NOT anticipate shock)
    V_next = value_path[shock_start+1]
    γ_t, μ_t, y_t, b_R_t = γ_post, μ_post, y_post, b_R_post

    b̄ = p_R * b_R_t + (1 - p_R)*b_N + l_u
    θ, w_t = solve_tightness(μ_t, y_t, b̄)
    p_contact = matching_rates(θ, μ_t).f

    # Myopic update using pre-shock steady state continuation values
    W_R_t = w_t + β*((1-δ)*W_R_ss + δ*U_R_ss)
    U_R_t = b_R_t + l_u + β*(p_contact*W_R_ss + (1-p_contact)*U_R_ss)

    W_N_t = w_t + β*((1-δ)*W_N_ss + δ*U_N_ss)
    U_N_t = b_N + l_u + β*(p_contact*W_N_ss + (1-p_contact)*U_N_ss)

    value_path[shock_start] = ValueTuple(W_R_t, U_R_t, W_N_t, U_N_t)

    # Pre-shock (t < shock_start): pre-COVID steady state (myopic)
    for t in (shock_start-1):-1:1
        value_path[t] = ValueTuple(W_R_ss, U_R_ss, W_N_ss, U_N_ss)
    end

    return value_path
end


# Anticipated version means that individuals anticipate COVID and the benefit change
function compute_value_path_anticipated(γ_pre, μ_pre, γ_post, μ_post)
    value_path = Vector{ValueTuple}(undef, T+1)

    # Terminal condition: pre-COVID steady state
    _, _, _, _, W_N_ss, W_R_ss, U_N_ss, U_R_ss, _, _ = job_finding_rates(γ_pre, μ_pre, y_pre, b_R_pre)
    value_path[T+1] = ValueTuple(W_R_ss, U_R_ss, W_N_ss, U_N_ss)

    # Backward induction
    for t in T:-1:1
        if shock_start <= t <= shock_end
            γ_t, μ_t, y_t, b_R_t = γ_post, μ_post, y_post, b_R_post
        else
            γ_t, μ_t, y_t, b_R_t = γ_pre, μ_pre, y_pre, b_R_pre
        end

        V_next = value_path[t+1]
        b̄ = p_R * b_R_t + (1 - p_R)*b_N + l_u
        θ, w_t = solve_tightness(μ_t, y_t, b̄)
        p_contact = matching_rates(θ, μ_t).f

        W_R_t = w_t + β*((1-δ)*V_next.W_R + δ*V_next.U_R)
        U_R_t = b_R_t + l_u + β*(p_contact*V_next.W_R + (1-p_contact)*V_next.U_R)

        W_N_t = w_t + β*((1-δ)*V_next.W_N + δ*V_next.U_N)
        U_N_t = b_N + l_u + β*(p_contact*V_next.W_N + (1-p_contact)*V_next.U_N)

        value_path[t] = ValueTuple(W_R_t, U_R_t, W_N_t, U_N_t)
    end

    return value_path
end

# Core benefit only scenario
function compute_value_path_benefit_only()
    value_path = Vector{ValueTuple}(undef, T+1)

    # Terminal condition: pre-COVID steady state
    _, _, _, _, W_N_ss, W_R_ss, U_N_ss, U_R_ss, _, _ =
        job_finding_rates(γ_pre, μ_pre, y_pre, b_R_pre)
    value_path[T+1] = ValueTuple(W_R_ss, U_R_ss, W_N_ss, U_N_ss)

    # Post-shock (t = T down to shock_end+1): agents anticipate pre-COVID return
    for t in T:-1:(shock_end+1)
        V_next = value_path[t+1]
        y_t, b_R_t = y_pre, b_R_pre  # post-reversal parameters
        γ_t, μ_t = γ_pre, μ_pre      # labor market unchanged

        b̄ = p_R * b_R_t + (1 - p_R)*b_N + l_u
        θ, w_t = solve_tightness(μ_t, y_t, b̄)
        p_contact = matching_rates(θ, μ_t).f

        W_R_t = w_t + β*((1-δ)*V_next.W_R + δ*V_next.U_R)
        U_R_t = b_R_t + l_u + β*(p_contact*V_next.W_R + (1-p_contact)*V_next.U_R)

        W_N_t = w_t + β*((1-δ)*V_next.W_N + δ*V_next.U_N)
        U_N_t = b_N + l_u + β*(p_contact*V_next.W_N + (1-p_contact)*V_next.U_N)

        value_path[t] = ValueTuple(W_R_t, U_R_t, W_N_t, U_N_t)
    end

    # Shock period (t = shock_end down to shock_start+1): agents anticipate reversal
    for t in shock_end:-1:(shock_start+1)
        V_next = value_path[t+1]
        y_t, b_R_t = y_post, b_R_post
        γ_t, μ_t = γ_pre, μ_pre

        b̄ = p_R * b_R_t + (1 - p_R)*b_N + l_u
        θ, w_t = solve_tightness(μ_t, y_t, b̄)
        p_contact = matching_rates(θ, μ_t).f

        W_R_t = w_t + β*((1-δ)*V_next.W_R + δ*V_next.U_R)
        U_R_t = b_R_t + l_u + β*(p_contact*V_next.W_R + (1-p_contact)*V_next.U_R)

        W_N_t = w_t + β*((1-δ)*V_next.W_N + δ*V_next.U_N)
        U_N_t = b_N + l_u + β*(p_contact*V_next.W_N + (1-p_contact)*V_next.U_N)

        value_path[t] = ValueTuple(W_R_t, U_R_t, W_N_t, U_N_t)
    end

    # At shock_start (agents did NOT anticipate shock)
    V_next = value_path[shock_start+1]
    y_t, b_R_t = y_post, b_R_post
    γ_t, μ_t = γ_pre, μ_pre

    b̄ = p_R * b_R_t + (1 - p_R)*b_N + l_u
    θ, w_t = solve_tightness(μ_t, y_t, b̄)
    p_contact = matching_rates(θ, μ_t).f

    # Myopic update using pre-shock steady state continuation values
    W_R_t = w_t + β*((1-δ)*W_R_ss + δ*U_R_ss)
    U_R_t = b_R_t + l_u + β*(p_contact*W_R_ss + (1-p_contact)*U_R_ss)

    W_N_t = w_t + β*((1-δ)*W_N_ss + δ*U_N_ss)
    U_N_t = b_N + l_u + β*(p_contact*W_N_ss + (1-p_contact)*U_N_ss)

    value_path[shock_start] = ValueTuple(W_R_t, U_R_t, W_N_t, U_N_t)

    # Pre-shock: pre-COVID steady state (myopic)
    for t in (shock_start-1):-1:1
        value_path[t] = ValueTuple(W_R_ss, U_R_ss, W_N_ss, U_N_ss)
    end

    return value_path
end

# Anticipated benefit scenario
function compute_value_path_benefit_only_anticipated()
    value_path = Vector{ValueTuple}(undef, T+1)

    # Terminal condition: pre-COVID steady state
    _, _, _, _, W_N_ss, W_R_ss, U_N_ss, U_R_ss, _, _ =
        job_finding_rates(γ_pre, μ_pre, y_pre, b_R_pre)
    value_path[T+1] = ValueTuple(W_R_ss, U_R_ss, W_N_ss, U_N_ss)

    for t in T:-1:1
        # Only benefit payment and y change
        if shock_start <= t <= shock_end
            y_t, b_R_t = y_post, b_R_post
        else
            y_t, b_R_t = y_pre, b_R_pre
        end

        γ_t, μ_t = γ_pre, μ_pre  # keep labor market parameters constant

        V_next = value_path[t+1]
        b̄ = p_R * b_R_t + (1 - p_R)*b_N + l_u
        θ, w_t = solve_tightness(μ_t, y_t, b̄)
        p_contact = matching_rates(θ, μ_t).f

        W_R_t = w_t + β*((1-δ)*V_next.W_R + δ*V_next.U_R)
        U_R_t = b_R_t + l_u + β*(p_contact*V_next.W_R + (1-p_contact)*V_next.U_R)

        W_N_t = w_t + β*((1-δ)*V_next.W_N + δ*V_next.U_N)
        U_N_t = b_N + l_u + β*(p_contact*V_next.W_N + (1-p_contact)*V_next.U_N)

        value_path[t] = ValueTuple(W_R_t, U_R_t, W_N_t, U_N_t)
    end

    return value_path
end


# Job finding rates using current γ, μ, y, b_R
function job_finding_rates(γ, μ, y, b_R)
    b̄ = p_R * b_R + (1 - p_R)*b_N + l_u
    θ, w = solve_tightness(μ, y, b̄)
    rates = matching_rates(θ, μ)
    p_contact = rates.f

    W_R, U_R = w, b_R + l_u
    for _ in 1:500
        W_R_new = w + β*((1-δ)*W_R + δ*U_R)
        U_R_new = b_R + l_u + β*(p_contact*W_R + (1-p_contact)*U_R)
        if abs(W_R_new - W_R) < 1e-8 && abs(U_R_new - U_R) < 1e-8
            break
        end
        W_R, U_R = W_R_new, U_R_new
    end

    W_N, U_N = w, b_N + l_u
    for _ in 1:500
        W_N_new = w + β*((1-δ)*W_N + δ*U_N)
        U_N_new = b_N + l_u + β*(p_contact*W_N + (1-p_contact)*U_N)
        if abs(W_N_new - W_N) < 1e-8 && abs(U_N_new - U_N) < 1e-8
            break
        end
        W_N, U_N = W_N_new, U_N_new
    end

    s_R = β * p_contact * (W_R - U_R) / γ
    s_N = β * p_contact * (W_N - U_N) / γ
    f_R = s_R * p_contact
    f_N = s_N * p_contact

    return f_N, f_R, s_N, s_R, W_N, W_R, U_N, U_R, θ, w
end


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

println("\n🔧 Calibrating parameters...")

γ_pre, μ_pre, _, _, _ = calibrate_grid(y_pre, b_R_pre, f_N_target_pre, f_R_target_pre)
γ_post, μ_post, _, _, _ = calibrate_grid(y_post, b_R_post, f_N_target_post, f_R_target_post)

println("✅ Calibration complete: γ_pre=$(γ_pre), μ_pre=$(μ_pre), γ_post=$(γ_post), μ_post=$(μ_post)")
