# search_matching_model_dual_agents_dynamic.jl
# Full model: parameterized, modular, variable leisure support - new calibration to accurately capture dyanmic nature of shock (calibrated on average observed change)
# Nash Bargaining still not fully included.

using Printf, Plots, CairoMakie, Statistics

# Parameters
β    = 0.99
δ    = 0.02
p_R  = 0.5
α    = 0.5
φ    = 0.5
y_pre = 2.0
y_post = 2.0
b_R_pre = 0.28
b_R_post = 1.2 * b_R_pre
b_N = 0.0
l_u_R_pre, l_u_N_pre = 0.1, 0.1
l_u_R_post, l_u_N_post = 0.3, 0.1
c_vac = 0.21
wge = 0.8

f_N_target_pre = 0.10
f_R_target_pre = 0.0866
f_N_target_post = 0.0821
f_R_target_post = 0.0312

T = 30
shock_start, shock_end = 7, 13

struct ValueTuple
    W_R::Float64; U_R::Float64; W_N::Float64; U_N::Float64
end

function matching_rates(θ, μ)
    f = μ * θ^(1 - α); q = μ * θ^(-α)
    return (f=min(f,1.0), q=min(q,1.0))
end

exogenous_wage = true
function nash_wage(y, b̄)
    exogenous_wage ? wge : φ * y + (1 - φ) * b̄
end

function firm_surplus(y, w)
    surplus = y - w
    if surplus <= 0
        error("Firm surplus non-positive: y=$y, w=$w")
    end
    return surplus / (1 - β * (1 - δ))
end

function solve_tightness(μ, y, b̄)
    w = nash_wage(y, b̄)
    J = firm_surplus(y, w)
    θ = ((β * μ * J) / c_vac)^(1 / α)
    return θ, w
end

function job_finding_rates(γ, μ, y, b_R, l_u_R, l_u_N)
    b̄ = p_R * b_R + (1 - p_R)*b_N + (p_R*l_u_R + (1-p_R)*l_u_N)
    θ, w = solve_tightness(μ, y, b̄)
    p_contact = matching_rates(θ, μ).f

    W_R, U_R = w, b_R + l_u_R
    for _ in 1:500
        W_R_new = w + β*((1-δ)*W_R + δ*U_R)
        U_R_new = b_R + l_u_R + β*(p_contact*W_R + (1-p_contact)*U_R)
        if abs(W_R_new - W_R) < 1e-8 && abs(U_R_new - U_R) < 1e-8
            break
        end
        W_R, U_R = W_R_new, U_R_new
    end

    W_N, U_N = w, b_N + l_u_N
    for _ in 1:500
        W_N_new = w + β*((1-δ)*W_N + δ*U_N)
        U_N_new = b_N + l_u_N + β*(p_contact*W_N + (1-p_contact)*U_N)
        if abs(W_N_new - W_N) < 1e-8 && abs(U_N_new - U_N) < 1e-8
            break
        end
        W_N, U_N = W_N_new, U_N_new
    end

    s_R = β * p_contact * (W_R - U_R) / γ
    s_N = β * p_contact * (W_N - U_N) / γ
    f_R = s_R * p_contact; f_N = s_N * p_contact

    return f_N, f_R, s_N, s_R, W_N, W_R, U_N, U_R, θ, w
end

function compute_value_path(γ_pre, μ_pre, γ_post, μ_post,
                            y_pre, y_post, b_R_pre, b_R_post,
                            l_u_R_pre, l_u_R_post, l_u_N_pre, l_u_N_post)

    value_path = Vector{ValueTuple}(undef, T+1)

    _, _, _, _, W_N_ss, W_R_ss, U_N_ss, U_R_ss, _, _ =
        job_finding_rates(γ_pre, μ_pre, y_pre, b_R_pre, l_u_R_pre, l_u_N_pre)
    value_path[T+1] = ValueTuple(W_R_ss, U_R_ss, W_N_ss, U_N_ss)

    for t in T:-1:1
        V_next = value_path[t+1]
        if t >= shock_end+1
            γ_t, μ_t, y_t, b_R_t = γ_pre, μ_pre, y_pre, b_R_pre
            l_u_R_t, l_u_N_t = l_u_R_pre, l_u_N_pre
        elseif t >= shock_start
            γ_t, μ_t, y_t, b_R_t = γ_post, μ_post, y_post, b_R_post
            l_u_R_t, l_u_N_t = l_u_R_post, l_u_N_post
        else
            γ_t, μ_t, y_t, b_R_t = γ_pre, μ_pre, y_pre, b_R_pre
            l_u_R_t, l_u_N_t = l_u_R_pre, l_u_N_pre
        end

        b̄ = p_R*b_R_t + (1 - p_R)*b_N + (p_R*l_u_R_t + (1-p_R)*l_u_N_t)
        θ, w_t = solve_tightness(μ_t, y_t, b̄)
        p_contact = matching_rates(θ, μ_t).f

        W_R_t = w_t + β*((1-δ)*V_next.W_R + δ*V_next.U_R)
        U_R_t = b_R_t + l_u_R_t + β*(p_contact*V_next.W_R + (1-p_contact)*V_next.U_R)

        W_N_t = w_t + β*((1-δ)*V_next.W_N + δ*V_next.U_N)
        U_N_t = b_N + l_u_N_t + β*(p_contact*V_next.W_N + (1-p_contact)*V_next.U_N)

        value_path[t] = ValueTuple(W_R_t, U_R_t, W_N_t, U_N_t)
    end

    return value_path
end

function calibrate_grid_static(y, b_R, l_u_R, l_u_N, f_N_target, f_R_target)
    γ_vals = range(0.001, stop=1.0, length=30)
    μ_vals = range(0.05, stop=1.5, length=30)
    residuals = fill(Inf, length(γ_vals), length(μ_vals))
    best_γ, best_μ, min_residual = NaN, NaN, Inf

    for (i, γ) in enumerate(γ_vals), (j, μ) in enumerate(μ_vals)
        try
            f_N, f_R, _, _, _, _, _, _, _ = job_finding_rates(γ, μ, y, b_R, l_u_R, l_u_N)
            res = (f_N - f_N_target)^2 + (f_R - f_R_target)^2
            residuals[i, j] = sqrt(res)
            if res < min_residual
                min_residual = res
                best_γ, best_μ = γ, μ
            end
        catch
            residuals[i, j] = NaN
        end
    end

    println("✅ Pre-COVID steady-state calibration:")
    println("   γ=$(best_γ), μ=$(best_μ), residual norm=$(sqrt(min_residual))")

    return best_γ, best_μ, residuals, γ_vals, μ_vals
end

function calibrate_grid(y_pre, b_R_pre, y_post, b_R_post,
                        f_N_target_pre, f_R_target_pre,
                        f_N_target_post, f_R_target_post,
                        l_u_R_pre, l_u_R_post, l_u_N_pre, l_u_N_post)
    γ_vals = range(0.001, stop=1.0, length=30)
    μ_vals = range(0.05, stop=1.5, length=30)

    # Step 1: Pre-COVID calibration
    γ_pre, μ_pre, _, _, _ = calibrate_grid_static(y_pre, b_R_pre, l_u_R_pre, l_u_N_pre,
                                                 f_N_target_pre, f_R_target_pre)

    # Step 2: Post-COVID dynamic calibration
    best_γ_post, best_μ_post, min_residual_post = NaN, NaN, Inf

    for (i, γ_post) in enumerate(γ_vals), (j, μ_post) in enumerate(μ_vals)
        try
            path = compute_value_path(γ_pre, μ_pre, γ_post, μ_post,
                                      y_pre, y_post, b_R_pre, b_R_post,
                                      l_u_R_pre, l_u_R_post, l_u_N_pre, l_u_N_post)
            f_N_path, f_R_path = simulate_job_finding_rates(path,
                γ_pre, μ_pre, γ_post, μ_post,
                y_pre, y_post, b_R_pre, b_R_post,
                l_u_R_pre, l_u_R_post, l_u_N_pre, l_u_N_post)

            f_N_avg = mean(f_N_path[shock_start:shock_end])
            f_R_avg = mean(f_R_path[shock_start:shock_end])
            res = (f_N_avg - f_N_target_post)^2 + (f_R_avg - f_R_target_post)^2

            if res < min_residual_post
                min_residual_post = res
                best_γ_post, best_μ_post = γ_post, μ_post
            end
        catch e
            @warn "Calibration error at γ_post=$γ_post, μ_post=$μ_post: $e"
        end
    end

    println("✅ Dynamic calibration complete:")
    println("   γ_pre=$(γ_pre), μ_pre=$(μ_pre)")
    println("   γ_post=$(best_γ_post), μ_post=$(best_μ_post)")
    return γ_pre, μ_pre, best_γ_post, best_μ_post, γ_vals, μ_vals
end
