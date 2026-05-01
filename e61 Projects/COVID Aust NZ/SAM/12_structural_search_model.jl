# 12_structural_search_model.jl
#
# Two-type finite-horizon search and matching model for interpreting the DiD
# between benefit-eligible Australians (R) and ineligible New Zealanders (N).
#
# Main improvements relative to the 11_ scripts:
# - Search effort is chosen inside the unemployed Bellman problem.
# - Benefits, outside-option/leisure values, matching efficiency, and health/search
#   cost shocks are separate environment objects.
# - Counterfactuals are explicit: full shock, benefit-only, and COVID-only.
# - Calibration uses named fields rather than long positional leisure arguments.

using CSV
using DataFrames
using Plots
using Printf
using Statistics

Base.@kwdef struct ModelSpec
    beta::Float64 = 0.99
    delta::Float64 = 0.01
    recipient_share::Float64 = 0.5
    match_elasticity::Float64 = 0.5
    bargaining_power::Float64 = 0.5
    vacancy_cost::Float64 = 1.0
    exogenous_wage::Bool = true
    wage::Float64 = 0.55
    risk_aversion::Float64 = 0.0
    max_search::Float64 = 1.0
    probability_cap::Float64 = 0.999
    flow_floor::Float64 = 1e-8
    T::Int = 60
    shock_start::Int = 12
    shock_end::Int = 36
end

Base.@kwdef struct Environment
    mu::Float64
    y::Float64
    b_R::Float64
    b_N::Float64
    leisure_R::Float64
    leisure_N::Float64
    kappa_R::Float64
    kappa_N::Float64
end

struct ValueState
    W_R::Float64
    U_R::Float64
    W_N::Float64
    U_N::Float64
end

Base.@kwdef struct PeriodOutcome
    value::ValueState
    search_R::Float64
    search_N::Float64
    find_R::Float64
    find_N::Float64
    contact::Float64
    theta::Float64
    wage::Float64
end

function utility(flow::Float64, spec::ModelSpec)
    c = max(flow, spec.flow_floor)
    rho = spec.risk_aversion
    if abs(rho) < 1e-12
        return c
    elseif abs(rho - 1.0) < 1e-12
        return log(c)
    else
        return (c^(1.0 - rho) - 1.0) / (1.0 - rho)
    end
end

outside_R(env::Environment) = env.b_R + env.leisure_R
outside_N(env::Environment) = env.b_N + env.leisure_N

function wage_rule(env::Environment, spec::ModelSpec)
    if spec.exogenous_wage
        return spec.wage
    end

    avg_outside = spec.recipient_share * outside_R(env) +
                  (1.0 - spec.recipient_share) * outside_N(env)
    return spec.bargaining_power * env.y + (1.0 - spec.bargaining_power) * avg_outside
end

function labor_market(env::Environment, spec::ModelSpec)
    wage = wage_rule(env, spec)
    surplus = env.y - wage
    if surplus <= 0.0
        error("Firm surplus is non-positive: y=$(env.y), wage=$wage")
    end

    firm_value = surplus / (1.0 - spec.beta * (1.0 - spec.delta))
    alpha = spec.match_elasticity
    theta = ((spec.beta * env.mu * firm_value) / spec.vacancy_cost)^(1.0 / alpha)
    contact = min(env.mu * theta^(1.0 - alpha), spec.probability_cap)

    return theta, contact, wage
end

function choose_search(next_surplus::Float64, contact::Float64, kappa::Float64, spec::ModelSpec)
    if next_surplus <= 0.0 || contact <= 0.0
        return 0.0
    end

    unconstrained = spec.beta * contact * next_surplus / kappa
    probability_bound = spec.probability_cap / contact
    return clamp(unconstrained, 0.0, min(spec.max_search, probability_bound))
end

function bellman_update(env::Environment, next::ValueState, spec::ModelSpec)
    theta, contact, wage = labor_market(env, spec)

    s_R = choose_search(next.W_R - next.U_R, contact, env.kappa_R, spec)
    s_N = choose_search(next.W_N - next.U_N, contact, env.kappa_N, spec)
    f_R = min(s_R * contact, spec.probability_cap)
    f_N = min(s_N * contact, spec.probability_cap)

    work_flow = utility(wage, spec)
    unemp_flow_R = utility(outside_R(env), spec) - 0.5 * env.kappa_R * s_R^2
    unemp_flow_N = utility(outside_N(env), spec) - 0.5 * env.kappa_N * s_N^2

    W_R = work_flow + spec.beta * ((1.0 - spec.delta) * next.W_R + spec.delta * next.U_R)
    W_N = work_flow + spec.beta * ((1.0 - spec.delta) * next.W_N + spec.delta * next.U_N)
    U_R = unemp_flow_R + spec.beta * (f_R * next.W_R + (1.0 - f_R) * next.U_R)
    U_N = unemp_flow_N + spec.beta * (f_N * next.W_N + (1.0 - f_N) * next.U_N)

    return PeriodOutcome(
        value = ValueState(W_R, U_R, W_N, U_N),
        search_R = s_R,
        search_N = s_N,
        find_R = f_R,
        find_N = f_N,
        contact = contact,
        theta = theta,
        wage = wage,
    )
end

function stationary_values(env::Environment, spec::ModelSpec; tol::Float64 = 1e-9, max_iter::Int = 20_000)
    work_flow = utility(wage_rule(env, spec), spec)
    guess = ValueState(
        work_flow / (1.0 - spec.beta),
        utility(outside_R(env), spec) / (1.0 - spec.beta),
        work_flow / (1.0 - spec.beta),
        utility(outside_N(env), spec) / (1.0 - spec.beta),
    )

    for _ in 1:max_iter
        out = bellman_update(env, guess, spec)
        new = out.value
        diff = maximum(abs.((new.W_R - guess.W_R, new.U_R - guess.U_R,
                             new.W_N - guess.W_N, new.U_N - guess.U_N)))
        guess = new
        diff < tol && return guess
    end

    @warn "Stationary value iteration did not converge"
    return guess
end

function environment_for_period(t::Int, pre_env::Environment, shock_env::Environment, spec::ModelSpec)
    return spec.shock_start <= t <= spec.shock_end ? shock_env : pre_env
end

function compute_value_path(pre_env::Environment, shock_env::Environment, spec::ModelSpec;
                            anticipation::Symbol = :unanticipated)
    values = Vector{ValueState}(undef, spec.T + 1)
    pre_ss = stationary_values(pre_env, spec)
    values[spec.T + 1] = pre_ss

    for t in spec.T:-1:1
        env_t = environment_for_period(t, pre_env, shock_env, spec)
        next_value = values[t + 1]

        if anticipation == :unanticipated && t < spec.shock_start
            next_value = pre_ss
        end

        values[t] = bellman_update(env_t, next_value, spec).value
    end

    return values
end

function simulate_path(pre_env::Environment, shock_env::Environment, spec::ModelSpec;
                       u_R0::Float64 = 0.02, u_N0::Float64 = 0.05,
                       anticipation::Symbol = :unanticipated)
    values = compute_value_path(pre_env, shock_env, spec; anticipation = anticipation)
    pre_ss = stationary_values(pre_env, spec)

    u_R = zeros(spec.T + 1)
    u_N = zeros(spec.T + 1)
    employment = zeros(spec.T + 1)
    search_R = zeros(spec.T)
    search_N = zeros(spec.T)
    find_R = zeros(spec.T)
    find_N = zeros(spec.T)
    contact = zeros(spec.T)
    theta = zeros(spec.T)
    wage = zeros(spec.T)
    surplus_R = zeros(spec.T)
    surplus_N = zeros(spec.T)

    u_R[1], u_N[1] = u_R0, u_N0
    employment[1] = 1.0 - u_R0 - u_N0

    for t in 1:spec.T
        env_t = environment_for_period(t, pre_env, shock_env, spec)
        decision_next = anticipation == :unanticipated && t < spec.shock_start ? pre_ss : values[t + 1]
        out = bellman_update(env_t, decision_next, spec)

        search_R[t], search_N[t] = out.search_R, out.search_N
        find_R[t], find_N[t] = out.find_R, out.find_N
        contact[t], theta[t], wage[t] = out.contact, out.theta, out.wage
        surplus_R[t] = decision_next.W_R - decision_next.U_R
        surplus_N[t] = decision_next.W_N - decision_next.U_N

        separations = spec.delta * employment[t]
        hires_R = find_R[t] * u_R[t]
        hires_N = find_N[t] * u_N[t]
        u_R[t + 1] = max(u_R[t] + spec.recipient_share * separations - hires_R, 0.0)
        u_N[t + 1] = max(u_N[t] + (1.0 - spec.recipient_share) * separations - hires_N, 0.0)
        employment[t + 1] = 1.0 - u_R[t + 1] - u_N[t + 1]
    end

    return (
        values = values,
        u_R = u_R,
        u_N = u_N,
        employment = employment,
        search_R = search_R,
        search_N = search_N,
        find_R = find_R,
        find_N = find_N,
        contact = contact,
        theta = theta,
        wage = wage,
        surplus_R = surplus_R,
        surplus_N = surplus_N,
    )
end

function copy_env(env::Environment; mu = env.mu, y = env.y, b_R = env.b_R, b_N = env.b_N,
                  leisure_R = env.leisure_R, leisure_N = env.leisure_N,
                  kappa_R = env.kappa_R, kappa_N = env.kappa_N)
    return Environment(
        mu = mu,
        y = y,
        b_R = b_R,
        b_N = b_N,
        leisure_R = leisure_R,
        leisure_N = leisure_N,
        kappa_R = kappa_R,
        kappa_N = kappa_N,
    )
end

function logrange(lo::Float64, hi::Float64, n::Int)
    return exp.(range(log(lo), log(hi), length = n))
end

function minimize_on_log_grid(objective, lo::Float64, hi::Float64; grid_n::Int = 80, passes::Int = 3)
    best = (loss = Inf, x = NaN, payload = nothing)
    current_lo, current_hi = lo, hi

    for _ in 1:passes
        for x in logrange(current_lo, current_hi, grid_n)
            loss, payload = objective(x)
            if loss < best.loss
                best = (loss = loss, x = x, payload = payload)
            end
        end

        step = exp((log(current_hi) - log(current_lo)) / max(grid_n - 1, 1))
        current_lo = max(lo, best.x / step^3)
        current_hi = min(hi, best.x * step^3)
    end

    return best
end

function period_means(find_N::Vector{Float64}, find_R::Vector{Float64}, spec::ModelSpec)
    pre = 1:(spec.shock_start - 1)
    shock = spec.shock_start:spec.shock_end
    post = (spec.shock_end + 1):spec.T
    return (
        pre_N = mean(find_N[pre]),
        pre_R = mean(find_R[pre]),
        shock_N = mean(find_N[shock]),
        shock_R = mean(find_R[shock]),
        post_N = mean(find_N[post]),
        post_R = mean(find_R[post]),
    )
end

function calibrate_pre(base_env::Environment, spec::ModelSpec, target_N::Float64, target_R::Float64;
                       grid_n::Int = 55)
    best = (loss = Inf, mu = NaN, kappa = NaN, find_N = NaN, find_R = NaN)

    mu_lo, mu_hi = 0.01, 2.0
    kappa_lo, kappa_hi = 0.001, 5.0
    for _ in 1:3
        for mu in logrange(mu_lo, mu_hi, grid_n), kappa in logrange(kappa_lo, kappa_hi, grid_n)
            env = copy_env(base_env; mu = mu, kappa_R = kappa, kappa_N = kappa)
            values = stationary_values(env, spec)
            out = bellman_update(env, values, spec)
            loss = (out.find_N - target_N)^2 + (out.find_R - target_R)^2
            if loss < best.loss
                best = (loss = loss, mu = mu, kappa = kappa, find_N = out.find_N, find_R = out.find_R)
            end
        end

        mu_step = exp((log(mu_hi) - log(mu_lo)) / max(grid_n - 1, 1))
        kappa_step = exp((log(kappa_hi) - log(kappa_lo)) / max(grid_n - 1, 1))
        mu_lo, mu_hi = max(0.01, best.mu / mu_step^3), min(2.0, best.mu * mu_step^3)
        kappa_lo, kappa_hi = max(0.001, best.kappa / kappa_step^3), min(5.0, best.kappa * kappa_step^3)
    end

    env = copy_env(base_env; mu = best.mu, kappa_R = best.kappa, kappa_N = best.kappa)
    return env, best
end

function calibrate_post_targeted(pre_env::Environment, post_template::Environment, spec::ModelSpec,
                                 target_N::Float64, target_R::Float64;
                                 common_search_cost_multiplier::Float64 = 1.0,
                                 grid_n::Int = 120)
    # Identification choice:
    # 1. The ineligible group has no benefit shock, so it anchors the common
    #    COVID job-availability shock, mu_post, conditional on an assumed common
    #    search-cost/health multiplier.
    # 2. The eligible group's remaining post-target gap is fitted with a
    #    recipient-specific kappa_R shock. This is the explicit "other
    #    differential shock" that would confound a plain DiD.
    kappa_N_post = pre_env.kappa_N * common_search_cost_multiplier

    best_mu = minimize_on_log_grid(0.01, 2.0; grid_n = grid_n, passes = 4) do mu
        post_env = copy_env(post_template; mu = mu, kappa_N = kappa_N_post, kappa_R = pre_env.kappa_R)
        sim = simulate_path(pre_env, post_env, spec)
        means = period_means(sim.find_N, sim.find_R, spec)
        loss = (means.shock_N - target_N)^2
        return loss, means.shock_N
    end

    best_kappa_R = minimize_on_log_grid(0.001, 20.0; grid_n = grid_n, passes = 4) do kappa_R
        post_env = copy_env(post_template; mu = best_mu.x, kappa_N = kappa_N_post, kappa_R = kappa_R)
        sim = simulate_path(pre_env, post_env, spec)
        means = period_means(sim.find_N, sim.find_R, spec)
        loss = (means.shock_R - target_R)^2
        return loss, means.shock_R
    end

    env = copy_env(
        post_template;
        mu = best_mu.x,
        kappa_N = kappa_N_post,
        kappa_R = best_kappa_R.x,
    )

    fit = (
        mu = best_mu.x,
        kappa_N = kappa_N_post,
        kappa_R = best_kappa_R.x,
        shock_N = best_mu.payload,
        shock_R = best_kappa_R.payload,
        loss = best_mu.loss + best_kappa_R.loss,
    )

    return env, fit
end

function did_series(sim)
    diff = sim.find_R .- sim.find_N
    return diff .- diff[1]
end

function print_means(label::String, sim, spec::ModelSpec)
    m = period_means(sim.find_N, sim.find_R, spec)
    @printf("\n%s average job-finding rates (percent)\n", label)
    @printf("  Pre:   N = %5.2f, R = %5.2f, R-N = %6.2f p.p.\n",
            100m.pre_N, 100m.pre_R, 100(m.pre_R - m.pre_N))
    @printf("  Shock: N = %5.2f, R = %5.2f, R-N = %6.2f p.p.\n",
            100m.shock_N, 100m.shock_R, 100(m.shock_R - m.shock_N))
    @printf("  Post:  N = %5.2f, R = %5.2f, R-N = %6.2f p.p.\n",
            100m.post_N, 100m.post_R, 100(m.post_R - m.post_N))
end

function scenario_summary(full_sim, benefit_sim, covid_sim, spec::ModelSpec)
    shock = spec.shock_start:spec.shock_end
    did_full = did_series(full_sim)
    did_benefit = did_series(benefit_sim)
    did_covid = did_series(covid_sim)
    did_interaction = did_full .- did_benefit .- did_covid

    return (
        full = mean(did_full[shock]),
        benefit = mean(did_benefit[shock]),
        covid = mean(did_covid[shock]),
        interaction = mean(did_interaction[shock]),
    )
end

function run_model()
    spec = ModelSpec()

    target_pre_N = 0.1000
    target_pre_R = 0.0866
    target_post_N = 0.0821
    target_post_R = 0.0312

    # Data and identifying assumptions. Change these in one place for sensitivity runs.
    wage = spec.wage
    benefit_R_pre = 0.28 * wage
    benefit_R_post = 2.0 * benefit_R_pre
    benefit_N = 0.0
    y_pre = 1.0
    y_post = 1.0

    leisure_R_pre = 0.10 * wage
    leisure_N_pre = 0.10 * wage

    # Default: no differential leisure/outside-option shock. To test the idea that
    # COVID increased the value of non-work more for one group, change these.
    leisure_R_post_multiplier = 1.0
    leisure_N_post_multiplier = 1.0
    leisure_R_post = leisure_R_pre * leisure_R_post_multiplier
    leisure_N_post = leisure_N_pre * leisure_N_post_multiplier

    uncalibrated_pre = Environment(
        mu = 0.4,
        y = y_pre,
        b_R = benefit_R_pre,
        b_N = benefit_N,
        leisure_R = leisure_R_pre,
        leisure_N = leisure_N_pre,
        kappa_R = 0.2,
        kappa_N = 0.2,
    )

    post_template = copy_env(
        uncalibrated_pre;
        y = y_post,
        b_R = benefit_R_post,
        leisure_R = leisure_R_post,
        leisure_N = leisure_N_post,
    )

    println("\nCalibrating pre-COVID mu and common search cost kappa...")
    pre_env, pre_fit = calibrate_pre(uncalibrated_pre, spec, target_pre_N, target_pre_R)
    @printf("  mu_pre = %.5f, kappa_pre = %.5f, fitted f_N = %.4f, f_R = %.4f, loss = %.2e\n",
            pre_fit.mu, pre_fit.kappa, pre_fit.find_N, pre_fit.find_R, pre_fit.loss)

    common_search_cost_multiplier = 1.0
    println("\nCalibrating COVID shock with N as the common-shock anchor...")
    full_post_env, post_fit = calibrate_post_targeted(
        pre_env,
        post_template,
        spec,
        target_post_N,
        target_post_R;
        common_search_cost_multiplier = common_search_cost_multiplier,
    )
    @printf("  assumed common kappa multiplier = %.3f\n", common_search_cost_multiplier)
    @printf("  mu_post = %.5f, kappa_N_post = %.5f, kappa_R_post = %.5f\n",
            post_fit.mu, post_fit.kappa_N, post_fit.kappa_R)
    @printf("  fitted shock f_N = %.4f, f_R = %.4f, loss = %.2e\n",
            post_fit.shock_N, post_fit.shock_R, post_fit.loss)
    @printf("  implied recipient-specific kappa multiplier = %.3f\n", post_fit.kappa_R / pre_fit.kappa)

    benefit_only_env = copy_env(
        pre_env;
        b_R = benefit_R_post,
        b_N = benefit_N,
        leisure_R = leisure_R_pre,
        leisure_N = leisure_N_pre,
    )

    covid_only_env = copy_env(
        full_post_env;
        b_R = benefit_R_pre,
        b_N = benefit_N,
        leisure_R = leisure_R_post,
        leisure_N = leisure_N_post,
    )

    full_sim = simulate_path(pre_env, full_post_env, spec)
    benefit_sim = simulate_path(pre_env, benefit_only_env, spec)
    covid_sim = simulate_path(pre_env, covid_only_env, spec)

    print_means("Full shock", full_sim, spec)
    print_means("Benefit-only", benefit_sim, spec)
    print_means("COVID-only", covid_sim, spec)

    summary = scenario_summary(full_sim, benefit_sim, covid_sim, spec)
    @printf("\nAverage shock-period DiD decomposition (p.p.)\n")
    @printf("  Full shock:   %7.3f\n", 100summary.full)
    @printf("  Benefit only: %7.3f\n", 100summary.benefit)
    @printf("  COVID only:   %7.3f\n", 100summary.covid)
    @printf("  Interaction:  %7.3f\n", 100summary.interaction)

    did_full = did_series(full_sim)
    did_benefit = did_series(benefit_sim)
    did_covid = did_series(covid_sim)
    did_interaction = did_full .- did_benefit .- did_covid

    df = DataFrame(
        Week = 1:spec.T,
        DiD_Full_Shock = 100 .* did_full,
        DiD_Benefit_Only = 100 .* did_benefit,
        DiD_COVID_Only = 100 .* did_covid,
        DiD_Interaction = 100 .* did_interaction,
        f_N_Full = 100 .* full_sim.find_N,
        f_R_Full = 100 .* full_sim.find_R,
        f_N_Benefit_Only = 100 .* benefit_sim.find_N,
        f_R_Benefit_Only = 100 .* benefit_sim.find_R,
        f_N_COVID_Only = 100 .* covid_sim.find_N,
        f_R_COVID_Only = 100 .* covid_sim.find_R,
    )
    CSV.write("did_job_finding_rates_12.csv", df)

    plt_did = plot(
        df.Week,
        df.DiD_Full_Shock,
        label = "Full shock",
        lw = 2,
        xlabel = "Week",
        ylabel = "Difference-in-differences (p.p.)",
        title = "Model DiD: Full vs Counterfactual Shocks",
    )
    plot!(plt_did, df.Week, df.DiD_Benefit_Only, label = "Benefit only", lw = 2, ls = :dash)
    plot!(plt_did, df.Week, df.DiD_COVID_Only, label = "COVID only", lw = 2, ls = :dot)
    plot!(plt_did, df.Week, df.DiD_Interaction, label = "Interaction", lw = 2, ls = :dashdot)
    savefig(plt_did, "did_job_finding_rates_12.png")

    plt_levels = plot(
        df.Week,
        df.f_N_Full,
        label = "N full",
        lw = 2,
        xlabel = "Week",
        ylabel = "Job-finding rate (%)",
        title = "Job-Finding Rates",
    )
    plot!(plt_levels, df.Week, df.f_R_Full, label = "R full", lw = 2)
    plot!(plt_levels, df.Week, df.f_N_Benefit_Only, label = "N benefit only", lw = 2, ls = :dash)
    plot!(plt_levels, df.Week, df.f_R_Benefit_Only, label = "R benefit only", lw = 2, ls = :dash)
    plot!(plt_levels, df.Week, df.f_N_COVID_Only, label = "N COVID only", lw = 2, ls = :dot)
    plot!(plt_levels, df.Week, df.f_R_COVID_Only, label = "R COVID only", lw = 2, ls = :dot)
    savefig(plt_levels, "job_finding_rates_12.png")

    return (
        spec = spec,
        pre_env = pre_env,
        full_post_env = full_post_env,
        benefit_only_env = benefit_only_env,
        covid_only_env = covid_only_env,
        full_sim = full_sim,
        benefit_sim = benefit_sim,
        covid_sim = covid_sim,
        summary = summary,
        output = df,
    )
end

if abspath(PROGRAM_FILE) == @__FILE__
    run_model()
end
