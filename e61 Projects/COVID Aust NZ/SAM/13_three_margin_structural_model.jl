# 13_three_margin_structural_model.jl
#
# Bellman-based three-margin extension of the 12_ structural search model.
#
# Purpose:
# - Keep the empirical object as a percentage-point DiD in job-finding rates.
# - Keep the 12_ model backbone: W/U values are solved by Bellman iteration and
#   finite-horizon backward induction.
# - Add the three extensions discussed after reviewing the working paper:
#     1. search intensity chosen inside the unemployed Bellman equation;
#     2. offer acceptance / reservation behaviour;
#     3. endogenous separation / quit risk.
# - Vary the assumed differential COVID health/work-disutility shock between
#   eligible Australians (R) and ineligible New Zealanders (N), and recalibrate
#   search curvature needed to match the observed Australian post-COVID
#   job-finding rate using the known supplement size.
# - Search effort is an index, not a probability. Following the Shimer (2004)
#   multiple-application logic, effort maps concavely into an offer probability:
#   p_offer = 1 - exp(-contact * search).
# - The supplement size is treated as known. The model now calibrates the search
#   curvature needed to match the Australian post-COVID JFR under each assumed
#   differential health/work-disutility shock, rather than scaling the benefit.

using CSV
using DataFrames
using Optim
using Plots
using Printf
using Statistics

Base.@kwdef struct ModelSpec
    beta::Float64 = 0.99
    firm_delta::Float64 = 0.01
    recipient_share::Float64 = 0.5
    match_elasticity::Float64 = 0.42
    vacancy_cost::Float64 = 1.0
    wage::Float64 = 0.55
    y::Float64 = 1.0
    base_replacement_rate::Float64 = 0.28
    benefit_multiplier::Float64 = 2.0
    leisure_value::Float64 = 0.10 * 0.55
    offer_threshold::Float64 = 0.15
    offer_dispersion::Float64 = 0.35
    separation_threshold::Float64 = 0.32
    separation_dispersion::Float64 = 0.08
    sep_health_sensitivity::Float64 = 0.35
    search_curvature::Float64 = 7.0
    max_search::Float64 = 50.0
    probability_cap::Float64 = 0.999
    flow_floor::Float64 = 1e-8
    T::Int = 60
    shock_start::Int = 12
    shock_end::Int = 36
end

Base.@kwdef struct Targets
    # Table 3 reduced-form JFR targets:
    # NZ pre = 0.1002; AUS pre = 0.1002 - 0.0134;
    # NZ post = 0.1002 - 0.0179; AUS post adds Aus x Post = -0.0171.
    # The DiD object is the interaction coefficient, not the common Post effect.
    f_N_pre::Float64 = 0.1002
    f_R_pre::Float64 = 0.0868
    f_N_post::Float64 = 0.0823
    f_R_post::Float64 = 0.0518
    jfr_common_post_coef::Float64 = -0.0179
    jfr_did_coef::Float64 = -0.0171

    # Table 4 reduced-form separation targets:
    # NZ pre = 0.0480; AUS pre = 0.0480 + 0.0004;
    # NZ post = 0.0480 + 0.0070; AUS post adds 0.0372.
    sep_N_pre::Float64 = 0.0480
    sep_R_pre::Float64 = 0.0484
    sep_N_post::Float64 = 0.0550
    sep_R_post::Float64 = 0.0926
    sep_common_post_coef::Float64 = 0.0070
    sep_did_coef::Float64 = 0.0372
end

Base.@kwdef struct Environment
    mu::Float64
    b_R::Float64
    b_N::Float64
    health_R::Float64
    health_N::Float64
    kappa_R::Float64
    kappa_N::Float64
end

Base.@kwdef struct SeparationParams
    base::Float64
    amp::Float64
end

struct ValueState
    W_R::Float64
    U_R::Float64
    W_N::Float64
    U_N::Float64
end

function copy_spec(spec::ModelSpec; search_curvature::Float64 = spec.search_curvature)
    return ModelSpec(
        beta = spec.beta,
        firm_delta = spec.firm_delta,
        recipient_share = spec.recipient_share,
        match_elasticity = spec.match_elasticity,
        vacancy_cost = spec.vacancy_cost,
        wage = spec.wage,
        y = spec.y,
        base_replacement_rate = spec.base_replacement_rate,
        benefit_multiplier = spec.benefit_multiplier,
        leisure_value = spec.leisure_value,
        offer_threshold = spec.offer_threshold,
        offer_dispersion = spec.offer_dispersion,
        separation_threshold = spec.separation_threshold,
        separation_dispersion = spec.separation_dispersion,
        sep_health_sensitivity = spec.sep_health_sensitivity,
        search_curvature = search_curvature,
        max_search = spec.max_search,
        probability_cap = spec.probability_cap,
        flow_floor = spec.flow_floor,
        T = spec.T,
        shock_start = spec.shock_start,
        shock_end = spec.shock_end,
    )
end

Base.@kwdef struct PeriodOutcome
    value::ValueState
    search_R::Float64
    search_N::Float64
    accept_R::Float64
    accept_N::Float64
    sep_R::Float64
    sep_N::Float64
    find_R::Float64
    find_N::Float64
    contact::Float64
    theta::Float64
    surplus_R::Float64
    surplus_N::Float64
end

benefit_pre(spec::ModelSpec) = spec.base_replacement_rate * spec.wage
benefit_post(spec::ModelSpec) = spec.benefit_multiplier * benefit_pre(spec)
benefit_at_supplement_fraction(spec::ModelSpec, fraction::Float64) =
    benefit_pre(spec) + fraction * (benefit_post(spec) - benefit_pre(spec))

logistic(x) = 1.0 / (1.0 + exp(-x))

function utility(flow::Float64, spec::ModelSpec)
    return max(flow, spec.flow_floor)
end

outside_R(env::Environment, spec::ModelSpec) = env.b_R + spec.leisure_value
outside_N(env::Environment, spec::ModelSpec) = env.b_N + spec.leisure_value

function flow_surplus_R(env::Environment, spec::ModelSpec)
    return spec.wage - env.health_R - outside_R(env, spec)
end

function flow_surplus_N(env::Environment, spec::ModelSpec)
    return spec.wage - env.health_N - outside_N(env, spec)
end

function accept_prob(flow_surplus::Float64, spec::ModelSpec)
    return clamp(logistic((flow_surplus - spec.offer_threshold) / spec.offer_dispersion),
                 0.0, 1.0)
end

function sep_shape(flow_surplus::Float64, spec::ModelSpec)
    return logistic((spec.separation_threshold - flow_surplus) / spec.separation_dispersion)
end

function separation_rate(flow_surplus::Float64, health::Float64,
                         sep::SeparationParams, spec::ModelSpec)
    return clamp(sep.base + sep.amp * sep_shape(flow_surplus, spec) +
                 spec.sep_health_sensitivity * health,
                 0.0, spec.probability_cap)
end

function labor_market(env::Environment, spec::ModelSpec)
    firm_surplus = spec.y - spec.wage
    firm_surplus <= 0.0 && error("Firm surplus is non-positive.")

    firm_value = firm_surplus / (1.0 - spec.beta * (1.0 - spec.firm_delta))
    alpha = spec.match_elasticity
    theta = ((spec.beta * env.mu * firm_value) / spec.vacancy_cost)^(1.0 / alpha)
    contact = min(env.mu * theta^(1.0 - alpha), spec.probability_cap)
    return theta, contact
end

function choose_search(next_surplus::Float64, contact::Float64, accept::Float64,
                       kappa::Float64, spec::ModelSpec)
    if next_surplus <= 0.0 || contact <= 0.0 || accept <= 0.0
        return 0.0
    end

    marginal_gain(s) = spec.beta * accept * contact * exp(-contact * s) * next_surplus
    marginal_cost(s) = kappa * s^spec.search_curvature
    foc(s) = marginal_cost(s) - marginal_gain(s)

    if foc(spec.max_search) <= 0.0
        return spec.max_search
    end

    lo, hi = 0.0, spec.max_search
    for _ in 1:80
        mid = 0.5 * (lo + hi)
        if foc(mid) <= 0.0
            lo = mid
        else
            hi = mid
        end
    end
    return 0.5 * (lo + hi)
end

function offer_probability(search::Float64, contact::Float64, spec::ModelSpec)
    # Continuous approximation to Shimer's 1 - (1 - contact)^applications.
    return clamp(1.0 - exp(-contact * max(search, 0.0)), 0.0, spec.probability_cap)
end

function job_finding_probability(search::Float64, contact::Float64,
                                 accept::Float64, spec::ModelSpec)
    return clamp(accept * offer_probability(search, contact, spec),
                 0.0, spec.probability_cap)
end

function search_cost(search::Float64, kappa::Float64, spec::ModelSpec)
    return kappa * max(search, 0.0)^(1.0 + spec.search_curvature) /
           (1.0 + spec.search_curvature)
end

function bellman_update(env::Environment, next::ValueState,
                        sep::SeparationParams, spec::ModelSpec)
    theta, contact = labor_market(env, spec)

    fs_R, fs_N = flow_surplus_R(env, spec), flow_surplus_N(env, spec)
    accept_R, accept_N = accept_prob(fs_R, spec), accept_prob(fs_N, spec)
    sep_R = separation_rate(fs_R, env.health_R, sep, spec)
    sep_N = separation_rate(fs_N, env.health_N, sep, spec)

    next_surplus_R = next.W_R - next.U_R
    next_surplus_N = next.W_N - next.U_N
    search_R = choose_search(next_surplus_R, contact, accept_R, env.kappa_R, spec)
    search_N = choose_search(next_surplus_N, contact, accept_N, env.kappa_N, spec)
    find_R = job_finding_probability(search_R, contact, accept_R, spec)
    find_N = job_finding_probability(search_N, contact, accept_N, spec)

    work_flow_R = utility(spec.wage - env.health_R, spec)
    work_flow_N = utility(spec.wage - env.health_N, spec)
    unemp_flow_R = utility(outside_R(env, spec), spec) -
                   search_cost(search_R, env.kappa_R, spec)
    unemp_flow_N = utility(outside_N(env, spec), spec) -
                   search_cost(search_N, env.kappa_N, spec)

    W_R = work_flow_R + spec.beta * ((1.0 - sep_R) * next.W_R + sep_R * next.U_R)
    W_N = work_flow_N + spec.beta * ((1.0 - sep_N) * next.W_N + sep_N * next.U_N)
    U_R = unemp_flow_R + spec.beta * (find_R * next.W_R + (1.0 - find_R) * next.U_R)
    U_N = unemp_flow_N + spec.beta * (find_N * next.W_N + (1.0 - find_N) * next.U_N)

    return PeriodOutcome(
        value = ValueState(W_R, U_R, W_N, U_N),
        search_R = search_R,
        search_N = search_N,
        accept_R = accept_R,
        accept_N = accept_N,
        sep_R = sep_R,
        sep_N = sep_N,
        find_R = find_R,
        find_N = find_N,
        contact = contact,
        theta = theta,
        surplus_R = next_surplus_R,
        surplus_N = next_surplus_N,
    )
end

function stationary_values(env::Environment, sep::SeparationParams, spec::ModelSpec;
                           tol::Float64 = 1e-9, max_iter::Int = 20_000)
    guess = ValueState(
        utility(spec.wage - env.health_R, spec) / (1.0 - spec.beta),
        utility(outside_R(env, spec), spec) / (1.0 - spec.beta),
        utility(spec.wage - env.health_N, spec) / (1.0 - spec.beta),
        utility(outside_N(env, spec), spec) / (1.0 - spec.beta),
    )

    for _ in 1:max_iter
        out = bellman_update(env, guess, sep, spec)
        new = out.value
        diff = maximum(abs.((new.W_R - guess.W_R, new.U_R - guess.U_R,
                             new.W_N - guess.W_N, new.U_N - guess.U_N)))
        guess = new
        diff < tol && return guess
    end

    @warn "Stationary value iteration did not converge"
    return guess
end

function environment_for_period(t::Int, pre_env::Environment,
                                shock_env::Environment, spec::ModelSpec)
    return spec.shock_start <= t <= spec.shock_end ? shock_env : pre_env
end

function compute_value_path(pre_env::Environment, shock_env::Environment,
                            sep::SeparationParams, spec::ModelSpec;
                            anticipation::Symbol = :unanticipated)
    values = Vector{ValueState}(undef, spec.T + 1)
    pre_ss = stationary_values(pre_env, sep, spec)
    values[spec.T + 1] = pre_ss

    for t in spec.T:-1:1
        env_t = environment_for_period(t, pre_env, shock_env, spec)
        next_value = values[t + 1]
        if anticipation == :unanticipated && t < spec.shock_start
            next_value = pre_ss
        end
        values[t] = bellman_update(env_t, next_value, sep, spec).value
    end

    return values
end

function simulate_path(pre_env::Environment, shock_env::Environment,
                       sep::SeparationParams, spec::ModelSpec;
                       anticipation::Symbol = :unanticipated)
    values = compute_value_path(pre_env, shock_env, sep, spec; anticipation = anticipation)
    pre_ss = stationary_values(pre_env, sep, spec)

    rows = NamedTuple[]
    for t in 1:spec.T
        env_t = environment_for_period(t, pre_env, shock_env, spec)
        decision_next = anticipation == :unanticipated && t < spec.shock_start ?
            pre_ss : values[t + 1]
        out = bellman_update(env_t, decision_next, sep, spec)

        push!(rows, (
            week = t,
            in_shock = spec.shock_start <= t <= spec.shock_end,
            find_R = out.find_R,
            find_N = out.find_N,
            sep_R = out.sep_R,
            sep_N = out.sep_N,
            search_R = out.search_R,
            search_N = out.search_N,
            accept_R = out.accept_R,
            accept_N = out.accept_N,
            surplus_R = out.surplus_R,
            surplus_N = out.surplus_N,
            contact = out.contact,
            theta = out.theta,
        ))
    end

    return DataFrame(rows)
end

function shock_means(df::DataFrame)
    sub = df[df.in_shock .== true, :]
    return (
        find_R = mean(sub.find_R),
        find_N = mean(sub.find_N),
        sep_R = mean(sub.sep_R),
        sep_N = mean(sub.sep_N),
        search_R = mean(sub.search_R),
        search_N = mean(sub.search_N),
        accept_R = mean(sub.accept_R),
        accept_N = mean(sub.accept_N),
        surplus_R = mean(sub.surplus_R),
        surplus_N = mean(sub.surplus_N),
        contact = mean(sub.contact),
    )
end

function pre_environment(mu::Float64, kappa::Float64, spec::ModelSpec)
    return Environment(
        mu = mu,
        b_R = benefit_pre(spec),
        b_N = 0.0,
        health_R = 0.0,
        health_N = 0.0,
        kappa_R = kappa,
        kappa_N = kappa,
    )
end

function shock_environment(mu::Float64, kappa::Float64, spec::ModelSpec;
                           b_R::Float64, health_R::Float64, health_N::Float64)
    return Environment(
        mu = mu,
        b_R = b_R,
        b_N = 0.0,
        health_R = health_R,
        health_N = health_N,
        kappa_R = kappa,
        kappa_N = kappa,
    )
end

function fit_pre_separation(spec::ModelSpec, targets::Targets)
    env = pre_environment(0.1, 1.0, spec)
    g_N = sep_shape(flow_surplus_N(env, spec), spec)
    g_R = sep_shape(flow_surplus_R(env, spec), spec)
    amp = (targets.sep_R_pre - targets.sep_N_pre) / (g_R - g_N)
    base = targets.sep_N_pre - amp * g_N

    if amp < 0.0 || base < 0.0
        error("Pre-period separation moments imply invalid separation parameters.")
    end

    return SeparationParams(base = base, amp = amp)
end

function pre_fit_objective(x, sep::SeparationParams, spec::ModelSpec, targets::Targets)
    mu, kappa = exp(x[1]), exp(x[2])
    if !(0.001 <= mu <= 3.0 && 0.001 <= kappa <= 50.0)
        return 1e4 + abs(log(mu)) + abs(log(kappa))
    end

    env = pre_environment(mu, kappa, spec)
    values = stationary_values(env, sep, spec)
    out = bellman_update(env, values, sep, spec)
    return (out.find_N - targets.f_N_pre)^2 + (out.find_R - targets.f_R_pre)^2
end

function calibrate_pre(sep::SeparationParams, spec::ModelSpec, targets::Targets)
    starts = [[log(0.05), log(0.5)],
              [log(0.10), log(1.0)],
              [log(0.20), log(2.0)],
              [log(0.40), log(5.0)]]
    best_x = starts[1]
    best_loss = Inf

    for start in starts
        opt = optimize(x -> pre_fit_objective(x, sep, spec, targets),
                       start, NelderMead(), Optim.Options(iterations = 600))
        if Optim.minimum(opt) < best_loss
            best_loss = Optim.minimum(opt)
            best_x = Optim.minimizer(opt)
        end
    end

    mu, kappa = exp(best_x[1]), exp(best_x[2])
    env = pre_environment(mu, kappa, spec)
    values = stationary_values(env, sep, spec)
    out = bellman_update(env, values, sep, spec)

    return (
        env = env,
        mu = mu,
        kappa = kappa,
        loss = best_loss,
        f_N = out.find_N,
        f_R = out.find_R,
        sep_N = out.sep_N,
        sep_R = out.sep_R,
        search_N = out.search_N,
        search_R = out.search_R,
        accept_N = out.accept_N,
        accept_R = out.accept_R,
    )
end

function fit_health_N(sep::SeparationParams, pre_calib, spec::ModelSpec, targets::Targets)
    objective(h) = begin
        env = shock_environment(
            pre_calib.mu,
            pre_calib.kappa,
            spec;
            b_R = benefit_pre(spec),
            health_R = h,
            health_N = h,
        )
        (separation_rate(flow_surplus_N(env, spec), h, sep, spec) - targets.sep_N_post)^2
    end
    opt = optimize(objective, 0.0, 0.5)
    return Optim.minimizer(opt)
end

function post_N_fit_objective(log_mu, pre_env::Environment, kappa::Float64,
                              health_N::Float64, sep::SeparationParams,
                              spec::ModelSpec, targets::Targets)
    mu = exp(log_mu)
    if !(0.001 <= mu <= 3.0)
        return 1e4 + abs(log(mu))
    end

    shock_env = shock_environment(
        mu,
        kappa,
        spec;
        b_R = benefit_pre(spec),
        health_R = health_N,
        health_N = health_N,
    )
    means = shock_means(simulate_path(pre_env, shock_env, sep, spec))
    return (means.find_N - targets.f_N_post)^2
end

function calibrate_mu_post(pre_env::Environment, kappa::Float64, health_N::Float64,
                           sep::SeparationParams, spec::ModelSpec, targets::Targets)
    opt = optimize(
        x -> post_N_fit_objective(x, pre_env, kappa, health_N, sep, spec, targets),
        log(0.001), log(3.0),
    )
    mu = exp(Optim.minimizer(opt))
    shock_env = shock_environment(
        mu,
        kappa,
        spec;
        b_R = benefit_pre(spec),
        health_R = health_N,
        health_N = health_N,
    )
    means = shock_means(simulate_path(pre_env, shock_env, sep, spec))
    return (mu = mu, f_N = means.find_N)
end

function evaluate_scenario(r_health_premium::Float64, pre_calib,
                           mu_post::Float64, health_N::Float64,
                           sep::SeparationParams, spec::ModelSpec, targets::Targets)
    health_R = r_health_premium * health_N
    post_b = benefit_post(spec)

    common_covid_baseline_env = shock_environment(
        mu_post,
        pre_calib.kappa,
        spec;
        b_R = benefit_pre(spec),
        health_R = health_N,
        health_N = health_N,
    )
    common_covid_supplement_env = shock_environment(
        mu_post,
        pre_calib.kappa,
        spec;
        b_R = post_b,
        health_R = health_N,
        health_N = health_N,
    )
    differential_covid_baseline_env = shock_environment(
        mu_post,
        pre_calib.kappa,
        spec;
        b_R = benefit_pre(spec),
        health_R = health_R,
        health_N = health_N,
    )
    full_env = shock_environment(
        mu_post,
        pre_calib.kappa,
        spec;
        b_R = post_b,
        health_R = health_R,
        health_N = health_N,
    )
    normal_benefit_env = shock_environment(
        pre_calib.mu,
        pre_calib.kappa,
        spec;
        b_R = post_b,
        health_R = 0.0,
        health_N = 0.0,
    )

    common_covid_baseline = shock_means(simulate_path(pre_calib.env, common_covid_baseline_env, sep, spec))
    common_covid_supplement = shock_means(simulate_path(pre_calib.env, common_covid_supplement_env, sep, spec))
    differential_covid_baseline = shock_means(simulate_path(pre_calib.env, differential_covid_baseline_env, sep, spec))
    full = shock_means(simulate_path(pre_calib.env, full_env, sep, spec))
    normal_benefit = shock_means(simulate_path(pre_calib.env, normal_benefit_env, sep, spec))

    pre_gap_pp = 100 * (pre_calib.f_R - pre_calib.f_N)
    common_covid_baseline_gap_pp = 100 * (common_covid_baseline.find_R - common_covid_baseline.find_N)
    differential_covid_baseline_gap_pp = 100 * (differential_covid_baseline.find_R - differential_covid_baseline.find_N)
    full_gap_pp = 100 * (full.find_R - full.find_N)

    target_common_post_pp = 100 * targets.jfr_common_post_coef
    target_did_pp = 100 * targets.jfr_did_coef
    target_cell_did_pp = 100 * ((targets.f_R_post - targets.f_N_post) -
                                (targets.f_R_pre - targets.f_N_pre))
    model_full_did_pp = full_gap_pp - pre_gap_pp

    # This is not the paper's direct/common COVID effect. The common Post
    # coefficient is differenced out in the reduced-form DiD. This term is the
    # model-implied nonlinear interaction between a common COVID market shock
    # and the pre-existing R/N eligibility gap.
    structural_common_covid_x_baseline_gap_pp = common_covid_baseline_gap_pp - pre_gap_pp
    differential_work_disutility_pp = differential_covid_baseline_gap_pp - common_covid_baseline_gap_pp
    supplement_effect_pp = 100 * (full.find_R - differential_covid_baseline.find_R)
    predicted_jfr_rebound_if_supplement_removed_pp = -supplement_effect_pp
    supplement_effect_common_health_pp = 100 * (common_covid_supplement.find_R - common_covid_baseline.find_R)
    decomposition_residual_pp = model_full_did_pp -
        structural_common_covid_x_baseline_gap_pp -
        differential_work_disutility_pp -
        supplement_effect_pp

    covid_benefit_pp = supplement_effect_pp
    normal_benefit_pp = 100 * ((normal_benefit.find_R - normal_benefit.find_N) -
                               (pre_calib.f_R - pre_calib.f_N))
    log_benefit_denom = log(spec.benefit_multiplier)
    target_full_elasticity_logdid =
        (log(targets.f_R_post / targets.f_R_pre) -
         log(targets.f_N_post / targets.f_N_pre)) / log_benefit_denom
    model_full_elasticity_logdid =
        (log(full.find_R / pre_calib.f_R) -
         log(full.find_N / pre_calib.f_N)) / log_benefit_denom
    structural_common_covid_x_baseline_elasticity_logdid =
        (log(common_covid_baseline.find_R / pre_calib.f_R) -
         log(common_covid_baseline.find_N / pre_calib.f_N)) / log_benefit_denom
    differential_work_disutility_elasticity_logdid =
        (log(differential_covid_baseline.find_R / common_covid_baseline.find_R) -
         log(differential_covid_baseline.find_N / common_covid_baseline.find_N)) / log_benefit_denom
    supplement_effect_elasticity_covid_logdid =
        (log(full.find_R / differential_covid_baseline.find_R) -
         log(full.find_N / differential_covid_baseline.find_N)) / log_benefit_denom
    supplement_effect_common_health_elasticity_logdid =
        (log(common_covid_supplement.find_R / common_covid_baseline.find_R) -
         log(common_covid_supplement.find_N / common_covid_baseline.find_N)) / log_benefit_denom
    normal_market_benefit_elasticity_logdid =
        (log(normal_benefit.find_R / pre_calib.f_R) -
         log(normal_benefit.find_N / pre_calib.f_N)) / log_benefit_denom

    sep_target_common_post_pp = 100 * targets.sep_common_post_coef
    sep_target_did_pp = 100 * targets.sep_did_coef
    sep_target_cell_did_pp = 100 * ((targets.sep_R_post - targets.sep_N_post) -
                                    (targets.sep_R_pre - targets.sep_N_pre))
    sep_model_full_did_pp = 100 * ((full.sep_R - full.sep_N) -
                                   (pre_calib.sep_R - pre_calib.sep_N))
    sep_structural_common_covid_x_baseline_gap_pp =
        100 * ((common_covid_baseline.sep_R - common_covid_baseline.sep_N) -
               (pre_calib.sep_R - pre_calib.sep_N))
    sep_differential_work_disutility_pp =
        100 * ((differential_covid_baseline.sep_R - differential_covid_baseline.sep_N) -
               (common_covid_baseline.sep_R - common_covid_baseline.sep_N))
    sep_covid_benefit_pp = 100 * (full.sep_R - differential_covid_baseline.sep_R)
    predicted_sr_fall_if_supplement_removed_pp = -sep_covid_benefit_pp
    sep_normal_benefit_pp = 100 * (normal_benefit.sep_R - pre_calib.sep_R)

    return (
        r_health_premium = r_health_premium,
        post_R_fit_gap_pp = 100 * (full.find_R - targets.f_R_post),
        post_R_fit_ok = abs(full.find_R - targets.f_R_post) < 0.0005,
        mu_pre = pre_calib.mu,
        mu_post = mu_post,
        kappa = pre_calib.kappa,
        search_curvature = spec.search_curvature,
        offer_dispersion = spec.offer_dispersion,
        health_N = health_N,
        health_R = health_R,
        differential_health_shock = health_R - health_N,
        differential_health_shock_pct_wage = 100 * (health_R - health_N) / spec.wage,
        sep_base = sep.base,
        sep_amp = sep.amp,
        pre_loss = pre_calib.loss,
        f_N_pre = pre_calib.f_N,
        f_R_pre = pre_calib.f_R,
        f_N_post = full.find_N,
        f_R_full = full.find_R,
        f_R_common_covid_baseline = common_covid_baseline.find_R,
        f_N_common_covid_baseline = common_covid_baseline.find_N,
        f_R_differential_covid_baseline = differential_covid_baseline.find_R,
        f_R_covid_only = differential_covid_baseline.find_R,
        f_R_pre_benefit = normal_benefit.find_R,
        pre_gap_pp = pre_gap_pp,
        common_covid_baseline_gap_pp = common_covid_baseline_gap_pp,
        differential_covid_baseline_gap_pp = differential_covid_baseline_gap_pp,
        full_gap_pp = full_gap_pp,
        target_common_post_pp = target_common_post_pp,
        target_did_pp = target_did_pp,
        target_cell_did_pp = target_cell_did_pp,
        model_full_did_pp = model_full_did_pp,
        structural_common_covid_x_baseline_gap_pp = structural_common_covid_x_baseline_gap_pp,
        differential_work_disutility_pp = differential_work_disutility_pp,
        supplement_effect_pp = supplement_effect_pp,
        predicted_jfr_rebound_if_supplement_removed_pp = predicted_jfr_rebound_if_supplement_removed_pp,
        supplement_effect_common_health_pp = supplement_effect_common_health_pp,
        decomposition_residual_pp = decomposition_residual_pp,
        covid_market_benefit_pp = covid_benefit_pp,
        normal_market_benefit_pp = normal_benefit_pp,
        target_full_elasticity_logdid = target_full_elasticity_logdid,
        model_full_elasticity_logdid = model_full_elasticity_logdid,
        structural_common_covid_x_baseline_elasticity_logdid = structural_common_covid_x_baseline_elasticity_logdid,
        differential_work_disutility_elasticity_logdid = differential_work_disutility_elasticity_logdid,
        supplement_effect_elasticity_covid_logdid = supplement_effect_elasticity_covid_logdid,
        supplement_effect_common_health_elasticity_logdid = supplement_effect_common_health_elasticity_logdid,
        normal_market_benefit_elasticity_logdid = normal_market_benefit_elasticity_logdid,
        target_to_covid_benefit_ratio = target_did_pp / covid_benefit_pp,
        target_to_normal_benefit_ratio = target_did_pp / normal_benefit_pp,
        target_to_covid_benefit_elasticity_ratio =
            target_full_elasticity_logdid / supplement_effect_elasticity_covid_logdid,
        target_to_normal_benefit_elasticity_ratio =
            target_full_elasticity_logdid / normal_market_benefit_elasticity_logdid,
        sep_N_pre = pre_calib.sep_N,
        sep_R_pre = pre_calib.sep_R,
        sep_N_post = full.sep_N,
        sep_R_full = full.sep_R,
        sep_R_common_covid_baseline = common_covid_baseline.sep_R,
        sep_N_common_covid_baseline = common_covid_baseline.sep_N,
        sep_R_differential_covid_baseline = differential_covid_baseline.sep_R,
        sep_R_covid_only = differential_covid_baseline.sep_R,
        sep_target_common_post_pp = sep_target_common_post_pp,
        sep_target_did_pp = sep_target_did_pp,
        sep_target_cell_did_pp = sep_target_cell_did_pp,
        sep_model_full_did_pp = sep_model_full_did_pp,
        sep_structural_common_covid_x_baseline_gap_pp = sep_structural_common_covid_x_baseline_gap_pp,
        sep_differential_work_disutility_pp = sep_differential_work_disutility_pp,
        sep_covid_benefit_pp = sep_covid_benefit_pp,
        predicted_sr_fall_if_supplement_removed_pp = predicted_sr_fall_if_supplement_removed_pp,
        sep_normal_benefit_pp = sep_normal_benefit_pp,
        search_N_pre = pre_calib.search_N,
        search_R_pre = pre_calib.search_R,
        search_N_post = full.search_N,
        search_R_full = full.search_R,
        search_R_common_covid_baseline = common_covid_baseline.search_R,
        search_R_differential_covid_baseline = differential_covid_baseline.search_R,
        search_R_covid_only = differential_covid_baseline.search_R,
        accept_N_pre = pre_calib.accept_N,
        accept_R_pre = pre_calib.accept_R,
        accept_N_post = full.accept_N,
        accept_R_full = full.accept_R,
        accept_R_common_covid_baseline = common_covid_baseline.accept_R,
        accept_R_differential_covid_baseline = differential_covid_baseline.accept_R,
        accept_R_covid_only = differential_covid_baseline.accept_R,
        surplus_N_post = full.surplus_N,
        surplus_R_full = full.surplus_R,
        surplus_R_common_covid_baseline = common_covid_baseline.surplus_R,
        surplus_R_differential_covid_baseline = differential_covid_baseline.surplus_R,
        surplus_R_covid_only = differential_covid_baseline.surplus_R,
        contact_post = full.contact,
    )
end

function calibration_for_curvature(search_curvature::Float64,
                                   base_spec::ModelSpec,
                                   targets::Targets)
    spec = copy_spec(base_spec; search_curvature = search_curvature)
    sep = fit_pre_separation(spec, targets)
    pre_calib = calibrate_pre(sep, spec, targets)
    health_N = fit_health_N(sep, pre_calib, spec, targets)
    post_calib = calibrate_mu_post(pre_calib.env, pre_calib.kappa,
                                   health_N, sep, spec, targets)

    return (
        spec = spec,
        sep = sep,
        pre_calib = pre_calib,
        health_N = health_N,
        post_calib = post_calib,
    )
end

function calibrate_search_curvature_for_health(r_health_premium::Float64,
                                               curvature_calibrations,
                                               base_spec::ModelSpec,
                                               targets::Targets)
    rows = NamedTuple[]
    best = nothing
    best_loss = Inf
    last_out = nothing
    bracket = nothing

    for cal in curvature_calibrations
        out = evaluate_scenario(
            r_health_premium,
            cal.pre_calib,
            cal.post_calib.mu,
            cal.health_N,
            cal.sep,
            cal.spec,
            targets,
        )
        loss = (out.f_R_full - targets.f_R_post)^2
        push!(rows, merge(out, (
            curvature_fit_gap_pp = out.post_R_fit_gap_pp,
            curvature_fit_ok = abs(out.post_R_fit_gap_pp) < 0.05,
        )))
        if loss < best_loss
            best, best_loss = out, loss
        end
        if last_out !== nothing &&
           sign(last_out.post_R_fit_gap_pp) != sign(out.post_R_fit_gap_pp)
            bracket = (last_out, out)
        end
        last_out = out
    end

    if bracket !== nothing
        lo, hi = bracket
        weight = abs(lo.post_R_fit_gap_pp) /
                 (abs(lo.post_R_fit_gap_pp) + abs(hi.post_R_fit_gap_pp))
        eta = lo.search_curvature +
              weight * (hi.search_curvature - lo.search_curvature)
        cal = calibration_for_curvature(eta, base_spec, targets)
        out = evaluate_scenario(
            r_health_premium,
            cal.pre_calib,
            cal.post_calib.mu,
            cal.health_N,
            cal.sep,
            cal.spec,
            targets,
        )
        loss = (out.f_R_full - targets.f_R_post)^2
        push!(rows, merge(out, (
            curvature_fit_gap_pp = out.post_R_fit_gap_pp,
            curvature_fit_ok = abs(out.post_R_fit_gap_pp) < 0.05,
        )))
        if loss < best_loss
            best, best_loss = out, loss
        end
    end

    best = merge(best, (
        curvature_fit_gap_pp = best.post_R_fit_gap_pp,
        curvature_fit_ok = abs(best.post_R_fit_gap_pp) < 0.05,
    ))

    return best, DataFrame(rows)
end

function literature_moments()
    return DataFrame(
        Moment = [
            "JFR: NZ pre",
            "JFR: AUS pre",
            "JFR: NZ post",
            "JFR: AUS post",
            "Separation: NZ pre",
            "Separation: AUS pre",
            "Separation: NZ post",
            "Separation: AUS post",
            "Matching elasticity wrt unemployed/jobseekers",
            "Matching elasticity wrt vacancies",
            "Matching efficiency level",
            "Job-finding probability and labour-market tightness",
            "Monthly labour-force gross flows during COVID",
            "Annual job mobility",
            "Annual left/lost job and retrenchment rates",
            "Broad annual separation rate",
            "Reservation-wage/acceptance margin",
            "Potential workers / marginal attachment",
            "Coronavirus Supplement size",
            "Mutual-obligation timing",
            "Endogenous search effort maps concavely into offers",
            "Search-effort response to unemployment benefits",
            "Health/work-disutility difference by eligibility group",
        ],
        CurrentUseIn13 = [
            "Direct calibration target",
            "Direct calibration target",
            "Direct calibration target",
            "Direct calibration target",
            "Direct calibration target",
            "Direct calibration target",
            "Direct calibration target",
            "Diagnostic target; compared to model-implied separation DiD",
            "Model parameter: match_elasticity",
            "Implicit counterpart to match_elasticity under CRS normalization",
            "Benchmark for matching efficiency, not a direct level target",
            "Motivates using mu_post to fit NZ post JFR",
            "Not directly used; useful validation for monthly transitions",
            "Not directly used; informs separation/job-to-job sensitivity",
            "Not directly used; informs separation decomposition",
            "Not directly used; informs plausible separation range",
            "Implemented structurally through accept_prob; not yet calibrated to this moment",
            "Not directly used; informs sample selection and NILF transitions",
            "Used to set benefit_multiplier and supplement shock",
            "Not directly used; candidate shifter for search-cost/mutual-obligation channel",
            "Implemented through p_offer = 1 - exp(-contact * search)",
            "Not directly used; candidate prior for kappa/search elasticity",
            "Varied as main sensitivity parameter",
        ],
        CurrentValue = [
            0.1002,
            0.0868,
            0.0823,
            0.0518,
            0.0480,
            0.0484,
            0.0550,
            0.0926,
            0.42,
            0.58,
            missing,
            missing,
            missing,
            missing,
            missing,
            missing,
            missing,
            missing,
            2.0,
            missing,
            missing,
            missing,
            missing,
        ],
        RecommendedUse = [
            "Keep as primary empirical moment from the paper/sample.",
            "Keep as primary empirical moment from the paper/sample.",
            "Keep as primary empirical moment from the paper/sample.",
            "Keep as primary empirical moment from the paper/sample.",
            "Keep as primary empirical moment from the paper/sample.",
            "Keep as primary empirical moment from the paper/sample.",
            "Keep as primary empirical moment from the paper/sample.",
            "Use to discipline whether assumed differential health shocks are plausible.",
            "Use as baseline and sensitivity range for match_elasticity.",
            "Use as cross-check because Australian estimates imply vacancies matter more than unemployed stocks.",
            "Use only to benchmark changes in mu; model mu is normalized.",
            "Estimate from ABS flows and vacancies for the exact population/time window.",
            "Use to validate model-implied JFR and separation flows in COVID months.",
            "Use as an upper-level benchmark for voluntary/job-to-job mobility.",
            "Use to distinguish quits/job-to-job changes from involuntary loss.",
            "Use as broad external check; not job-loser-specific weekly hazard.",
            "Calibrate offer_threshold/offer_dispersion once a comparable acceptance/reservation moment is chosen.",
            "Use to decide whether NILF transitions or marginal attachment should be added.",
            "Use as known policy shock; do not estimate this from the DiD.",
            "Use as institutional timing for search-cost or compliance shock, not as benefit generosity.",
            "Use as the core endogenous-search probability mapping; search_curvature is calibrated across the health-shock grid.",
            "International fallback only; replace with Australian admin/search-report moment if available.",
            "Currently missing; estimate or bound using health, lockdown, local outbreak, household, financial-stress, and welfare-exclusion variables. Do not use occupation/industry here because the study estimates already control for them.",
        ],
        CentralValue = [
            0.1002,
            0.0868,
            0.0823,
            0.0518,
            0.0480,
            0.0484,
            0.0550,
            0.0926,
            0.326,
            0.450,
            0.841,
            missing,
            missing,
            0.08,
            0.017,
            0.20,
            missing,
            1.9e6,
            2.0,
            missing,
            missing,
            -1.9,
            missing,
        ],
        Low = [
            missing, missing, missing, missing,
            missing, missing, missing, missing,
            0.33, 0.45, missing, missing, missing,
            0.075, 0.017, 0.08, missing, missing,
            2.0, missing, missing, -2.2, 1.0,
        ],
        High = [
            missing, missing, missing, missing,
            missing, missing, missing, missing,
            0.50, 0.67, missing, missing, missing,
            0.096, 0.017, 0.25, missing, missing,
            2.0, missing, missing, -1.6, 6.5,
        ],
        Availability = [
            "Paper/sample estimate",
            "Paper/sample estimate",
            "Paper/sample estimate",
            "Paper/sample estimate",
            "Paper/sample estimate",
            "Paper/sample estimate",
            "Paper/sample estimate",
            "Paper/sample estimate",
            "Australian literature",
            "Australian literature",
            "Australian literature",
            "Australian data framework available; sample-specific moment should be estimated",
            "Australian data article; exact sample-specific monthly flows should be estimated",
            "Australian official statistic",
            "Australian official statistic",
            "Australian literature, broad employee-separation concept",
            "Australian literature, older and not COVID-specific",
            "Australian official statistic; not necessarily same sample",
            "Australian institutional/policy fact",
            "Australian institutional/policy fact",
            "Structural design from search literature",
            "International literature fallback",
            "Missing; estimate from administrative and exposure data",
        ],
        Source = [
            "COVID Labour Supply NZ/AUS working paper, Table 3 reduced-form regression cell",
            "COVID Labour Supply NZ/AUS working paper, Table 3 reduced-form regression cell",
            "COVID Labour Supply NZ/AUS working paper, Table 3 reduced-form regression cell",
            "COVID Labour Supply NZ/AUS working paper, Table 3 reduced-form regression cell",
            "COVID Labour Supply NZ/AUS working paper, Table 4",
            "COVID Labour Supply NZ/AUS working paper, Table 4",
            "COVID Labour Supply NZ/AUS working paper, Table 4",
            "COVID Labour Supply NZ/AUS working paper, Table 4",
            "Lake, Shamiri, Sharma and Bialowas 2024, ABS/RBA conference paper",
            "Lake, Shamiri, Sharma and Bialowas 2024, ABS/RBA conference paper",
            "Lake, Shamiri, Sharma and Bialowas 2024, ABS/RBA conference paper",
            "Australian Treasury Round Up 2024, matching across skills and regions",
            "ABS, Flows into and out of employment and unemployment",
            "ABS, Job mobility February 2024",
            "ABS, Job mobility February 2024",
            "Sila 2019 OECD/HILDA job displacement paper",
            "RBA RDP 1999-02, Reservation Wages and Duration of Unemployment",
            "ABS, Potential workers February 2024",
            "Parliamentary Library/PBO summaries of JobSeeker COVID measures",
            "Parliamentary Library, Changes to COVID-19 social security measures",
            "Shimer 2004, Search Intensity",
            "Krueger and Mueller 2010, US time-use evidence",
            "No clean Australian moment found",
        ],
        URL = [
            "local paper: COVID_Labour_Supply__NZ_Aus_ (9).pdf",
            "local paper: COVID_Labour_Supply__NZ_Aus_ (9).pdf",
            "local paper: COVID_Labour_Supply__NZ_Aus_ (9).pdf",
            "local paper: COVID_Labour_Supply__NZ_Aus_ (9).pdf",
            "local paper: COVID_Labour_Supply__NZ_Aus_ (9).pdf",
            "local paper: COVID_Labour_Supply__NZ_Aus_ (9).pdf",
            "local paper: COVID_Labour_Supply__NZ_Aus_ (9).pdf",
            "local paper: COVID_Labour_Supply__NZ_Aus_ (9).pdf",
            "https://www.rba.gov.au/publications/other-confs/abs-and-rba-joint-conferences/2024/pdf/abs-rba-conference-2024-lake-shamiri-sharma-bialowas.pdf",
            "https://www.rba.gov.au/publications/other-confs/abs-and-rba-joint-conferences/2024/pdf/abs-rba-conference-2024-lake-shamiri-sharma-bialowas.pdf",
            "https://www.rba.gov.au/publications/other-confs/abs-and-rba-joint-conferences/2024/pdf/abs-rba-conference-2024-lake-shamiri-sharma-bialowas.pdf",
            "https://treasury.gov.au/sites/default/files/2024-03/p2024-495252-full-report_0.pdf",
            "https://www.abs.gov.au/articles/flows-and-out-employment-and-unemployment",
            "https://www.abs.gov.au/statistics/labour/jobs/job-mobility/feb-2024",
            "https://www.abs.gov.au/statistics/labour/jobs/job-mobility/feb-2024",
            "https://www.oecd.org/content/dam/oecd/en/publications/reports/2019/02/job-displacement-in-australia_531c1c06/eccc0b28-en.pdf",
            "https://www.rba.gov.au/publications/rdp/1999/1999-02.html",
            "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/potential-workers/feb-2024",
            "https://www.aph.gov.au/About_Parliament/Parliamentary_departments/Parliamentary_Library/Research/FlagPost/2020/March/New_coronavirus_supplement",
            "https://www.aph.gov.au/About_Parliament/Parliamentary_departments/Parliamentary_Library/Research/Research_Papers/2020-21/ChangesCOVID-19SocialSecurity",
            "https://home.uchicago.edu/~shimer/wp/intensity.pdf",
            "https://www.sciencedirect.com/science/article/pii/S0047272709001625",
            "",
        ],
        Notes = [
            "These are the JFR targets already embedded in Targets().",
            "These are the JFR targets already embedded in Targets().",
            "These are the JFR targets already embedded in Targets().",
            "These are the JFR targets already embedded in Targets().",
            "Used to fit the pre separation function.",
            "Used to fit the pre separation function.",
            "Used to infer the common health/work-disutility component through NZ post separations.",
            "Used as an external plausibility check for R differential health/work-disutility assumptions.",
            "Paper reports unemployed elasticity about 0.326; current model uses 0.42 as a CRS-normalized baseline from previous 12b convention.",
            "Paper reports vacancy elasticity 0.450; useful because the sum of reported elasticities is below one.",
            "Reported efficiency is 0.841 over the study period; not directly comparable to weekly model mu.",
            "Treasury defines matching probability as hires from job searchers divided by previous-period job searchers and tightness as vacancies/searchers.",
            "ABS article reports May-June 2020 unemployed-to-employed flow of 21.5% and April-May 2020 of 19.3%; adapt carefully to sample frequency.",
            "ABS reports 8% job mobility in 2024 and 9.6% in 2023; useful for voluntary/job-to-job margin, not direct job-loss separations.",
            "ABS reports 2.1 million left/lost a job and a 1.7% annual retrenchment rate in year ending Feb 2024.",
            "OECD/HILDA reports all moves out of jobs around 20% annually in Australia circa 2015/2016.",
            "RBA finds reservation wages had little effect on duration; few offers appear more binding, so acceptance margin should be disciplined cautiously.",
            "ABS reports 1.9 million potential workers in Feb 2024; use to think about participation/NILF extension.",
            "The supplement almost doubled the maximum JobSeeker payment, so the rate change should remain treated as known.",
            "Mutual obligation suspension/reintroduction is a separate policy shock that may alter search costs independently of payment generosity.",
            "Shimer uses p = 1 - (1 - mu(theta))^s for multiple applications; this script uses the continuous approximation p = 1 - exp(-contact * s).",
            "Fallback only; use Australian administrative search/reporting data if available.",
            "This is the central missing moment for whether DiD overstates or understates the benefit-only effect.",
        ],
    )
end

function run_health_grid(; premiums = collect(range(1.0, 6.5, length = 23)),
                         spec = ModelSpec(), targets = Targets())
    curvature_grid = sort(unique(vcat(
        collect(range(3.0, 16.0, length = 14)),
        collect(range(18.0, 60.0, length = 8)),
        [5.0, 7.0, 10.0, 12.0, 24.0, 40.0],
    )))
    curvature_calibrations = [
        calibration_for_curvature(eta, spec, targets) for eta in curvature_grid
    ]

    summaries = NamedTuple[]
    paths = DataFrame()
    for premium in premiums
        best, path = calibrate_search_curvature_for_health(
            premium, curvature_calibrations, spec, targets)
        path.assumed_r_health_premium .= premium
        paths = vcat(paths, path; cols = :union)
        push!(summaries, best)
    end

    summary = DataFrame(summaries)
    sort!(summary, :r_health_premium)
    reference = curvature_calibrations[argmin(abs.([cal.spec.search_curvature - spec.search_curvature
                                                    for cal in curvature_calibrations]))]
    return summary, paths, reference.pre_calib, reference.post_calib, reference.sep
end

function july_reduction_predictions(summary::DataFrame, pre_calib, post_calib,
                                    sep::SeparationParams, spec::ModelSpec;
                                    reduction_fraction::Float64 = 250 / 550,
                                    targets::Targets = Targets())
    rows = NamedTuple[]

    for r in eachrow(summary)
        cal = calibration_for_curvature(r.search_curvature, spec, targets)
        full_env = shock_environment(
            cal.post_calib.mu,
            cal.pre_calib.kappa,
            cal.spec;
            b_R = benefit_post(cal.spec),
            health_R = r.health_R,
            health_N = r.health_N,
        )
        reduced_env = shock_environment(
            cal.post_calib.mu,
            cal.pre_calib.kappa,
            cal.spec;
            b_R = benefit_at_supplement_fraction(cal.spec, reduction_fraction),
            health_R = r.health_R,
            health_N = r.health_N,
        )
        full = shock_means(simulate_path(cal.pre_calib.env, full_env, cal.sep, cal.spec))
        reduced = shock_means(simulate_path(cal.pre_calib.env, reduced_env, cal.sep, cal.spec))

        push!(rows, (
            differential_health_shock_pct_wage = r.differential_health_shock_pct_wage,
            initial_supplement_fraction = 1.0,
            reduced_supplement_fraction = reduction_fraction,
            jfr_R_full_supplement = full.find_R,
            jfr_R_reduced_supplement = reduced.find_R,
            predicted_R_jfr_change_pp = 100 * (reduced.find_R - full.find_R),
            predicted_DiD_change_pp =
                100 * ((reduced.find_R - reduced.find_N) - (full.find_R - full.find_N)),
            sr_R_full_supplement = full.sep_R,
            sr_R_reduced_supplement = reduced.sep_R,
            predicted_R_sr_change_pp = 100 * (reduced.sep_R - full.sep_R),
        ))
    end

    return DataFrame(rows)
end

function plot_outputs(summary::DataFrame, paths::DataFrame, july_predictions::DataFrame)
    p1 = plot(summary.r_health_premium, summary.search_curvature,
        label = "Required search curvature", lw = 2,
        xlabel = "R health/work-disutility premium over N",
        ylabel = "Search curvature eta",
        title = "Search Curvature Needed to Fit R Job Finding")
    savefig(p1, "13_required_search_curvature.png")

    p2 = plot(summary.r_health_premium, summary.target_did_pp,
        label = "Empirical JFR DiD", lw = 2,
        xlabel = "R health/work-disutility premium over N",
        ylabel = "Job-finding response (p.p.)",
        title = "Bellman Model: Named JFR Decomposition")
    plot!(p2, summary.r_health_premium, summary.model_full_did_pp,
        label = "Model full DiD", lw = 2, ls = :dash)
    plot!(p2, summary.r_health_premium, summary.supplement_effect_pp,
        label = "Australian supplement effect", lw = 2, ls = :dot)
    plot!(p2, summary.r_health_premium, summary.differential_work_disutility_pp,
        label = "Differential work-disutility effect", lw = 2, ls = :dashdot)
    plot!(p2, summary.r_health_premium, summary.structural_common_covid_x_baseline_gap_pp,
        label = "Structural common COVID x baseline gap", lw = 2, ls = :dash)
    savefig(p2, "13_named_jfr_decomposition.png")

    p2b = plot(summary.r_health_premium, summary.target_did_pp,
        label = "Empirical JFR DiD", lw = 2,
        xlabel = "R health/work-disutility premium over N",
        ylabel = "Job-finding response (p.p.)",
        title = "Supplement Effect: COVID vs No-COVID Markets")
    plot!(p2b, summary.r_health_premium, summary.supplement_effect_pp,
        label = "Supplement effect in COVID market", lw = 2, ls = :dash)
    plot!(p2b, summary.r_health_premium, summary.supplement_effect_common_health_pp,
        label = "Supplement effect with common health shock", lw = 2, ls = :dashdot)
    plot!(p2b, summary.r_health_premium, summary.normal_market_benefit_pp,
        label = "Benefit effect without COVID", lw = 2, ls = :dot)
    savefig(p2b, "13_jfr_pp_effects_by_health_premium.png")

    p2c = plot(summary.r_health_premium, summary.target_full_elasticity_logdid,
        label = "Empirical DiD elasticity", lw = 2,
        xlabel = "R health/work-disutility premium over N",
        ylabel = "Log-DiD elasticity wrt benefits",
        title = "Supplemental Elasticities")
    plot!(p2c, summary.r_health_premium, summary.model_full_elasticity_logdid,
        label = "Model full DiD elasticity", lw = 2, ls = :dash)
    plot!(p2c, summary.r_health_premium, summary.supplement_effect_elasticity_covid_logdid,
        label = "Supplement elasticity in COVID market", lw = 2, ls = :dot)
    plot!(p2c, summary.r_health_premium, summary.normal_market_benefit_elasticity_logdid,
        label = "Supplement elasticity without COVID", lw = 2, ls = :dashdot)
    plot!(p2c, summary.r_health_premium,
        summary.differential_work_disutility_elasticity_logdid,
        label = "Differential work-disutility pseudo-elasticity", lw = 2, ls = :dash)
    savefig(p2c, "13_supplemental_elasticities.png")

    p2d = plot(summary.r_health_premium,
        abs.(summary.target_to_covid_benefit_elasticity_ratio),
        label = "|Empirical DiD / COVID-market supplement elasticity|", lw = 2,
        xlabel = "R health/work-disutility premium over N",
        ylabel = "Absolute ratio",
        title = "Supplemental Elasticity Ratios")
    plot!(p2d, summary.r_health_premium,
        abs.(summary.target_to_normal_benefit_elasticity_ratio),
        label = "|Empirical DiD / no-COVID supplement elasticity|", lw = 2, ls = :dash)
    hline!(p2d, [1.0], label = "Equal magnitude", color = :black, ls = :dot)
    savefig(p2d, "13_supplemental_elasticity_ratios.png")

    p2e = plot(summary.differential_health_shock_pct_wage, summary.target_full_elasticity_logdid,
        label = "Empirical DiD elasticity", lw = 2,
        xlabel = "Additional R health/work-disutility shock (% of weekly wage)",
        ylabel = "Log-DiD elasticity wrt benefits",
        title = "Elasticities by Differential Health Shock")
    plot!(p2e, summary.differential_health_shock_pct_wage,
        summary.supplement_effect_elasticity_covid_logdid,
        label = "Supplement elasticity in COVID market", lw = 2, ls = :dash)
    plot!(p2e, summary.differential_health_shock_pct_wage,
        summary.normal_market_benefit_elasticity_logdid,
        label = "Supplement elasticity without COVID", lw = 2, ls = :dot)
    plot!(p2e, summary.differential_health_shock_pct_wage,
        summary.differential_work_disutility_elasticity_logdid,
        label = "Differential work-disutility pseudo-elasticity", lw = 2, ls = :dashdot)
    savefig(p2e, "13_elasticities_by_differential_health_shock.png")

    p2f = plot(summary.differential_health_shock_pct_wage,
        summary.predicted_jfr_rebound_if_supplement_removed_pp,
        label = "Predicted AUS JFR rebound", lw = 2,
        xlabel = "Additional R health/work-disutility shock (% of weekly wage)",
        ylabel = "Predicted response to supplement removal (p.p.)",
        title = "Out-of-Sample Prediction: Removing the Supplement")
    plot!(p2f, summary.differential_health_shock_pct_wage,
        summary.predicted_sr_fall_if_supplement_removed_pp,
        label = "Predicted AUS separation-rate fall", lw = 2, ls = :dash)
    hline!(p2f, [0.0], label = "", color = :black, alpha = 0.35)
    savefig(p2f, "13_predicted_removal_effects.png")

    p2g = plot(july_predictions.differential_health_shock_pct_wage,
        july_predictions.predicted_DiD_change_pp,
        label = "Predicted JFR DiD change", lw = 2,
        xlabel = "Additional R health/work-disutility shock (% of weekly wage)",
        ylabel = "Predicted response to 550 -> 250 supplement (p.p.)",
        title = "Out-of-Sample Prediction: July Supplement Reduction")
    plot!(p2g, july_predictions.differential_health_shock_pct_wage,
        july_predictions.predicted_R_sr_change_pp,
        label = "Predicted AUS separation-rate change", lw = 2, ls = :dash)
    hline!(p2g, [0.0], label = "", color = :black, alpha = 0.35)
    savefig(p2g, "13_predicted_july_550_to_250.png")

    p3 = plot(summary.r_health_premium, summary.sep_target_did_pp,
        label = "Empirical separation DiD", lw = 2,
        xlabel = "R health/work-disutility premium over N",
        ylabel = "Separation response (p.p.)",
        title = "Bellman Model: Endogenous Separations")
    plot!(p3, summary.r_health_premium, summary.sep_model_full_did_pp,
        label = "Model full separation DiD", lw = 2, ls = :dash)
    plot!(p3, summary.r_health_premium, summary.sep_differential_work_disutility_pp,
        label = "Differential work-disutility effect", lw = 2, ls = :dashdot)
    plot!(p3, summary.r_health_premium, summary.sep_covid_benefit_pp,
        label = "Supplement contribution in COVID market", lw = 2, ls = :dot)
    savefig(p3, "13_separation_pp_effects_by_health_premium.png")

    p4 = plot(summary.r_health_premium, summary.search_R_full,
        label = "R search, full COVID", lw = 2,
        xlabel = "R health/work-disutility premium over N",
        ylabel = "Margin value",
        title = "Search and Acceptance Margins from Bellman Values")
    plot!(p4, summary.r_health_premium, summary.search_R_covid_only,
        label = "R search, COVID-only", lw = 2, ls = :dash)
    plot!(p4, summary.r_health_premium, summary.accept_R_full,
        label = "R acceptance, full COVID", lw = 2, ls = :dot)
    plot!(p4, summary.r_health_premium, summary.accept_R_covid_only,
        label = "R acceptance, COVID-only", lw = 2, ls = :dashdot)
    savefig(p4, "13_search_acceptance_margins.png")

    p5 = scatter(paths.assumed_r_health_premium, paths.search_curvature,
        marker_z = 100 .* (paths.f_R_full .- Targets().f_R_post),
        xlabel = "R health/work-disutility premium over N",
        ylabel = "Search curvature eta",
        colorbar_title = "R JFR fit gap p.p.",
        title = "Calibration Surface: R Job-Finding Fit",
        label = "")
    savefig(p5, "13_calibration_surface.png")
end

function print_summary(summary::DataFrame)
    println("\nBellman three-margin model: selected health-premium assumptions")
    select_rows = [1, cld(nrow(summary), 2), nrow(summary)]
    for i in select_rows
        r = summary[i, :]
        @printf("\nR health premium %.2f\n", r.r_health_premium)
        @printf("  paper JFR common Post:         %.2f p.p. (differenced out of DiD)\n",
                r.target_common_post_pp)
        @printf("  paper JFR Aus x Post DiD:      %.2f p.p.\n", r.target_did_pp)
        @printf("  required search curvature eta: %.3f\n", r.search_curvature)
        @printf("  model R post JFR:              %.2f%% (target %.2f%%)\n",
                100r.f_R_full, 100Targets().f_R_post)
        @printf("  COVID-market benefit effect:   %.2f p.p.\n", r.covid_market_benefit_pp)
        @printf("  predicted JFR rebound if removed: %.2f p.p.\n",
                r.predicted_jfr_rebound_if_supplement_removed_pp)
        @printf("  predicted SR fall if removed:  %.2f p.p.\n",
                r.predicted_sr_fall_if_supplement_removed_pp)
        @printf("  no-COVID benefit effect:       %.2f p.p.\n", r.normal_market_benefit_pp)
        @printf("  empirical / COVID benefit elasticity: %.3f / %.3f\n",
                r.target_full_elasticity_logdid,
                r.supplement_effect_elasticity_covid_logdid)
        @printf("  structural common COVID x baseline gap: %.2f p.p.\n",
                r.structural_common_covid_x_baseline_gap_pp)
        @printf("  differential work disutility:  %.2f p.p.\n", r.differential_work_disutility_pp)
        @printf("  model separation DiD:          %.2f p.p. (target %.2f p.p.)\n",
                r.sep_model_full_did_pp, r.sep_target_did_pp)
        @printf("  R search full / covid-only:    %.3f / %.3f\n",
                r.search_R_full, r.search_R_covid_only)
        @printf("  R accept full / covid-only:    %.3f / %.3f\n",
                r.accept_R_full, r.accept_R_covid_only)
    end
end

function run_model()
    spec = ModelSpec()
    targets = Targets()
    summary, paths, pre_calib, post_calib, sep = run_health_grid(spec = spec, targets = targets)
    july_predictions = july_reduction_predictions(summary, pre_calib, post_calib, sep, spec)

    CSV.write("13_three_margin_summary.csv", summary)
    CSV.write("13_decomposition_by_work_disutility.csv", summary)
    CSV.write("13_three_margin_calibration_paths.csv", paths)
    CSV.write("13_predicted_july_550_to_250.csv", july_predictions)
    CSV.write("13_literature_moments.csv", literature_moments())
    plot_outputs(summary, paths, july_predictions)
    print_summary(summary)

    println("\nCalibration checks:")
    @printf("  pre N/R JFR:       %.2f%% / %.2f%%\n", 100pre_calib.f_N, 100pre_calib.f_R)
    @printf("  pre N/R sep:       %.2f%% / %.2f%%\n", 100pre_calib.sep_N, 100pre_calib.sep_R)
    @printf("  post N JFR target: %.2f%%\n", 100post_calib.f_N)
    @printf("  sep base / amp:    %.4f / %.4f\n", sep.base, sep.amp)

    println("\nSaved outputs:")
    println("  13_three_margin_summary.csv")
    println("  13_decomposition_by_work_disutility.csv")
    println("  13_three_margin_calibration_paths.csv")
    println("  13_predicted_july_550_to_250.csv")
    println("  13_literature_moments.csv")
    println("  13_required_search_curvature.png")
    println("  13_named_jfr_decomposition.png")
    println("  13_jfr_pp_effects_by_health_premium.png")
    println("  13_supplemental_elasticities.png")
    println("  13_supplemental_elasticity_ratios.png")
    println("  13_elasticities_by_differential_health_shock.png")
    println("  13_predicted_removal_effects.png")
    println("  13_predicted_july_550_to_250.png")
    println("  13_separation_pp_effects_by_health_premium.png")
    println("  13_search_acceptance_margins.png")
    println("  13_calibration_surface.png")

    return (summary = summary, paths = paths, july = july_predictions, pre = pre_calib,
            post = post_calib, sep = sep)
end

if abspath(PROGRAM_FILE) == @__FILE__
    run_model()
end
