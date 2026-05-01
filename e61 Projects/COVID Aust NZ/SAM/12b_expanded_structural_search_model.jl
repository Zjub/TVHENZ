# 12b_expanded_structural_search_model.jl
#
# Expanded diagnostics for the two-type structural search model.
# This script keeps 12_structural_search_model.jl as the model core and adds:
# - the marginal benefit effect in the COVID environment: Full shock - COVID-only;
# - a literature/moment table for calibration targets and missing Australian moments;
# - one-way and paired sensitivity plots to show when the DiD over/understates the
#   model-implied labour-supply response.

using CSV
using DataFrames
using Optim
using Plots
using Printf
using Statistics

include("12_structural_search_model.jl")

const TARGET_PRE_N = 0.1000
const TARGET_PRE_R = 0.0866
const TARGET_POST_N = 0.0821
const TARGET_POST_R = 0.0312

target_full_did_pp() = 100 * ((TARGET_POST_R - TARGET_POST_N) - (TARGET_PRE_R - TARGET_PRE_N))
target_full_logdid_elasticity(; benefit_multiplier = 2.0) =
    (log(TARGET_POST_R / TARGET_PRE_R) - log(TARGET_POST_N / TARGET_PRE_N)) / log(benefit_multiplier)
logdid_elasticity_from_means(pre_R, pre_N, post_R, post_N, log_denom) =
    (log(post_R / pre_R) - log(post_N / pre_N)) / log_denom

function build_spec(; match_elasticity = 0.42, delta = 0.01, risk_aversion = 0.0)
    return ModelSpec(
        beta = 0.99,
        delta = delta,
        recipient_share = 0.5,
        match_elasticity = match_elasticity,
        bargaining_power = 0.5,
        vacancy_cost = 1.0,
        exogenous_wage = true,
        wage = 0.55,
        risk_aversion = risk_aversion,
        max_search = 1.0,
        probability_cap = 0.999,
        flow_floor = 1e-8,
        T = 60,
        shock_start = 12,
        shock_end = 36,
    )
end

function model_environments(spec::ModelSpec;
                            benefit_multiplier = 2.0,
                            benefit_weight = 1.0,
                            leisure_R_post_multiplier = 1.0,
                            leisure_N_post_multiplier = 1.0)
    wage = spec.wage
    actual_benefit_R_pre = 0.28 * wage
    actual_benefit_R_post = benefit_multiplier * actual_benefit_R_pre
    benefit_R_pre = benefit_weight * actual_benefit_R_pre
    benefit_R_post = benefit_weight * actual_benefit_R_post

    leisure_R_pre = 0.10 * wage
    leisure_N_pre = 0.10 * wage

    pre_template = Environment(
        mu = 0.4,
        y = 1.0,
        b_R = benefit_R_pre,
        b_N = 0.0,
        leisure_R = leisure_R_pre,
        leisure_N = leisure_N_pre,
        kappa_R = 0.2,
        kappa_N = 0.2,
    )

    post_template = copy_env(
        pre_template;
        b_R = benefit_R_post,
        leisure_R = leisure_R_pre * leisure_R_post_multiplier,
        leisure_N = leisure_N_pre * leisure_N_post_multiplier,
    )

    return pre_template, post_template
end

function bounded_pre_objective(base_env::Environment, spec::ModelSpec, target_N::Float64, target_R::Float64)
    return function (x)
        mu = exp(x[1])
        kappa = exp(x[2])
        if !(0.005 <= mu <= 3.0 && 0.0005 <= kappa <= 20.0)
            return 1e6 + abs(log(mu)) + abs(log(kappa))
        end

        env = copy_env(base_env; mu = mu, kappa_R = kappa, kappa_N = kappa)
        values = stationary_values(env, spec)
        out = bellman_update(env, values, spec)
        return (out.find_N - target_N)^2 + (out.find_R - target_R)^2
    end
end

function calibrate_pre_refined(base_env::Environment, spec::ModelSpec,
                               target_N::Float64, target_R::Float64; grid_n::Int = 35)
    grid_env, grid_fit = calibrate_pre(base_env, spec, target_N, target_R; grid_n = grid_n)
    objective = bounded_pre_objective(base_env, spec, target_N, target_R)

    starts = [
        [log(grid_fit.mu), log(grid_fit.kappa)],
        [log(0.06), log(0.60)],
        [log(0.07), log(0.90)],
        [log(0.10), log(2.00)],
        [log(0.04), log(0.25)],
    ]

    best_opt = nothing
    best_loss = grid_fit.loss
    best_x = [log(grid_fit.mu), log(grid_fit.kappa)]

    for initial in starts
        opt = optimize(objective, initial, NelderMead(), Optim.Options(iterations = 300))
        loss = Optim.minimum(opt)
        if loss < best_loss
            best_opt = opt
            best_loss = loss
            best_x = Optim.minimizer(opt)
        end
    end

    mu, kappa = exp(best_x[1]), exp(best_x[2])

    if best_opt === nothing || !(0.005 <= mu <= 3.0 && 0.0005 <= kappa <= 20.0)
        return grid_env, grid_fit
    end

    env = copy_env(base_env; mu = mu, kappa_R = kappa, kappa_N = kappa)
    values = stationary_values(env, spec)
    out = bellman_update(env, values, spec)
    fit = (loss = best_loss, mu = mu, kappa = kappa, find_N = out.find_N, find_R = out.find_R)
    return env, fit
end

function calibrate_post_targeted_refined(pre_env::Environment, post_template::Environment, spec::ModelSpec,
                                         target_N::Float64, target_R::Float64;
                                         common_search_cost_multiplier::Float64 = 1.0,
                                         grid_n::Int = 60)
    kappa_N_post = pre_env.kappa_N * common_search_cost_multiplier

    mu_objective(log_mu) = begin
        mu = exp(log_mu)
        post_env = copy_env(post_template; mu = mu, kappa_N = kappa_N_post, kappa_R = pre_env.kappa_R)
        sim = simulate_path(pre_env, post_env, spec)
        means = period_means(sim.find_N, sim.find_R, spec)
        (means.shock_N - target_N)^2
    end

    mu_grid = minimize_on_log_grid(0.01, 2.0; grid_n = grid_n, passes = 3) do mu
        return mu_objective(log(mu)), nothing
    end
    mu_opt = optimize(mu_objective, log(max(0.01, mu_grid.x / 1.5)), log(min(2.0, mu_grid.x * 1.5)))
    mu_post = exp(Optim.minimizer(mu_opt))

    kappa_objective(log_kappa_R) = begin
        kappa_R = exp(log_kappa_R)
        post_env = copy_env(post_template; mu = mu_post, kappa_N = kappa_N_post, kappa_R = kappa_R)
        sim = simulate_path(pre_env, post_env, spec)
        means = period_means(sim.find_N, sim.find_R, spec)
        (means.shock_R - target_R)^2
    end

    kappa_grid = minimize_on_log_grid(0.001, 20.0; grid_n = grid_n, passes = 3) do kappa_R
        return kappa_objective(log(kappa_R)), nothing
    end
    kappa_opt = optimize(
        kappa_objective,
        log(max(0.001, kappa_grid.x / 1.5)),
        log(min(20.0, kappa_grid.x * 1.5)),
    )
    kappa_R_post = exp(Optim.minimizer(kappa_opt))

    env = copy_env(post_template; mu = mu_post, kappa_N = kappa_N_post, kappa_R = kappa_R_post)
    sim = simulate_path(pre_env, env, spec)
    means = period_means(sim.find_N, sim.find_R, spec)
    fit = (
        mu = mu_post,
        kappa_N = kappa_N_post,
        kappa_R = kappa_R_post,
        shock_N = means.shock_N,
        shock_R = means.shock_R,
        loss = (means.shock_N - target_N)^2 + (means.shock_R - target_R)^2,
    )

    return env, fit
end

function shock_mean_N(pre_env::Environment, post_env::Environment, spec::ModelSpec)
    sim = simulate_path(pre_env, post_env, spec)
    return period_means(sim.find_N, sim.find_R, spec).shock_N
end

function shock_mean_R(pre_env::Environment, post_env::Environment, spec::ModelSpec)
    sim = simulate_path(pre_env, post_env, spec)
    return period_means(sim.find_N, sim.find_R, spec).shock_R
end

function fit_kappa_N_for_target(pre_env::Environment, post_template::Environment, spec::ModelSpec,
                                target_N::Float64; mu::Float64, grid_n::Int = 60)
    lower = pre_env.kappa_N
    post_at_lower = copy_env(post_template; mu = mu, kappa_N = lower, kappa_R = pre_env.kappa_R)
    if shock_mean_N(pre_env, post_at_lower, spec) <= target_N + 1e-8
        return lower
    end

    objective(log_kappa_N) = begin
        kappa_N = exp(log_kappa_N)
        post_env = copy_env(post_template; mu = mu, kappa_N = kappa_N, kappa_R = pre_env.kappa_R)
        (shock_mean_N(pre_env, post_env, spec) - target_N)^2
    end

    grid = minimize_on_log_grid(lower, 20.0; grid_n = grid_n, passes = 3) do kappa_N
        return objective(log(kappa_N)), nothing
    end
    opt = optimize(objective, log(max(lower, grid.x / 1.5)), log(min(20.0, grid.x * 1.5)))
    return exp(Optim.minimizer(opt))
end

function fit_mu_for_target_N(pre_env::Environment, post_template::Environment, spec::ModelSpec,
                             target_N::Float64; kappa_N::Float64, grid_n::Int = 60)
    objective(log_mu) = begin
        mu = exp(log_mu)
        post_env = copy_env(post_template; mu = mu, kappa_N = kappa_N, kappa_R = pre_env.kappa_R)
        (shock_mean_N(pre_env, post_env, spec) - target_N)^2
    end

    grid = minimize_on_log_grid(0.01, 2.0; grid_n = grid_n, passes = 3) do mu
        return objective(log(mu)), nothing
    end
    opt = optimize(objective, log(max(0.01, grid.x / 1.5)), log(min(2.0, grid.x * 1.5)))
    return exp(Optim.minimizer(opt))
end

function fit_kappa_R_for_target(pre_env::Environment, post_template::Environment, spec::ModelSpec,
                                target_R::Float64; mu::Float64, kappa_N::Float64, grid_n::Int = 60)
    lower = kappa_N
    objective(log_kappa_R) = begin
        kappa_R = exp(log_kappa_R)
        post_env = copy_env(post_template; mu = mu, kappa_N = kappa_N, kappa_R = kappa_R)
        (shock_mean_R(pre_env, post_env, spec) - target_R)^2
    end

    grid = minimize_on_log_grid(lower, 30.0; grid_n = grid_n, passes = 3) do kappa_R
        return objective(log(kappa_R)), nothing
    end
    opt = optimize(objective, log(max(lower, grid.x / 1.5)), log(min(30.0, grid.x * 1.5)))
    return exp(Optim.minimizer(opt))
end

function log_did_series(find_R_a::Vector{Float64}, find_N_a::Vector{Float64},
                        find_R_b::Vector{Float64}, find_N_b::Vector{Float64})
    return (log.(find_R_a) .- log.(find_R_b)) .- (log.(find_N_a) .- log.(find_N_b))
end

function log_did_from_pre(find_R::Vector{Float64}, find_N::Vector{Float64})
    return log_did_series(
        find_R,
        find_N,
        fill(find_R[1], length(find_R)),
        fill(find_N[1], length(find_N)),
    )
end

function benefit_log_change(pre_env::Environment, shock_env::Environment)
    return log(shock_env.b_R / pre_env.b_R)
end

function run_scenario(; label = "baseline",
                      match_elasticity = 0.42,
                      delta = 0.01,
                      risk_aversion = 0.0,
                      common_search_cost_multiplier = 1.0,
                      leisure_R_post_multiplier = 1.0,
                      leisure_N_post_multiplier = 1.0,
                      benefit_multiplier = 2.0,
                      benefit_weight = 1.0,
                      target_pre_N = TARGET_PRE_N,
                      target_pre_R = TARGET_PRE_R,
                      target_post_N = TARGET_POST_N,
                      target_post_R = TARGET_POST_R,
                      pre_grid_n = 45,
                      post_grid_n = 90)
    spec = build_spec(
        match_elasticity = match_elasticity,
        delta = delta,
        risk_aversion = risk_aversion,
    )

    pre_template, post_template = model_environments(
        spec;
        benefit_multiplier = benefit_multiplier,
        benefit_weight = benefit_weight,
        leisure_R_post_multiplier = leisure_R_post_multiplier,
        leisure_N_post_multiplier = leisure_N_post_multiplier,
    )

    pre_env, pre_fit = calibrate_pre_refined(
        pre_template,
        spec,
        target_pre_N,
        target_pre_R;
        grid_n = pre_grid_n,
    )

    full_post_env, post_fit = calibrate_post_targeted_refined(
        pre_env,
        post_template,
        spec,
        target_post_N,
        target_post_R;
        common_search_cost_multiplier = common_search_cost_multiplier,
        grid_n = post_grid_n,
    )

    benefit_only_env = copy_env(
        pre_env;
        b_R = post_template.b_R,
        b_N = post_template.b_N,
        leisure_R = pre_env.leisure_R,
        leisure_N = pre_env.leisure_N,
    )

    covid_only_env = copy_env(
        full_post_env;
        b_R = pre_env.b_R,
        b_N = pre_env.b_N,
        leisure_R = post_template.leisure_R,
        leisure_N = post_template.leisure_N,
    )

    full_sim = simulate_path(pre_env, full_post_env, spec)
    benefit_sim = simulate_path(pre_env, benefit_only_env, spec)
    covid_sim = simulate_path(pre_env, covid_only_env, spec)

    did_full = did_series(full_sim)
    did_benefit_normal = did_series(benefit_sim)
    did_covid = did_series(covid_sim)
    did_benefit_covid_env = did_full .- did_covid
    did_interaction = did_full .- did_benefit_normal .- did_covid

    log_denom = benefit_log_change(pre_env, full_post_env)
    log_did_full = log_did_from_pre(full_sim.find_R, full_sim.find_N)
    log_did_benefit_normal = log_did_from_pre(benefit_sim.find_R, benefit_sim.find_N)
    log_did_covid = log_did_from_pre(covid_sim.find_R, covid_sim.find_N)
    log_did_benefit_covid_env = log_did_series(
        full_sim.find_R,
        full_sim.find_N,
        covid_sim.find_R,
        covid_sim.find_N,
    )

    shock = spec.shock_start:spec.shock_end
    means_full = period_means(full_sim.find_N, full_sim.find_R, spec)

    summary = (
        label = label,
        match_elasticity = match_elasticity,
        vacancy_elasticity = 1.0 - match_elasticity,
        delta = delta,
        risk_aversion = risk_aversion,
        common_search_cost_multiplier = common_search_cost_multiplier,
        leisure_R_post_multiplier = leisure_R_post_multiplier,
        leisure_N_post_multiplier = leisure_N_post_multiplier,
        benefit_multiplier = benefit_multiplier,
        benefit_weight = benefit_weight,
        target_pre_N = target_pre_N,
        target_pre_R = target_pre_R,
        target_post_N = target_post_N,
        target_post_R = target_post_R,
        mu_pre = pre_env.mu,
        mu_post = full_post_env.mu,
        kappa_pre = pre_env.kappa_R,
        kappa_R_post = full_post_env.kappa_R,
        kappa_N_post = full_post_env.kappa_N,
        fit_pre_N = pre_fit.find_N,
        fit_pre_R = pre_fit.find_R,
        fit_post_N = post_fit.shock_N,
        fit_post_R = post_fit.shock_R,
        did_full_pp = 100 * mean(did_full[shock]),
        did_benefit_normal_pp = 100 * mean(did_benefit_normal[shock]),
        did_covid_only_pp = 100 * mean(did_covid[shock]),
        did_benefit_covid_env_pp = 100 * mean(did_benefit_covid_env[shock]),
        did_interaction_pp = 100 * mean(did_interaction[shock]),
        elasticity_full_logdid = mean(log_did_full[shock]) / log_denom,
        elasticity_benefit_normal_logdid = mean(log_did_benefit_normal[shock]) / log_denom,
        elasticity_covid_only_logdid = mean(log_did_covid[shock]) / log_denom,
        elasticity_benefit_covid_env_logdid = mean(log_did_benefit_covid_env[shock]) / log_denom,
        elasticity_ratio_full_to_marginal = abs(mean(log_did_benefit_covid_env[shock])) > 1e-10 ?
            mean(log_did_full[shock]) / mean(log_did_benefit_covid_env[shock]) : NaN,
        observed_did_over_marginal = mean(abs.(did_benefit_covid_env[shock])) > 1e-10 ?
            mean(did_full[shock]) / mean(did_benefit_covid_env[shock]) : NaN,
        shock_gap_full_pp = 100 * (means_full.shock_R - means_full.shock_N),
    )

    series = DataFrame(
        Week = 1:spec.T,
        DiD_Full_Shock = 100 .* did_full,
        DiD_Benefit_Normal_Market = 100 .* did_benefit_normal,
        DiD_COVID_Only = 100 .* did_covid,
        DiD_Benefit_In_COVID_Market = 100 .* did_benefit_covid_env,
        DiD_Interaction = 100 .* did_interaction,
        Elasticity_Full_LogDiD = log_did_full ./ log_denom,
        Elasticity_Benefit_Normal_Market = log_did_benefit_normal ./ log_denom,
        Elasticity_COVID_Only = log_did_covid ./ log_denom,
        Elasticity_Benefit_In_COVID_Market = log_did_benefit_covid_env ./ log_denom,
        f_N_Full = 100 .* full_sim.find_N,
        f_R_Full = 100 .* full_sim.find_R,
        f_N_COVID_Only = 100 .* covid_sim.find_N,
        f_R_COVID_Only = 100 .* covid_sim.find_R,
    )

    return (
        spec = spec,
        pre_env = pre_env,
        full_post_env = full_post_env,
        benefit_only_env = benefit_only_env,
        covid_only_env = covid_only_env,
        full_sim = full_sim,
        benefit_sim = benefit_sim,
        covid_sim = covid_sim,
        series = series,
        summary = summary,
    )
end

function run_equal_health_scenario(; label = "equal_health",
                                   match_elasticity = 0.42,
                                   delta = 0.01,
                                   risk_aversion = 0.0,
                                   common_search_cost_multiplier = 1.0,
                                   r_health_premium = 1.0,
                                   leisure_R_post_multiplier = 1.0,
                                   leisure_N_post_multiplier = 1.0,
                                   benefit_multiplier = 2.0,
                                   benefit_weight = 1.0,
                                   target_pre_N = TARGET_PRE_N,
                                   target_pre_R = TARGET_PRE_R,
                                   target_post_N = TARGET_POST_N,
                                   target_post_R = TARGET_POST_R,
                                   pre_grid_n = 35,
                                   post_grid_n = 60)
    spec = build_spec(match_elasticity = match_elasticity, delta = delta, risk_aversion = risk_aversion)
    pre_template, post_template = model_environments(
        spec;
        benefit_multiplier = benefit_multiplier,
        benefit_weight = benefit_weight,
        leisure_R_post_multiplier = leisure_R_post_multiplier,
        leisure_N_post_multiplier = leisure_N_post_multiplier,
    )

    pre_env, pre_fit = calibrate_pre_refined(
        pre_template,
        spec,
        target_pre_N,
        target_pre_R;
        grid_n = pre_grid_n,
    )

    kappa_post = pre_env.kappa_N * common_search_cost_multiplier
    mu_post = fit_mu_for_target_N(
        pre_env,
        post_template,
        spec,
        target_post_N;
        kappa_N = kappa_post,
        grid_n = post_grid_n,
    )

    full_post_env = copy_env(post_template; mu = mu_post, kappa_N = kappa_post, kappa_R = r_health_premium * kappa_post)
    covid_only_env = copy_env(full_post_env; b_R = pre_env.b_R)
    benefit_only_env = copy_env(pre_env; b_R = post_template.b_R)

    full_sim = simulate_path(pre_env, full_post_env, spec)
    covid_sim = simulate_path(pre_env, covid_only_env, spec)
    benefit_sim = simulate_path(pre_env, benefit_only_env, spec)

    did_full = did_series(full_sim)
    did_covid = did_series(covid_sim)
    did_benefit_covid = did_full .- did_covid
    did_benefit_normal = did_series(benefit_sim)
    log_denom = log(benefit_multiplier)
    log_did_full = log_did_from_pre(full_sim.find_R, full_sim.find_N)
    log_did_benefit_normal = log_did_from_pre(benefit_sim.find_R, benefit_sim.find_N)
    log_did_covid = log_did_from_pre(covid_sim.find_R, covid_sim.find_N)
    log_did_benefit_covid = log_did_series(
        full_sim.find_R,
        full_sim.find_N,
        covid_sim.find_R,
        covid_sim.find_N,
    )
    shock = spec.shock_start:spec.shock_end
    means = period_means(full_sim.find_N, full_sim.find_R, spec)
    means_covid = period_means(covid_sim.find_N, covid_sim.find_R, spec)
    means_benefit = period_means(benefit_sim.find_N, benefit_sim.find_R, spec)
    full_elasticity_from_means = logdid_elasticity_from_means(
        means.pre_R,
        means.pre_N,
        means.shock_R,
        means.shock_N,
        log_denom,
    )
    benefit_covid_elasticity_from_means =
        (log(means.shock_R / means_covid.shock_R) - log(means.shock_N / means_covid.shock_N)) / log_denom
    benefit_normal_elasticity_from_means = logdid_elasticity_from_means(
        means_benefit.pre_R,
        means_benefit.pre_N,
        means_benefit.shock_R,
        means_benefit.shock_N,
        log_denom,
    )

    summary = (
        label = label,
        match_elasticity = match_elasticity,
        delta = delta,
        risk_aversion = risk_aversion,
        common_search_cost_multiplier = common_search_cost_multiplier,
        r_health_premium = r_health_premium,
        leisure_R_post_multiplier = leisure_R_post_multiplier,
        leisure_N_post_multiplier = leisure_N_post_multiplier,
        benefit_multiplier = benefit_multiplier,
        benefit_weight = benefit_weight,
        target_pre_N = target_pre_N,
        target_pre_R = target_pre_R,
        target_post_N = target_post_N,
        target_post_R = target_post_R,
        mu_pre = pre_env.mu,
        mu_post = full_post_env.mu,
        kappa_pre = pre_env.kappa_R,
        kappa_N_post = kappa_post,
        kappa_R_post = r_health_premium * kappa_post,
        fit_pre_N = pre_fit.find_N,
        fit_pre_R = pre_fit.find_R,
        fit_post_N = means.shock_N,
        fit_post_R = means.shock_R,
        post_R_target_gap_pp = 100 * (means.shock_R - target_post_R),
        did_full_pp = 100 * mean(did_full[shock]),
        target_full_did_pp = 100 * ((target_post_R - target_post_N) - (target_pre_R - target_pre_N)),
        did_benefit_covid_env_pp = 100 * mean(did_benefit_covid[shock]),
        did_benefit_normal_pp = 100 * mean(did_benefit_normal[shock]),
        did_covid_only_pp = 100 * mean(did_covid[shock]),
        elasticity_full_logdid = full_elasticity_from_means,
        target_full_elasticity_logdid =
            (log(target_post_R / target_pre_R) - log(target_post_N / target_pre_N)) / log_denom,
        elasticity_benefit_normal_logdid = benefit_normal_elasticity_from_means,
        elasticity_benefit_covid_env_logdid = benefit_covid_elasticity_from_means,
        elasticity_covid_only_logdid = mean(log_did_covid[shock]) / log_denom,
        target_to_structural_elasticity_ratio =
            ((log(target_post_R / target_pre_R) - log(target_post_N / target_pre_N)) / log_denom) /
            benefit_covid_elasticity_from_means,
    )

    series = DataFrame(
        Week = 1:spec.T,
        DiD_Full_Shock = 100 .* did_full,
        DiD_COVID_Only = 100 .* did_covid,
        DiD_Benefit_In_COVID_Market = 100 .* did_benefit_covid,
        DiD_Benefit_Normal_Market = 100 .* did_benefit_normal,
        Elasticity_Full_LogDiD = log_did_full ./ log_denom,
        Elasticity_COVID_Only = log_did_covid ./ log_denom,
        Elasticity_Benefit_In_COVID_Market = log_did_benefit_covid ./ log_denom,
        Elasticity_Benefit_Normal_Market = log_did_benefit_normal ./ log_denom,
        Target_Full_Elasticity_LogDiD = fill(summary.target_full_elasticity_logdid, spec.T),
        f_N_Full = 100 .* full_sim.find_N,
        f_R_Full = 100 .* full_sim.find_R,
        f_N_COVID_Only = 100 .* covid_sim.find_N,
        f_R_COVID_Only = 100 .* covid_sim.find_R,
    )

    return (
        spec = spec,
        pre_env = pre_env,
        full_post_env = full_post_env,
        covid_only_env = covid_only_env,
        benefit_only_env = benefit_only_env,
        full_sim = full_sim,
        covid_sim = covid_sim,
        benefit_sim = benefit_sim,
        series = series,
        summary = summary,
    )
end

function calibrate_benefit_weight_equal_health(; grid_n::Int = 25, refine_grid_n::Int = 45,
                                               pre_grid_n::Int = 30, post_grid_n::Int = 50,
                                               kwargs...)
    rows = NamedTuple[]
    best = nothing
    best_loss = Inf

    for weight in logrange(0.25, 8.0, grid_n)
        scenario = run_equal_health_scenario(;
            benefit_weight = weight,
            pre_grid_n = pre_grid_n,
            post_grid_n = post_grid_n,
            kwargs...,
        )
        loss = scenario.summary.post_R_target_gap_pp^2
        push!(rows, scenario.summary)
        if loss < best_loss
            best_loss = loss
            best = scenario
        end
    end

    lo = max(0.25, best.summary.benefit_weight / 1.6)
    hi = min(8.0, best.summary.benefit_weight * 1.6)
    for weight in logrange(lo, hi, refine_grid_n)
        scenario = run_equal_health_scenario(;
            benefit_weight = weight,
            pre_grid_n = pre_grid_n,
            post_grid_n = post_grid_n,
            kwargs...,
        )
        loss = scenario.summary.post_R_target_gap_pp^2
        push!(rows, scenario.summary)
        if loss < best_loss
            best_loss = loss
            best = scenario
        end
    end

    path = DataFrame(rows)
    sort!(path, :benefit_weight)
    return best, path
end

function health_premium_benefit_responsiveness_grid(; premiums = collect(range(1.0, 2.5, length = 9)),
                                                    grid_n::Int = 12,
                                                    refine_grid_n::Int = 20,
                                                    pre_grid_n::Int = 22,
                                                    post_grid_n::Int = 35)
    rows = NamedTuple[]
    paths = DataFrame()

    for premium in premiums
        calibrated, path = calibrate_benefit_weight_equal_health(
            r_health_premium = premium,
            grid_n = grid_n,
            refine_grid_n = refine_grid_n,
            pre_grid_n = pre_grid_n,
            post_grid_n = post_grid_n,
        )

        path.RHealthPremium .= premium
        paths = vcat(paths, path; cols = :union)
        push!(rows, calibrated.summary)
    end

    summary = DataFrame(rows)
    sort!(summary, :r_health_premium)
    sort!(paths, [:RHealthPremium, :benefit_weight])
    return summary, paths
end

function literature_moments()
    return DataFrame(
        Moment = [
            "Matching elasticity: unemployed/jobseekers",
            "Matching elasticity: vacancies",
            "Matching efficiency level",
            "Labour-market tightness and job-finding probability",
            "Separation/turnover rate",
            "Mutual-obligation/search requirement shock",
            "Search-effort response to benefit generosity",
            "Health/risk cost of search by eligibility group",
        ],
        SuggestedUse = [
            "Use to set match_elasticity. Australian estimate is 0.326; normalized CRS value with vacancy elasticity 0.450 is about 0.42.",
            "Use as 1 - match_elasticity in the CRS model. Australian estimate is 0.450; normalized CRS value is about 0.58.",
            "Use to benchmark mu changes, not as a direct model target because model mu has arbitrary weekly units.",
            "Use as additional calibration moments once matched-sample vacancies/searchers and UE flows are constructed.",
            "Use as sensitivity range for delta; annual employee separations near 20 percent imply roughly 0.4 percent weekly, but job-loss samples may justify higher values.",
            "Use to discipline common or recipient-specific kappa shocks during the suspension/reintroduction window.",
            "Australia-specific moment unavailable in current script; use international UI-search evidence only as a prior range.",
            "Unavailable; should be estimated from admin/LFS/health exposure data or bounded through sensitivity.",
        ],
        CentralValue = [0.42, 0.58, 0.841, missing, 0.004, missing, -1.9, missing],
        Low = [0.33, 0.50, missing, missing, 0.004, 0.8, -2.2, 0.8],
        High = [0.50, 0.67, missing, missing, 0.012, 1.4, -1.6, 2.5],
        Availability = [
            "Australian literature",
            "Australian literature",
            "Australian literature",
            "Australian data framework available; matched-study moment to estimate",
            "Australian literature, but not job-loser-specific weekly hazard",
            "Australian institutional timing available; behavioural magnitude missing",
            "International literature fallback",
            "Missing; estimate from administrative data",
        ],
        Source = [
            "Lake et al. 2024, ABS/RBA-JSA matching efficiency paper",
            "Lake et al. 2024, ABS/RBA-JSA matching efficiency paper",
            "Lake et al. 2024, ABS/RBA-JSA matching efficiency paper",
            "Treasury Round Up 2024; ABS Labour Force flows and Job Vacancies",
            "Sila 2019 OECD/HILDA job displacement paper",
            "Australian Treasury 2020 income-support fact sheet",
            "Krueger and Mueller 2010, US time-use evidence",
            "No clean Australian moment found",
        ],
        URL = [
            "https://www.rba.gov.au/publications/other-confs/abs-and-rba-joint-conferences/2024/pdf/abs-rba-conference-2024-lake-shamiri-sharma-bialowas.pdf",
            "https://www.rba.gov.au/publications/other-confs/abs-and-rba-joint-conferences/2024/pdf/abs-rba-conference-2024-lake-shamiri-sharma-bialowas.pdf",
            "https://www.rba.gov.au/publications/other-confs/abs-and-rba-joint-conferences/2024/pdf/abs-rba-conference-2024-lake-shamiri-sharma-bialowas.pdf",
            "https://treasury.gov.au/sites/default/files/2024-03/p2024-495252-full-report_0.pdf",
            "https://www.oecd.org/en/publications/job-displacement-in-australia_eccc0b28-en.html",
            "https://treasury.gov.au/sites/default/files/2020-07/Fact_sheet-Income_Support_for_Individuals_2.pdf",
            "https://www.sciencedirect.com/science/article/pii/S0047272709001625",
            "",
        ],
    )
end

function plot_baseline_series(baseline)
    df = baseline.series

    plt = plot(
        df.Week,
        df.DiD_Full_Shock,
        label = "Full DiD",
        lw = 2,
        xlabel = "Week",
        ylabel = "DiD in job-finding rates (p.p.)",
        title = "Observed DiD vs Structural Counterfactuals",
    )
    plot!(plt, df.Week, df.DiD_Benefit_In_COVID_Market, label = "Benefit effect in COVID market", lw = 2, ls = :dash)
    plot!(plt, df.Week, df.DiD_Benefit_Normal_Market, label = "Benefit only, normal market", lw = 2, ls = :dot)
    plot!(plt, df.Week, df.DiD_COVID_Only, label = "COVID only", lw = 2, ls = :dashdot)
    savefig(plt, "12b_did_marginal_benefit_covid_market.png")

    elasticities = plot(
        df.Week,
        df.Elasticity_Full_LogDiD,
        label = "Full DiD elasticity",
        lw = 2,
        xlabel = "Week",
        ylabel = "Log-DiD elasticity wrt benefits",
        title = "Elasticity: Full DiD vs Structural Benefit Response",
    )
    plot!(elasticities, df.Week, df.Elasticity_Benefit_In_COVID_Market,
          label = "Benefit elasticity in COVID market", lw = 2, ls = :dash)
    plot!(elasticities, df.Week, df.Elasticity_Benefit_Normal_Market,
          label = "Benefit elasticity, normal market", lw = 2, ls = :dot)
    plot!(elasticities, df.Week, df.Elasticity_COVID_Only,
          label = "COVID-only pseudo-elasticity", lw = 2, ls = :dashdot)
    hline!(elasticities, [0.0], label = "", color = :black, alpha = 0.3)
    savefig(elasticities, "12b_elasticity_marginal_benefit_covid_market.png")

    levels = plot(
        df.Week,
        df.f_N_Full,
        label = "N full",
        lw = 2,
        xlabel = "Week",
        ylabel = "Job-finding rate (%)",
        title = "Full Shock vs COVID-Only Levels",
    )
    plot!(levels, df.Week, df.f_R_Full, label = "R full", lw = 2)
    plot!(levels, df.Week, df.f_N_COVID_Only, label = "N COVID-only", lw = 2, ls = :dash)
    plot!(levels, df.Week, df.f_R_COVID_Only, label = "R COVID-only", lw = 2, ls = :dash)
    savefig(levels, "12b_job_finding_full_vs_covid_only.png")
end

function plot_benefit_weight_calibration(path::DataFrame, calibrated)
    plt_fit = plot(
        path.benefit_weight,
        path.fit_post_R .* 100,
        xscale = :log10,
        label = "Model-predicted R post rate",
        lw = 2,
        xlabel = "Benefit responsiveness weight",
        ylabel = "R post job-finding rate (%)",
        title = "Calibrating Benefit Responsiveness Under Equal Health Shocks",
    )
    hline!(plt_fit, [100 * TARGET_POST_R], label = "Observed R post target", color = :black, ls = :dash)
    vline!(plt_fit, [calibrated.summary.benefit_weight], label = "Calibrated weight", color = :red, ls = :dot)
    savefig(plt_fit, "12b_benefit_weight_fit_post_R.png")

    plt_elasticity = plot(
        path.benefit_weight,
        path.target_full_elasticity_logdid,
        xscale = :log10,
        label = "Target-coded full DiD elasticity",
        lw = 2,
        xlabel = "Benefit responsiveness weight",
        ylabel = "Elasticity wrt known benefit increase",
        title = "Benefit Responsiveness and Labour-Supply Elasticity",
    )
    plot!(plt_elasticity, path.benefit_weight, path.elasticity_benefit_covid_env_logdid,
          label = "Structural benefit elasticity", lw = 2, ls = :dash)
    plot!(plt_elasticity, path.benefit_weight, path.elasticity_covid_only_logdid,
          label = "COVID-only pseudo-elasticity", lw = 2, ls = :dot)
    vline!(plt_elasticity, [calibrated.summary.benefit_weight], label = "Calibrated weight", color = :red, ls = :dot)
    savefig(plt_elasticity, "12b_benefit_weight_elasticity.png")

    plt_ratio = plot(
        path.benefit_weight,
        abs.(path.target_to_structural_elasticity_ratio),
        xscale = :log10,
        label = "Target / structural benefit elasticity",
        lw = 2,
        xlabel = "Benefit responsiveness weight",
        ylabel = "Absolute ratio",
        title = "Does the DiD Overstate the Benefit Elasticity?",
    )
    hline!(plt_ratio, [1.0], label = "Equal magnitude", color = :black, ls = :dash)
    vline!(plt_ratio, [calibrated.summary.benefit_weight], label = "Calibrated weight", color = :red, ls = :dot)
    savefig(plt_ratio, "12b_benefit_weight_elasticity_ratio.png")
end

function plot_health_premium_benefit_responsiveness(summary::DataFrame, paths::DataFrame)
    plt_weight = plot(
        summary.r_health_premium,
        summary.benefit_weight,
        label = "Estimated benefit responsiveness",
        lw = 2,
        xlabel = "R health/search-cost premium over N",
        ylabel = "Benefit responsiveness weight",
        title = "Benefit Responsiveness Needed Under Health-Shock Assumptions",
    )
    hline!(plt_weight, [1.0], label = "Unit benefit weight", color = :black, ls = :dash)
    savefig(plt_weight, "12b_health_premium_required_benefit_weight.png")

    plt_elasticity = plot(
        summary.r_health_premium,
        summary.target_full_elasticity_logdid,
        label = "Target-coded DiD elasticity",
        lw = 2,
        xlabel = "R health/search-cost premium over N",
        ylabel = "Elasticity wrt known benefit increase",
        title = "Estimated Benefit Elasticity as Differential Health Shock Varies",
    )
    plot!(plt_elasticity, summary.r_health_premium, summary.elasticity_benefit_covid_env_logdid,
          label = "Structural benefit elasticity", lw = 2, ls = :dash)
    plot!(plt_elasticity, summary.r_health_premium, summary.elasticity_covid_only_logdid,
          label = "COVID-only pseudo-elasticity", lw = 2, ls = :dot)
    savefig(plt_elasticity, "12b_health_premium_estimated_benefit_elasticity.png")

    plt_pp = plot(
        summary.r_health_premium,
        summary.target_full_did_pp,
        label = "Empirical DiD",
        lw = 2,
        xlabel = "R health/search-cost premium over N",
        ylabel = "Job-finding response (p.p.)",
        title = "Percentage-Point Benefit Response as Health Assumption Varies",
    )
    plot!(plt_pp, summary.r_health_premium, summary.did_benefit_covid_env_pp,
          label = "Benefit effect in COVID market", lw = 2, ls = :dash)
    plot!(plt_pp, summary.r_health_premium, summary.did_benefit_normal_pp,
          label = "Benefit-only normal-market simulation", lw = 2, ls = :dot)
    hline!(plt_pp, [0.0], label = "", color = :black, alpha = 0.3)
    savefig(plt_pp, "12b_health_premium_estimated_benefit_pp.png")

    plt_pp_ratio = plot(
        summary.r_health_premium,
        abs.(summary.target_full_did_pp ./ summary.did_benefit_covid_env_pp),
        label = "Empirical DiD / COVID-market benefit effect",
        lw = 2,
        xlabel = "R health/search-cost premium over N",
        ylabel = "Absolute ratio",
        title = "DiD vs Structural Benefit Effect in Percentage Points",
    )
    hline!(plt_pp_ratio, [1.0], label = "Equal magnitude", color = :black, ls = :dash)
    savefig(plt_pp_ratio, "12b_health_premium_estimated_benefit_pp_ratio.png")

    plt_ratio = plot(
        summary.r_health_premium,
        abs.(summary.target_to_structural_elasticity_ratio),
        label = "Target / structural benefit elasticity",
        lw = 2,
        xlabel = "R health/search-cost premium over N",
        ylabel = "Absolute ratio",
        title = "DiD vs Estimated Benefit Elasticity",
    )
    hline!(plt_ratio, [1.0], label = "Equal magnitude", color = :black, ls = :dash)
    savefig(plt_ratio, "12b_health_premium_estimated_elasticity_ratio.png")

    plt_fit = plot(
        summary.r_health_premium,
        summary.post_R_target_gap_pp,
        label = "Post R fit error",
        lw = 2,
        xlabel = "R health/search-cost premium over N",
        ylabel = "Post R target gap (p.p.)",
        title = "Calibration Fit Across Health-Premium Assumptions",
    )
    hline!(plt_fit, [0.0], label = "Exact fit", color = :black, ls = :dash)
    savefig(plt_fit, "12b_health_premium_benefit_weight_fit_error.png")

    plt_surface = scatter(
        paths.RHealthPremium,
        paths.benefit_weight,
        marker_z = paths.post_R_target_gap_pp,
        xlabel = "R health/search-cost premium over N",
        ylabel = "Benefit responsiveness weight",
        title = "Post R Fit Across Health Premium and Benefit Responsiveness",
        colorbar_title = "R gap p.p.",
        label = "",
    )
    savefig(plt_surface, "12b_health_premium_benefit_weight_fit_surface.png")
end

function normal_market_elasticity_from_health_assumptions(health_summary::DataFrame)
    rows = NamedTuple[]

    for row in eachrow(health_summary)
        scenario = run_equal_health_scenario(
            r_health_premium = row.r_health_premium,
            benefit_weight = row.benefit_weight,
            pre_grid_n = 28,
            post_grid_n = 45,
        )

        spec = scenario.spec
        pre_env = scenario.pre_env
        normal_benefit_env = scenario.benefit_only_env
        normal_sim = simulate_path(pre_env, normal_benefit_env, spec)
        means = period_means(normal_sim.find_N, normal_sim.find_R, spec)
        log_denom = log(row.benefit_multiplier)

        normal_elasticity = logdid_elasticity_from_means(
            means.pre_R,
            means.pre_N,
            means.shock_R,
            means.shock_N,
            log_denom,
        )
        normal_did_pp = 100 * ((means.shock_R - means.shock_N) - (means.pre_R - means.pre_N))

        push!(rows, (
            r_health_premium = row.r_health_premium,
            benefit_weight = row.benefit_weight,
            target_full_elasticity_logdid = row.target_full_elasticity_logdid,
            covid_market_benefit_elasticity = row.elasticity_benefit_covid_env_logdid,
            normal_market_benefit_elasticity = normal_elasticity,
            target_to_normal_elasticity_ratio = row.target_full_elasticity_logdid / normal_elasticity,
            normal_market_did_pp = normal_did_pp,
            target_full_did_pp = row.target_full_did_pp,
            covid_market_benefit_did_pp = row.did_benefit_covid_env_pp,
        ))
    end

    return DataFrame(rows)
end

function plot_normal_market_elasticity(normal_df::DataFrame)
    plt = plot(
        normal_df.r_health_premium,
        normal_df.target_full_elasticity_logdid,
        label = "Empirical DiD elasticity",
        lw = 2,
        xlabel = "R health/search-cost premium over N used in COVID decomposition",
        ylabel = "Elasticity wrt known benefit increase",
        title = "Implied No-COVID Benefit Elasticity",
    )
    plot!(plt, normal_df.r_health_premium, normal_df.covid_market_benefit_elasticity,
          label = "Benefit elasticity in COVID market", lw = 2, ls = :dash)
    plot!(plt, normal_df.r_health_premium, normal_df.normal_market_benefit_elasticity,
          label = "Benefit elasticity without COVID", lw = 2, ls = :dot)
    hline!(plt, [0.0], label = "", color = :black, alpha = 0.3)
    savefig(plt, "12b_normal_market_benefit_elasticity.png")

    plt_ratio = plot(
        normal_df.r_health_premium,
        abs.(normal_df.target_to_normal_elasticity_ratio),
        label = "Empirical DiD / no-COVID benefit elasticity",
        lw = 2,
        xlabel = "R health/search-cost premium over N used in COVID decomposition",
        ylabel = "Absolute ratio",
        title = "Does the DiD Overstate the No-COVID Benefit Elasticity?",
    )
    hline!(plt_ratio, [1.0], label = "Equal magnitude", color = :black, ls = :dash)
    savefig(plt_ratio, "12b_normal_market_elasticity_ratio.png")

    plt_pp = plot(
        normal_df.r_health_premium,
        normal_df.target_full_did_pp,
        label = "Empirical DiD",
        lw = 2,
        xlabel = "R health/search-cost premium over N used in COVID decomposition",
        ylabel = "Difference-in-differences (p.p.)",
        title = "No-COVID Benefit Effect in Percentage Points",
    )
    plot!(plt_pp, normal_df.r_health_premium, normal_df.covid_market_benefit_did_pp,
          label = "Benefit effect in COVID market", lw = 2, ls = :dash)
    plot!(plt_pp, normal_df.r_health_premium, normal_df.normal_market_did_pp,
          label = "Benefit effect without COVID", lw = 2, ls = :dot)
    savefig(plt_pp, "12b_normal_market_benefit_did_pp.png")

    plt_pp_ratio = plot(
        normal_df.r_health_premium,
        abs.(normal_df.target_full_did_pp ./ normal_df.normal_market_did_pp),
        label = "Empirical DiD / no-COVID benefit effect",
        lw = 2,
        xlabel = "R health/search-cost premium over N used in COVID decomposition",
        ylabel = "Absolute ratio",
        title = "DiD vs No-COVID Benefit Effect in Percentage Points",
    )
    hline!(plt_pp_ratio, [1.0], label = "Equal magnitude", color = :black, ls = :dash)
    savefig(plt_pp_ratio, "12b_normal_market_benefit_did_pp_ratio.png")
end

function one_way_sensitivity()
    rows = NamedTuple[]

    sweeps = [
        ("match_elasticity", [0.33, 0.38, 0.42, 0.46, 0.50]),
        ("delta", [0.004, 0.006, 0.008, 0.010, 0.012]),
        ("common_search_cost_multiplier", [0.8, 0.9, 1.0, 1.2, 1.4]),
        ("leisure_R_post_multiplier", [0.8, 1.0, 1.2, 1.5, 2.0]),
        ("leisure_N_post_multiplier", [0.8, 1.0, 1.2, 1.5, 2.0]),
        ("benefit_multiplier", [1.5, 1.75, 2.0, 2.25, 2.5]),
    ]

    for (name, values) in sweeps
        for value in values
            kwargs = Dict{Symbol, Any}(:label => "$(name)=$(value)", :pre_grid_n => 30, :post_grid_n => 55)
            kwargs[Symbol(name)] = value
            scenario = run_scenario(; kwargs...)
            push!(rows, merge((Sweep = name, Value = value), scenario.summary))
        end
    end

    return DataFrame(rows)
end

function paired_sensitivity()
    rows = NamedTuple[]
    alphas = [0.33, 0.42, 0.50]
    common_costs = [0.8, 1.0, 1.4]
    recipient_leisure = [0.8, 1.0, 1.5]
    recipient_benefits = [1.75, 2.0, 2.25]

    for alpha in alphas, cost_mult in common_costs
        scenario = run_scenario(
            label = "alpha=$(alpha), common_cost=$(cost_mult)",
            match_elasticity = alpha,
            common_search_cost_multiplier = cost_mult,
            pre_grid_n = 28,
            post_grid_n = 50,
        )
        push!(rows, merge((Sweep = "alpha_x_common_cost", X = alpha, Y = cost_mult), scenario.summary))
    end

    for leisure_mult in recipient_leisure, benefit_mult in recipient_benefits
        scenario = run_scenario(
            label = "leisure_R=$(leisure_mult), benefit=$(benefit_mult)",
            leisure_R_post_multiplier = leisure_mult,
            benefit_multiplier = benefit_mult,
            pre_grid_n = 28,
            post_grid_n = 50,
        )
        push!(rows, merge((Sweep = "recipient_leisure_x_benefit", X = leisure_mult, Y = benefit_mult), scenario.summary))
    end

    return DataFrame(rows)
end

function moment_sensitivity()
    rows = NamedTuple[]
    multipliers = [0.90, 0.95, 1.00, 1.05, 1.10]

    for mult in multipliers
        scenario = run_scenario(
            label = "post_N_target_x$(mult)",
            target_post_N = TARGET_POST_N * mult,
            pre_grid_n = 30,
            post_grid_n = 55,
        )
        push!(rows, merge((Sweep = "post_N_target", Value = TARGET_POST_N * mult, Multiplier = mult), scenario.summary))
    end

    for mult in multipliers
        scenario = run_scenario(
            label = "post_R_target_x$(mult)",
            target_post_R = TARGET_POST_R * mult,
            pre_grid_n = 30,
            post_grid_n = 55,
        )
        push!(rows, merge((Sweep = "post_R_target", Value = TARGET_POST_R * mult, Multiplier = mult), scenario.summary))
    end

    for mult in multipliers
        scenario = run_scenario(
            label = "pre_R_target_x$(mult)",
            target_pre_R = TARGET_PRE_R * mult,
            pre_grid_n = 30,
            post_grid_n = 55,
        )
        push!(rows, merge((Sweep = "pre_R_target", Value = TARGET_PRE_R * mult, Multiplier = mult), scenario.summary))
    end

    return DataFrame(rows)
end

function flip_map()
    rows = NamedTuple[]
    benefit_values = [1.5, 2.0, 2.5, 3.0, 3.5, 4.0]
    leisure_values = [0.8, 1.0, 1.5, 2.0, 2.5]

    for benefit_mult in benefit_values, leisure_mult in leisure_values
        scenario = run_scenario(
            label = "benefit=$(benefit_mult), leisure_R=$(leisure_mult)",
            benefit_multiplier = benefit_mult,
            leisure_R_post_multiplier = leisure_mult,
            pre_grid_n = 28,
            post_grid_n = 50,
        )
        push!(rows, merge((BenefitMultiplier = benefit_mult, LeisureRMultiplier = leisure_mult), scenario.summary))
    end

    df = DataFrame(rows)
    df.Interpretation = ifelse.(
        abs.(df.did_full_pp) .> abs.(df.did_benefit_covid_env_pp),
        "DiD overstates benefit effect",
        "DiD understates benefit effect",
    )
    df.ElasticityInterpretation = ifelse.(
        abs.(df.elasticity_full_logdid) .> abs.(df.elasticity_benefit_covid_env_logdid),
        "DiD overstates elasticity",
        "DiD understates elasticity",
    )
    return df
end

function health_shock_decomposition()
    rows = NamedTuple[]
    spec = build_spec()
    pre_template, post_template = model_environments(spec; benefit_multiplier = 2.0)
    pre_env, pre_fit = calibrate_pre_refined(pre_template, spec, TARGET_PRE_N, TARGET_PRE_R; grid_n = 45)

    # health_share_N decomposes the observed N fall in log job finding into:
    # - a search-cost/health component, created by kappa_N, and
    # - a matching/contact component, created by mu.
    # The R-specific kappa shock is then fitted residually to match R's post rate.
    for health_share_N in range(0.0, 1.0, length = 11)
        target_health_only_N = TARGET_PRE_N * exp(health_share_N * log(TARGET_POST_N / TARGET_PRE_N))

        kappa_N = fit_kappa_N_for_target(
            pre_env,
            post_template,
            spec,
            target_health_only_N;
            mu = pre_env.mu,
            grid_n = 50,
        )
        mu_post = fit_mu_for_target_N(
            pre_env,
            post_template,
            spec,
            TARGET_POST_N;
            kappa_N = kappa_N,
            grid_n = 50,
        )
        kappa_R = fit_kappa_R_for_target(
            pre_env,
            post_template,
            spec,
            TARGET_POST_R;
            mu = mu_post,
            kappa_N = kappa_N,
            grid_n = 50,
        )

        full_post_env = copy_env(post_template; mu = mu_post, kappa_N = kappa_N, kappa_R = kappa_R)
        benefit_only_env = copy_env(pre_env; b_R = post_template.b_R)
        covid_only_env = copy_env(full_post_env; b_R = pre_env.b_R)

        full_sim = simulate_path(pre_env, full_post_env, spec)
        benefit_sim = simulate_path(pre_env, benefit_only_env, spec)
        covid_sim = simulate_path(pre_env, covid_only_env, spec)

        did_full = did_series(full_sim)
        did_covid = did_series(covid_sim)
        did_benefit_covid = did_full .- did_covid
        log_denom = benefit_log_change(pre_env, full_post_env)
        log_did_full = log_did_from_pre(full_sim.find_R, full_sim.find_N)
        log_did_benefit_covid = log_did_series(
            full_sim.find_R,
            full_sim.find_N,
            covid_sim.find_R,
            covid_sim.find_N,
        )

        shock = spec.shock_start:spec.shock_end
        push!(rows, (
            HealthShareN = Float64(health_share_N),
            MatchingShareN = 1.0 - Float64(health_share_N),
            mu_pre = pre_env.mu,
            mu_post = mu_post,
            kappa_pre = pre_env.kappa_N,
            kappa_N_post = kappa_N,
            kappa_R_post = kappa_R,
            kappa_N_multiplier = kappa_N / pre_env.kappa_N,
            kappa_R_multiplier = kappa_R / pre_env.kappa_R,
            R_health_premium = kappa_R / kappa_N,
            did_full_pp = 100 * mean(did_full[shock]),
            did_benefit_covid_env_pp = 100 * mean(did_benefit_covid[shock]),
            did_covid_only_pp = 100 * mean(did_covid[shock]),
            elasticity_full_logdid = mean(log_did_full[shock]) / log_denom,
            elasticity_benefit_covid_env_logdid = mean(log_did_benefit_covid[shock]) / log_denom,
            elasticity_ratio_full_to_marginal = mean(log_did_full[shock]) / mean(log_did_benefit_covid[shock]),
            fit_pre_N = pre_fit.find_N,
            fit_pre_R = pre_fit.find_R,
            fit_post_N = period_means(full_sim.find_N, full_sim.find_R, spec).shock_N,
            fit_post_R = period_means(full_sim.find_N, full_sim.find_R, spec).shock_R,
        ))
    end

    return DataFrame(rows)
end

function health_premium_grid()
    rows = NamedTuple[]
    spec = build_spec()
    pre_template, post_template = model_environments(spec; benefit_multiplier = 2.0)
    pre_env, _ = calibrate_pre_refined(pre_template, spec, TARGET_PRE_N, TARGET_PRE_R; grid_n = 45)

    health_shares = [0.0, 0.5, 1.0]
    premiums = [1.0, 1.25, 1.5, 1.75, 1.9, 2.0, 2.25, 2.5]

    for health_share_N in health_shares
        target_health_only_N = TARGET_PRE_N * exp(health_share_N * log(TARGET_POST_N / TARGET_PRE_N))
        kappa_N = fit_kappa_N_for_target(
            pre_env,
            post_template,
            spec,
            target_health_only_N;
            mu = pre_env.mu,
            grid_n = 50,
        )
        mu_post = fit_mu_for_target_N(
            pre_env,
            post_template,
            spec,
            TARGET_POST_N;
            kappa_N = kappa_N,
            grid_n = 50,
        )

        for premium in premiums
            kappa_R = premium * kappa_N
            full_post_env = copy_env(post_template; mu = mu_post, kappa_N = kappa_N, kappa_R = kappa_R)
            covid_only_env = copy_env(full_post_env; b_R = pre_env.b_R)

            full_sim = simulate_path(pre_env, full_post_env, spec)
            covid_sim = simulate_path(pre_env, covid_only_env, spec)
            means = period_means(full_sim.find_N, full_sim.find_R, spec)

            did_full = did_series(full_sim)
            did_covid = did_series(covid_sim)
            did_benefit_covid = did_full .- did_covid
            log_denom = benefit_log_change(pre_env, full_post_env)
            log_did_full = log_did_from_pre(full_sim.find_R, full_sim.find_N)
            log_did_benefit_covid = log_did_series(
                full_sim.find_R,
                full_sim.find_N,
                covid_sim.find_R,
                covid_sim.find_N,
            )
            shock = spec.shock_start:spec.shock_end

            push!(rows, (
                HealthShareN = health_share_N,
                RHealthPremium = premium,
                mu_post = mu_post,
                kappa_N_multiplier = kappa_N / pre_env.kappa_N,
                kappa_R_multiplier = kappa_R / pre_env.kappa_R,
                predicted_post_N = means.shock_N,
                predicted_post_R = means.shock_R,
                R_target_gap_pp = 100 * (means.shock_R - TARGET_POST_R),
                did_full_pp = 100 * mean(did_full[shock]),
                did_benefit_covid_env_pp = 100 * mean(did_benefit_covid[shock]),
                target_full_did_pp = target_full_did_pp(),
                elasticity_full_logdid = mean(log_did_full[shock]) / log_denom,
                elasticity_benefit_covid_env_logdid = mean(log_did_benefit_covid[shock]) / log_denom,
                elasticity_ratio_full_to_marginal = mean(log_did_full[shock]) / mean(log_did_benefit_covid[shock]),
                target_full_elasticity_logdid = target_full_logdid_elasticity(),
                target_to_structural_elasticity_ratio =
                    target_full_logdid_elasticity() / (mean(log_did_benefit_covid[shock]) / log_denom),
            ))
        end
    end

    return DataFrame(rows)
end

function plot_sensitivity(one_way::DataFrame, paired::DataFrame)
    for sweep in unique(one_way.Sweep)
        sub = sort(one_way[one_way.Sweep .== sweep, :], :Value)
        plt = plot(
            sub.Value,
            sub.did_full_pp,
            label = "Full DiD",
            lw = 2,
            xlabel = sweep,
            ylabel = "Shock-period DiD/effect (p.p.)",
            title = "Sensitivity: $(sweep)",
        )
        plot!(plt, sub.Value, sub.did_benefit_covid_env_pp, label = "Benefit in COVID market", lw = 2, ls = :dash)
        plot!(plt, sub.Value, sub.did_benefit_normal_pp, label = "Benefit in normal market", lw = 2, ls = :dot)
        hline!(plt, [0.0], label = "", color = :black, alpha = 0.3)
        savefig(plt, "12b_sensitivity_$(sweep).png")
    end

    ratio = one_way.observed_did_over_marginal
    plt_ratio = scatter(
        one_way.did_benefit_covid_env_pp,
        one_way.did_full_pp,
        group = one_way.Sweep,
        xlabel = "Benefit effect in COVID market (p.p.)",
        ylabel = "Full DiD (p.p.)",
        title = "When Does the DiD Overstate the Benefit Effect?",
        legend = :outerright,
    )
    plot!(plt_ratio, [-8, 2], [-8, 2], label = "45 degree line", color = :black, ls = :dash)
    savefig(plt_ratio, "12b_sensitivity_full_vs_marginal_benefit.png")

    one_way.AbsRatio = abs.(ratio)
    plt_abs_ratio = scatter(
        1:nrow(one_way),
        one_way.AbsRatio,
        group = one_way.Sweep,
        xlabel = "Sensitivity scenario",
        ylabel = "|Full DiD / benefit effect in COVID market|",
        title = "Magnitude of Reduced-Form DiD Relative to Benefit Effect",
        legend = :outerright,
    )
    hline!(plt_abs_ratio, [1.0], label = "Equal magnitude", color = :black, ls = :dash)
    savefig(plt_abs_ratio, "12b_sensitivity_over_under_ratio.png")

    plt_elasticity = scatter(
        one_way.elasticity_benefit_covid_env_logdid,
        one_way.elasticity_full_logdid,
        group = one_way.Sweep,
        xlabel = "Structural benefit elasticity in COVID market",
        ylabel = "Full DiD-implied elasticity",
        title = "Elasticity Sensitivity: Full DiD vs Benefit Response",
        legend = :outerright,
    )
    plot!(plt_elasticity, [-8, 1], [-8, 1], label = "Equal elasticity", color = :black, ls = :dash)
    savefig(plt_elasticity, "12b_sensitivity_elasticity_full_vs_marginal.png")

    plt_elasticity_ratio = scatter(
        1:nrow(one_way),
        abs.(one_way.elasticity_ratio_full_to_marginal),
        group = one_way.Sweep,
        xlabel = "Sensitivity scenario",
        ylabel = "|Full elasticity / structural benefit elasticity|",
        title = "Elasticity Over/Understatement Across Assumptions",
        legend = :outerright,
    )
    hline!(plt_elasticity_ratio, [1.0], label = "Equal magnitude", color = :black, ls = :dash)
    savefig(plt_elasticity_ratio, "12b_sensitivity_elasticity_ratio.png")

    for sweep in unique(paired.Sweep)
        sub = paired[paired.Sweep .== sweep, :]
        plt_pair = scatter(
            sub.X,
            sub.Y,
            marker_z = sub.observed_did_over_marginal,
            xlabel = sweep == "alpha_x_common_cost" ? "Matching elasticity wrt jobseekers" : "R leisure multiplier",
            ylabel = sweep == "alpha_x_common_cost" ? "Common search-cost multiplier" : "Benefit multiplier",
            title = "Full DiD / COVID-market Benefit Effect: $(sweep)",
            colorbar_title = "Ratio",
            label = "",
        )
        savefig(plt_pair, "12b_paired_sensitivity_$(sweep).png")

        plt_pair_elasticity = scatter(
            sub.X,
            sub.Y,
            marker_z = sub.elasticity_ratio_full_to_marginal,
            xlabel = sweep == "alpha_x_common_cost" ? "Matching elasticity wrt jobseekers" : "R leisure multiplier",
            ylabel = sweep == "alpha_x_common_cost" ? "Common search-cost multiplier" : "Benefit multiplier",
            title = "Elasticity Ratio: $(sweep)",
            colorbar_title = "Ratio",
            label = "",
        )
        savefig(plt_pair_elasticity, "12b_paired_sensitivity_elasticity_$(sweep).png")
    end
end

function plot_moment_sensitivity(moment_df::DataFrame, flip_df::DataFrame)
    for sweep in unique(moment_df.Sweep)
        sub = sort(moment_df[moment_df.Sweep .== sweep, :], :Multiplier)
        plt = plot(
            sub.Multiplier,
            sub.did_full_pp,
            label = "Full DiD",
            lw = 2,
            xlabel = "$(sweep) multiplier",
            ylabel = "Shock-period DiD/effect (p.p.)",
            title = "Moment Sensitivity: $(sweep)",
        )
        plot!(plt, sub.Multiplier, sub.did_benefit_covid_env_pp, label = "Benefit in COVID market", lw = 2, ls = :dash)
        plot!(plt, sub.Multiplier, sub.did_covid_only_pp, label = "COVID only", lw = 2, ls = :dot)
        hline!(plt, [0.0], label = "", color = :black, alpha = 0.3)
        savefig(plt, "12b_moment_sensitivity_$(sweep).png")
    end

    plt_flip = scatter(
        flip_df.BenefitMultiplier,
        flip_df.LeisureRMultiplier,
        marker_z = abs.(flip_df.did_full_pp) ./ abs.(flip_df.did_benefit_covid_env_pp),
        xlabel = "Benefit multiplier",
        ylabel = "R leisure/outside-option multiplier",
        title = "Flip Map: |Full DiD| / |Benefit Effect in COVID Market|",
        colorbar_title = "Ratio",
        label = "",
    )
    savefig(plt_flip, "12b_flip_map_over_under.png")

    plt_flip2 = scatter(
        flip_df.did_benefit_covid_env_pp,
        flip_df.did_full_pp,
        group = flip_df.Interpretation,
        xlabel = "Benefit effect in COVID market (p.p.)",
        ylabel = "Full DiD (p.p.)",
        title = "Overestimate vs Underestimate Regions",
        legend = :outerright,
    )
    plot!(plt_flip2, [-8, 0], [-8, 0], label = "Equal magnitude", color = :black, ls = :dash)
    savefig(plt_flip2, "12b_flip_map_full_vs_benefit.png")

    plt_flip_elasticity = scatter(
        flip_df.BenefitMultiplier,
        flip_df.LeisureRMultiplier,
        marker_z = abs.(flip_df.elasticity_ratio_full_to_marginal),
        xlabel = "Benefit multiplier",
        ylabel = "R leisure/outside-option multiplier",
        title = "Elasticity Flip Map: |Full| / |Structural Benefit|",
        colorbar_title = "Elasticity ratio",
        label = "",
    )
    savefig(plt_flip_elasticity, "12b_flip_map_elasticity_ratio.png")

    plt_moment_elasticity = scatter(
        moment_df.elasticity_benefit_covid_env_logdid,
        moment_df.elasticity_full_logdid,
        group = moment_df.Sweep,
        xlabel = "Structural benefit elasticity in COVID market",
        ylabel = "Full DiD-implied elasticity",
        title = "Moment Sensitivity: Elasticity Over/Understatement",
        legend = :outerright,
    )
    plot!(plt_moment_elasticity, [-8, 1], [-8, 1], label = "Equal elasticity", color = :black, ls = :dash)
    savefig(plt_moment_elasticity, "12b_moment_sensitivity_elasticity_full_vs_marginal.png")
end

function plot_elasticity_driver_summary(one_way::DataFrame, moment_df::DataFrame)
    one_way_ranges = combine(
        groupby(one_way, :Sweep),
        :elasticity_ratio_full_to_marginal => minimum => :MinRatio,
        :elasticity_ratio_full_to_marginal => maximum => :MaxRatio,
        :elasticity_ratio_full_to_marginal => mean => :MeanRatio,
    )
    one_way_ranges.Source .= "Assumption"

    moment_ranges = combine(
        groupby(moment_df, :Sweep),
        :elasticity_ratio_full_to_marginal => minimum => :MinRatio,
        :elasticity_ratio_full_to_marginal => maximum => :MaxRatio,
        :elasticity_ratio_full_to_marginal => mean => :MeanRatio,
    )
    moment_ranges.Source .= "Moment"

    ranges = vcat(one_way_ranges, moment_ranges)
    ranges.Range = ranges.MaxRatio .- ranges.MinRatio
    sort!(ranges, :Range, rev = true)
    CSV.write("12b_elasticity_driver_ranges.csv", ranges)

    labels = string.(ranges.Sweep)
    y = 1:nrow(ranges)
    plt = plot(
        xlim = (0, maximum(ranges.MaxRatio) * 1.08),
        ylim = (0.5, nrow(ranges) + 0.5),
        yticks = (y, labels),
        xlabel = "Full DiD elasticity / structural benefit elasticity",
        ylabel = "",
        title = "Which Assumptions Drive Elasticity Overstatement?",
        legend = :bottomright,
        size = (1050, 650),
    )

    for i in y
        plot!(plt, [ranges.MinRatio[i], ranges.MaxRatio[i]], [i, i],
              label = i == 1 ? "Sensitivity range" : "", lw = 8,
              color = ranges.Source[i] == "Moment" ? :darkorange : :steelblue)
        scatter!(plt, [ranges.MeanRatio[i]], [i],
                 label = i == 1 ? "Mean across tested values" : "",
                 color = :black, markersize = 4)
    end

    vline!(plt, [1.0], label = "No over/understatement", color = :black, ls = :dash)
    savefig(plt, "12b_elasticity_driver_ranges.png")

    return ranges
end

function plot_health_shock_decomposition(health_df::DataFrame)
    plt_elasticity = plot(
        health_df.HealthShareN,
        health_df.elasticity_full_logdid,
        label = "Full DiD-implied elasticity",
        lw = 2,
        xlabel = "Share of N's COVID job-finding fall attributed to health/search cost",
        ylabel = "Elasticity wrt benefit increase",
        title = "Elasticity Sensitivity to Nature of Health Shock",
    )
    plot!(plt_elasticity, health_df.HealthShareN, health_df.elasticity_benefit_covid_env_logdid,
          label = "Structural benefit elasticity", lw = 2, ls = :dash)
    hline!(plt_elasticity, [0.0], label = "", color = :black, alpha = 0.3)
    savefig(plt_elasticity, "12b_health_shock_elasticity_sensitivity.png")

    plt_ratio = plot(
        health_df.HealthShareN,
        abs.(health_df.elasticity_ratio_full_to_marginal),
        label = "Elasticity overstatement ratio",
        lw = 2,
        xlabel = "Share of N's COVID job-finding fall attributed to health/search cost",
        ylabel = "|Full elasticity / structural benefit elasticity|",
        title = "How Health-Shock Assumptions Drive Overstatement",
    )
    hline!(plt_ratio, [1.0], label = "Equal magnitude", color = :black, ls = :dash)
    savefig(plt_ratio, "12b_health_shock_elasticity_ratio.png")

    plt_mechanism = plot(
        health_df.HealthShareN,
        health_df.kappa_N_multiplier,
        label = "N health/search-cost multiplier",
        lw = 2,
        xlabel = "Share of N's COVID job-finding fall attributed to health/search cost",
        ylabel = "Multiplier",
        title = "Implied Health/Search-Cost Shocks",
    )
    plot!(plt_mechanism, health_df.HealthShareN, health_df.kappa_R_multiplier,
          label = "R health/search-cost multiplier", lw = 2, ls = :dash)
    plot!(plt_mechanism, health_df.HealthShareN, health_df.R_health_premium,
          label = "R/N health premium", lw = 2, ls = :dot)
    savefig(plt_mechanism, "12b_health_shock_implied_kappa.png")
end

function plot_health_premium_grid(premium_df::DataFrame)
    plt_fit = plot(
        xlabel = "R health/search-cost premium over N",
        ylabel = "Predicted R post job-finding rate (%)",
        title = "How Much Differential Health Shock Is Needed?",
    )
    for share in unique(premium_df.HealthShareN)
        sub = premium_df[premium_df.HealthShareN .== share, :]
        plot!(plt_fit, sub.RHealthPremium, 100 .* sub.predicted_post_R,
              label = "N health share = $(round(share, digits = 2))", lw = 2)
    end
    hline!(plt_fit, [100 * TARGET_POST_R], label = "Observed R post target", color = :black, ls = :dash)
    savefig(plt_fit, "12b_health_premium_fit_to_R_target.png")

    plt_ratio = plot(
        xlabel = "R health/search-cost premium over N",
        ylabel = "|Elasticity ratio|",
        title = "Observed vs Model-Predicted Elasticity Overstatement",
    )
    for share in unique(premium_df.HealthShareN)
        sub = premium_df[premium_df.HealthShareN .== share, :]
        plot!(plt_ratio, sub.RHealthPremium, abs.(sub.elasticity_ratio_full_to_marginal),
              label = "Model-predicted full, N share = $(round(share, digits = 2))", lw = 2)
        plot!(plt_ratio, sub.RHealthPremium, abs.(sub.target_to_structural_elasticity_ratio),
              label = "Target-coded full, N share = $(round(share, digits = 2))", lw = 2, ls = :dash)
    end
    hline!(plt_ratio, [1.0], label = "No over/understatement", color = :black, ls = :dash)
    savefig(plt_ratio, "12b_health_premium_elasticity_ratio.png")

    plt_full = plot(
        xlabel = "R health/search-cost premium over N",
        ylabel = "Full DiD-implied elasticity",
        title = "Observed Full DiD vs Model-Predicted Full DiD",
    )
    for share in unique(premium_df.HealthShareN)
        sub = premium_df[premium_df.HealthShareN .== share, :]
        plot!(plt_full, sub.RHealthPremium, sub.elasticity_full_logdid,
              label = "Model-predicted full, N share = $(round(share, digits = 2))", lw = 2)
    end
    hline!(plt_full, [target_full_logdid_elasticity()],
           label = "Target-coded empirical full DiD", color = :black, ls = :dash)
    savefig(plt_full, "12b_health_premium_full_elasticity_target.png")
end

function print_expanded_summary(baseline)
    s = baseline.summary
    @printf("\nBaseline expanded decomposition (shock-period averages, p.p.)\n")
    @printf("  Full DiD:                           %7.3f\n", s.did_full_pp)
    hasproperty(s, :target_full_did_pp) && @printf("  Target-coded empirical DiD:         %7.3f\n", s.target_full_did_pp)
    @printf("  Benefit-only in normal market:      %7.3f\n", s.did_benefit_normal_pp)
    @printf("  COVID-only DiD:                     %7.3f\n", s.did_covid_only_pp)
    @printf("  Benefit effect in COVID market:     %7.3f\n", s.did_benefit_covid_env_pp)
    hasproperty(s, :did_interaction_pp) && @printf("  Interaction residual:               %7.3f\n", s.did_interaction_pp)
    hasproperty(s, :observed_did_over_marginal) && @printf("  Full / COVID-market benefit ratio:  %7.3f\n", s.observed_did_over_marginal)
    hasproperty(s, :benefit_weight) && @printf("  Benefit responsiveness weight:      %7.3f\n", s.benefit_weight)
    hasproperty(s, :post_R_target_gap_pp) && @printf("  Post R target gap:                  %7.3f\n", s.post_R_target_gap_pp)
    @printf("\nBaseline log-DiD elasticities wrt benefit increase\n")
    @printf("  Full DiD-implied elasticity:        %7.3f\n", s.elasticity_full_logdid)
    hasproperty(s, :target_full_elasticity_logdid) && @printf("  Target-coded full elasticity:       %7.3f\n", s.target_full_elasticity_logdid)
    @printf("  Structural benefit elasticity:      %7.3f\n", s.elasticity_benefit_covid_env_logdid)
    hasproperty(s, :elasticity_ratio_full_to_marginal) && @printf("  Full / structural elasticity ratio: %7.3f\n", s.elasticity_ratio_full_to_marginal)
    hasproperty(s, :target_to_structural_elasticity_ratio) && @printf("  Target / structural ratio:          %7.3f\n", s.target_to_structural_elasticity_ratio)
end

function run_expanded()
    println("\nCalibrating benefit responsiveness under equal health shocks...")
    baseline, benefit_weight_path = calibrate_benefit_weight_equal_health(
        grid_n = 25,
        refine_grid_n = 45,
        pre_grid_n = 30,
        post_grid_n = 50,
    )
    print_expanded_summary(baseline)

    CSV.write("12b_did_marginal_benefit_covid_market.csv", baseline.series)
    CSV.write("12b_benefit_weight_calibration.csv", benefit_weight_path)
    CSV.write("12b_literature_moments.csv", literature_moments())
    plot_baseline_series(baseline)
    plot_benefit_weight_calibration(benefit_weight_path, baseline)

    println("\nEstimating benefit responsiveness across differential health-shock assumptions...")
    health_benefit_summary, health_benefit_paths = health_premium_benefit_responsiveness_grid()
    CSV.write("12b_health_premium_benefit_responsiveness.csv", health_benefit_summary)
    CSV.write("12b_health_premium_benefit_responsiveness_paths.csv", health_benefit_paths)
    plot_health_premium_benefit_responsiveness(health_benefit_summary, health_benefit_paths)

    println("\nComputing implied no-COVID benefit elasticities...")
    normal_market = normal_market_elasticity_from_health_assumptions(health_benefit_summary)
    CSV.write("12b_normal_market_elasticity.csv", normal_market)
    plot_normal_market_elasticity(normal_market)

    println("\nRunning one-way sensitivity sweeps...")
    one_way = one_way_sensitivity()
    CSV.write("12b_sensitivity_one_way.csv", one_way)

    println("\nRunning paired sensitivity sweeps...")
    paired = paired_sensitivity()
    CSV.write("12b_sensitivity_paired.csv", paired)

    plot_sensitivity(one_way, paired)

    println("\nRunning moment sensitivity sweeps...")
    moments = moment_sensitivity()
    CSV.write("12b_sensitivity_moments.csv", moments)

    println("\nRunning over/underestimate flip map...")
    flips = flip_map()
    CSV.write("12b_flip_map.csv", flips)

    plot_moment_sensitivity(moments, flips)
    driver_ranges = plot_elasticity_driver_summary(one_way, moments)

    println("\nRunning health-shock decomposition sensitivity...")
    health = health_shock_decomposition()
    CSV.write("12b_health_shock_decomposition.csv", health)
    plot_health_shock_decomposition(health)

    premium_grid = health_premium_grid()
    CSV.write("12b_health_premium_grid.csv", premium_grid)
    plot_health_premium_grid(premium_grid)

    println("\nSaved expanded outputs:")
    println("  12b_did_marginal_benefit_covid_market.csv")
    println("  12b_benefit_weight_calibration.csv")
    println("  12b_health_premium_benefit_responsiveness.csv")
    println("  12b_health_premium_benefit_responsiveness_paths.csv")
    println("  12b_normal_market_elasticity.csv")
    println("  12b_literature_moments.csv")
    println("  12b_sensitivity_one_way.csv")
    println("  12b_sensitivity_paired.csv")
    println("  12b_sensitivity_moments.csv")
    println("  12b_flip_map.csv")
    println("  12b_elasticity_driver_ranges.csv")
    println("  12b_health_shock_decomposition.csv")
    println("  12b_health_premium_grid.csv")
    println("  12b_did_marginal_benefit_covid_market.png")
    println("  12b_benefit_weight_*.png")
    println("  12b_health_premium_required_benefit_weight.png")
    println("  12b_health_premium_estimated_benefit_elasticity.png")
    println("  12b_normal_market_benefit_elasticity.png")
    println("  12b_elasticity_marginal_benefit_covid_market.png")
    println("  12b_elasticity_driver_ranges.png")
    println("  12b_health_shock_elasticity_*.png")
    println("  12b_health_premium_*.png")
    println("  12b_sensitivity_elasticity_*.png")
    println("  12b_sensitivity_*.png")

    return (baseline = baseline, benefit_weight_path = benefit_weight_path,
            health_benefit_summary = health_benefit_summary,
            health_benefit_paths = health_benefit_paths,
            normal_market = normal_market,
            one_way = one_way, paired = paired,
            moments = moments, flips = flips, driver_ranges = driver_ranges,
            health = health, premium_grid = premium_grid)
end

if abspath(PROGRAM_FILE) == @__FILE__
    run_expanded()
end
