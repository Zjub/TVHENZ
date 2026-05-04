# 14_joint_margin_structural_model.jl
#
# Two-agent search-and-matching model that jointly disciplines job finding and
# separations. This is intended as the first publishability upgrade from 13_:
# keep the two-agent framework, but force the model to speak to both empirical
# DiD margins in the paper.

using CSV
using DataFrames
using Optim
using Printf
using Statistics

Base.@kwdef struct ModelSpec
    beta::Float64 = 0.99
    firm_delta::Float64 = 0.01
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
    # Maps a COVID work-disutility flow shock into separations. This is not
    # chosen to fit AUS post job finding in the restricted calibration below.
    sep_health_sensitivity::Float64 = 0.5
    search_curvature::Float64 = 7.0
    max_search::Float64 = 50.0
    probability_cap::Float64 = 0.999
    flow_floor::Float64 = 1e-8
    T::Int = 60
    shock_start::Int = 12
    shock_end::Int = 36
end

Base.@kwdef struct Targets
    # Paper Table 3 implied JFR cells.
    f_N_pre::Float64 = 0.1002
    f_R_pre::Float64 = 0.0868
    f_N_post::Float64 = 0.0823
    f_R_post::Float64 = 0.0518

    # Paper Table 4 implied separation cells.
    sep_N_pre::Float64 = 0.0480
    sep_R_pre::Float64 = 0.0484
    sep_N_post::Float64 = 0.0550
    sep_R_post::Float64 = 0.0926
end

Base.@kwdef struct Environment
    mu::Float64
    kappa::Float64
    b_R::Float64
    b_N::Float64 = 0.0
    health_R::Float64
    health_N::Float64
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
    flow_surplus_R::Float64
    flow_surplus_N::Float64
    value_surplus_R::Float64
    value_surplus_N::Float64
end

benefit_pre(spec::ModelSpec) = spec.base_replacement_rate * spec.wage
benefit_post(spec::ModelSpec) = spec.benefit_multiplier * benefit_pre(spec)
logistic(x) = 1.0 / (1.0 + exp(-x))

function utility(flow::Float64, spec::ModelSpec)
    return max(flow, spec.flow_floor)
end

outside_R(env::Environment, spec::ModelSpec) = env.b_R + spec.leisure_value
outside_N(env::Environment, spec::ModelSpec) = env.b_N + spec.leisure_value
work_flow_R(env::Environment, spec::ModelSpec) = spec.wage - env.health_R
work_flow_N(env::Environment, spec::ModelSpec) = spec.wage - env.health_N
flow_surplus_R(env::Environment, spec::ModelSpec) =
    work_flow_R(env, spec) - outside_R(env, spec)
flow_surplus_N(env::Environment, spec::ModelSpec) =
    work_flow_N(env, spec) - outside_N(env, spec)

function accept_prob(flow_surplus::Float64, spec::ModelSpec)
    return clamp(logistic((flow_surplus - spec.offer_threshold) /
                          spec.offer_dispersion), 0.0, 1.0)
end

function sep_shape(flow_surplus::Float64, spec::ModelSpec)
    return logistic((spec.separation_threshold - flow_surplus) /
                    spec.separation_dispersion)
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
    theta = ((spec.beta * env.mu * firm_value) /
             spec.vacancy_cost)^(1.0 / spec.match_elasticity)
    contact = min(env.mu * theta^(1.0 - spec.match_elasticity),
                  spec.probability_cap)
    return theta, contact
end

function offer_probability(search::Float64, contact::Float64, spec::ModelSpec)
    return clamp(1.0 - exp(-contact * max(search, 0.0)),
                 0.0, spec.probability_cap)
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

function choose_search(next_surplus::Float64, contact::Float64,
                       accept::Float64, kappa::Float64,
                       spec::ModelSpec)
    if next_surplus <= 0.0 || contact <= 0.0 || accept <= 0.0
        return 0.0
    end

    marginal_gain(s) =
        spec.beta * accept * contact * exp(-contact * s) * next_surplus
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

function bellman_update(env::Environment, next::ValueState,
                        sep::SeparationParams, spec::ModelSpec)
    theta, contact = labor_market(env, spec)

    fs_R = flow_surplus_R(env, spec)
    fs_N = flow_surplus_N(env, spec)
    accept_R = accept_prob(fs_R, spec)
    accept_N = accept_prob(fs_N, spec)
    sep_R = separation_rate(fs_R, env.health_R, sep, spec)
    sep_N = separation_rate(fs_N, env.health_N, sep, spec)

    value_surplus_R = next.W_R - next.U_R
    value_surplus_N = next.W_N - next.U_N
    search_R = choose_search(value_surplus_R, contact, accept_R,
                             env.kappa, spec)
    search_N = choose_search(value_surplus_N, contact, accept_N,
                             env.kappa, spec)
    find_R = job_finding_probability(search_R, contact, accept_R, spec)
    find_N = job_finding_probability(search_N, contact, accept_N, spec)

    W_R = utility(work_flow_R(env, spec), spec) +
          spec.beta * ((1.0 - sep_R) * next.W_R + sep_R * next.U_R)
    W_N = utility(work_flow_N(env, spec), spec) +
          spec.beta * ((1.0 - sep_N) * next.W_N + sep_N * next.U_N)
    U_R = utility(outside_R(env, spec), spec) -
          search_cost(search_R, env.kappa, spec) +
          spec.beta * (find_R * next.W_R + (1.0 - find_R) * next.U_R)
    U_N = utility(outside_N(env, spec), spec) -
          search_cost(search_N, env.kappa, spec) +
          spec.beta * (find_N * next.W_N + (1.0 - find_N) * next.U_N)

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
        flow_surplus_R = fs_R,
        flow_surplus_N = fs_N,
        value_surplus_R = value_surplus_R,
        value_surplus_N = value_surplus_N,
    )
end

function stationary_values(env::Environment, sep::SeparationParams,
                           spec::ModelSpec;
                           tol::Float64 = 1e-9, max_iter::Int = 20_000)
    guess = ValueState(
        utility(work_flow_R(env, spec), spec) / (1.0 - spec.beta),
        utility(outside_R(env, spec), spec) / (1.0 - spec.beta),
        utility(work_flow_N(env, spec), spec) / (1.0 - spec.beta),
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
                            sep::SeparationParams, spec::ModelSpec)
    values = Vector{ValueState}(undef, spec.T + 1)
    pre_ss = stationary_values(pre_env, sep, spec)
    values[spec.T + 1] = pre_ss

    for t in spec.T:-1:1
        env_t = environment_for_period(t, pre_env, shock_env, spec)
        next_value = t < spec.shock_start ? pre_ss : values[t + 1]
        values[t] = bellman_update(env_t, next_value, sep, spec).value
    end

    return values, pre_ss
end

function simulate_path(pre_env::Environment, shock_env::Environment,
                       sep::SeparationParams, spec::ModelSpec)
    values, pre_ss = compute_value_path(pre_env, shock_env, sep, spec)

    rows = NamedTuple[]
    for t in 1:spec.T
        env_t = environment_for_period(t, pre_env, shock_env, spec)
        decision_next = t < spec.shock_start ? pre_ss : values[t + 1]
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
            contact = out.contact,
            theta = out.theta,
            flow_surplus_R = out.flow_surplus_R,
            flow_surplus_N = out.flow_surplus_N,
            value_surplus_R = out.value_surplus_R,
            value_surplus_N = out.value_surplus_N,
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
        contact = mean(sub.contact),
        theta = mean(sub.theta),
        flow_surplus_R = mean(sub.flow_surplus_R),
        flow_surplus_N = mean(sub.flow_surplus_N),
        value_surplus_R = mean(sub.value_surplus_R),
        value_surplus_N = mean(sub.value_surplus_N),
    )
end

function pre_environment(mu::Float64, kappa::Float64, spec::ModelSpec)
    return Environment(
        mu = mu,
        kappa = kappa,
        b_R = benefit_pre(spec),
        health_R = 0.0,
        health_N = 0.0,
    )
end

function shock_environment(mu::Float64, kappa::Float64, spec::ModelSpec;
                           b_R::Float64, health_R::Float64,
                           health_N::Float64)
    return Environment(
        mu = mu,
        kappa = kappa,
        b_R = b_R,
        health_R = health_R,
        health_N = health_N,
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

function pre_fit_objective(x, sep::SeparationParams,
                           spec::ModelSpec, targets::Targets)
    mu, kappa = exp(x[1]), exp(x[2])
    if !(0.001 <= mu <= 3.0 && 0.001 <= kappa <= 50.0)
        return 1e4
    end

    env = pre_environment(mu, kappa, spec)
    values = stationary_values(env, sep, spec)
    out = bellman_update(env, values, sep, spec)
    return (out.find_N - targets.f_N_pre)^2 +
           (out.find_R - targets.f_R_pre)^2
end

function calibrate_pre(sep::SeparationParams,
                       spec::ModelSpec, targets::Targets)
    starts = [[log(0.05), log(0.5)],
              [log(0.10), log(1.0)],
              [log(0.20), log(2.0)],
              [log(0.40), log(5.0)]]
    best_x = starts[1]
    best_loss = Inf

    for start in starts
        opt = optimize(x -> pre_fit_objective(x, sep, spec, targets),
                       start, NelderMead(),
                       Optim.Options(iterations = 700))
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
        find_R = out.find_R,
        find_N = out.find_N,
        sep_R = out.sep_R,
        sep_N = out.sep_N,
        search_R = out.search_R,
        search_N = out.search_N,
        accept_R = out.accept_R,
        accept_N = out.accept_N,
        contact = out.contact,
        theta = out.theta,
    )
end

function group_separation(env::Environment, sep::SeparationParams,
                          spec::ModelSpec, group::Symbol)
    if group == :R
        return separation_rate(flow_surplus_R(env, spec),
                               env.health_R, sep, spec)
    elseif group == :N
        return separation_rate(flow_surplus_N(env, spec),
                               env.health_N, sep, spec)
    else
        error("group must be :R or :N")
    end
end

function fit_health_for_sep(group::Symbol, target_sep::Float64,
                            mu::Float64, kappa::Float64,
                            b_R::Float64, fixed_health_other::Float64,
                            sep::SeparationParams, spec::ModelSpec)
    objective(h) = begin
        env = group == :R ?
            shock_environment(mu, kappa, spec;
                              b_R = b_R,
                              health_R = h,
                              health_N = fixed_health_other) :
            shock_environment(mu, kappa, spec;
                              b_R = b_R,
                              health_R = fixed_health_other,
                              health_N = h)
        (group_separation(env, sep, spec, group) - target_sep)^2
    end

    opt = optimize(objective, 0.0, 0.6)
    return Optim.minimizer(opt)
end

function post_fit_objective(x, pre_env::Environment,
                            health_R::Float64, health_N::Float64,
                            sep::SeparationParams,
                            spec::ModelSpec, targets::Targets)
    mu, kappa = exp(x[1]), exp(x[2])
    if !(0.001 <= mu <= 3.0 && 0.001 <= kappa <= 80.0)
        return 1e4
    end

    shock_env = shock_environment(mu, kappa, spec;
                                  b_R = benefit_post(spec),
                                  health_R = health_R,
                                  health_N = health_N)
    means = shock_means(simulate_path(pre_env, shock_env, sep, spec))
    return (means.find_N - targets.f_N_post)^2 +
           (means.find_R - targets.f_R_post)^2
end

function calibrate_post(pre_calib, sep::SeparationParams,
                        spec::ModelSpec, targets::Targets)
    # Health/work-disutility shifters are pinned down by separation cells.
    health_N = fit_health_for_sep(:N, targets.sep_N_post,
                                  pre_calib.mu, pre_calib.kappa,
                                  benefit_post(spec), 0.0,
                                  sep, spec)
    health_R = fit_health_for_sep(:R, targets.sep_R_post,
                                  pre_calib.mu, pre_calib.kappa,
                                  benefit_post(spec), health_N,
                                  sep, spec)

    starts = [[log(pre_calib.mu), log(pre_calib.kappa)],
              [log(0.05), log(0.5)],
              [log(0.10), log(1.0)],
              [log(0.20), log(2.0)],
              [log(0.40), log(5.0)]]
    best_x = starts[1]
    best_loss = Inf

    for start in starts
        opt = optimize(x -> post_fit_objective(x, pre_calib.env,
                                               health_R, health_N,
                                               sep, spec, targets),
                       start, NelderMead(),
                       Optim.Options(iterations = 900))
        if Optim.minimum(opt) < best_loss
            best_loss = Optim.minimum(opt)
            best_x = Optim.minimizer(opt)
        end
    end

    mu, kappa = exp(best_x[1]), exp(best_x[2])
    env = shock_environment(mu, kappa, spec;
                            b_R = benefit_post(spec),
                            health_R = health_R,
                            health_N = health_N)
    means = shock_means(simulate_path(pre_calib.env, env, sep, spec))

    return (
        env = env,
        mu = mu,
        kappa = kappa,
        health_R = health_R,
        health_N = health_N,
        loss = best_loss,
        find_R = means.find_R,
        find_N = means.find_N,
        sep_R = means.sep_R,
        sep_N = means.sep_N,
        search_R = means.search_R,
        search_N = means.search_N,
        accept_R = means.accept_R,
        accept_N = means.accept_N,
        contact = means.contact,
        theta = means.theta,
    )
end

did_pp(pre_R, pre_N, post_R, post_N) =
    100.0 * ((post_R - post_N) - (pre_R - pre_N))

function scenario_means(name::String, pre_calib, shock_env::Environment,
                        sep::SeparationParams, spec::ModelSpec)
    means = shock_means(simulate_path(pre_calib.env, shock_env, sep, spec))
    return merge((scenario = name,), means)
end

function build_decomposition(pre_calib, post_calib,
                             sep::SeparationParams,
                             spec::ModelSpec, targets::Targets)
    benefit_only_env = shock_environment(pre_calib.mu, pre_calib.kappa, spec;
                                         b_R = benefit_post(spec),
                                         health_R = 0.0,
                                         health_N = 0.0)
    common_covid_env = shock_environment(post_calib.mu, post_calib.kappa, spec;
                                         b_R = benefit_pre(spec),
                                         health_R = post_calib.health_N,
                                         health_N = post_calib.health_N)
    differential_covid_env =
        shock_environment(post_calib.mu, post_calib.kappa, spec;
                          b_R = benefit_pre(spec),
                          health_R = post_calib.health_R,
                          health_N = post_calib.health_N)
    full_env = post_calib.env

    scenario_rows = [
        scenario_means("benefit_only", pre_calib, benefit_only_env, sep, spec),
        scenario_means("common_covid_only", pre_calib, common_covid_env, sep, spec),
        scenario_means("differential_covid_only", pre_calib,
                       differential_covid_env, sep, spec),
        scenario_means("full", pre_calib, full_env, sep, spec),
    ]
    scenarios = DataFrame(scenario_rows)

    function row_for(outcome::String, data_pre_R, data_pre_N,
                     model_pre_R, model_pre_N, target_post_R,
                     target_post_N, col_R::Symbol, col_N::Symbol)
        rows = NamedTuple[]
        data_did = did_pp(data_pre_R, data_pre_N, target_post_R, target_post_N)
        for r in eachrow(scenarios)
            model_did = did_pp(model_pre_R, model_pre_N, r[col_R], r[col_N])
            push!(rows, (
                outcome = outcome,
                scenario = r.scenario,
                data_did_pp = data_did,
                model_did_pp = model_did,
                gap_pp = model_did - data_did,
            ))
        end
        return rows
    end

    decomp_rows = vcat(
        row_for("job_finding", targets.f_R_pre, targets.f_N_pre,
                pre_calib.find_R, pre_calib.find_N,
                targets.f_R_post, targets.f_N_post, :find_R, :find_N),
        row_for("separation", targets.sep_R_pre, targets.sep_N_pre,
                pre_calib.sep_R, pre_calib.sep_N,
                targets.sep_R_post, targets.sep_N_post, :sep_R, :sep_N),
    )

    return DataFrame(decomp_rows), scenarios
end

function stationary_outcome(env::Environment, sep::SeparationParams,
                            spec::ModelSpec)
    values = stationary_values(env, sep, spec)
    out = bellman_update(env, values, sep, spec)
    return (
        find_R = out.find_R,
        find_N = out.find_N,
        sep_R = out.sep_R,
        sep_N = out.sep_N,
        search_R = out.search_R,
        search_N = out.search_N,
        accept_R = out.accept_R,
        accept_N = out.accept_N,
    )
end

function replacement_rate_validation(pre_calib, post_calib,
                                     sep::SeparationParams,
                                     spec::ModelSpec)
    # Replace these assumed rates with empirical group means once available.
    # The data DiDs are Table 7's replacement-rate-group coefficients.
    groups = [
        (group = "Low RR", pre_rr = 0.14, post_rr = 0.28,
         data_did_pp = -2.37),
        (group = "Mid RR", pre_rr = 0.28, post_rr = 0.56,
         data_did_pp = -4.25),
        (group = "High RR", pre_rr = 0.45, post_rr = 0.90,
         data_did_pp = -6.04),
    ]

    rows = NamedTuple[]
    for g in groups
        pre_env = shock_environment(pre_calib.mu, pre_calib.kappa, spec;
                                    b_R = g.pre_rr * spec.wage,
                                    health_R = 0.0,
                                    health_N = 0.0)
        full_env = shock_environment(post_calib.mu, post_calib.kappa, spec;
                                     b_R = g.post_rr * spec.wage,
                                     health_R = post_calib.health_R,
                                     health_N = post_calib.health_N)
        benefit_only_env =
            shock_environment(pre_calib.mu, pre_calib.kappa, spec;
                              b_R = g.post_rr * spec.wage,
                              health_R = 0.0,
                              health_N = 0.0)

        pre = stationary_outcome(pre_env, sep, spec)
        full = shock_means(simulate_path(pre_env, full_env, sep, spec))
        benefit_only =
            shock_means(simulate_path(pre_env, benefit_only_env, sep, spec))

        full_did_pp = did_pp(pre.find_R, pre.find_N,
                             full.find_R, full.find_N)
        benefit_only_did_pp =
            did_pp(pre.find_R, pre.find_N,
                   benefit_only.find_R, benefit_only.find_N)

        push!(rows, (
            group = g.group,
            assumed_pre_replacement_rate = g.pre_rr,
            assumed_post_replacement_rate = g.post_rr,
            assumed_replacement_rate_change = g.post_rr - g.pre_rr,
            data_did_pp = g.data_did_pp,
            model_full_did_pp = full_did_pp,
            model_benefit_only_did_pp = benefit_only_did_pp,
            full_gap_pp = full_did_pp - g.data_did_pp,
            benefit_only_gap_pp = benefit_only_did_pp - g.data_did_pp,
            pre_find_N = pre.find_N,
            pre_find_R = pre.find_R,
            post_find_N = full.find_N,
            post_find_R = full.find_R,
            pre_gap_pp = 100.0 * (pre.find_R - pre.find_N),
            post_gap_pp = 100.0 * (full.find_R - full.find_N),
        ))
    end

    return DataFrame(rows)
end

function build_summary(pre_calib, post_calib,
                       sep::SeparationParams,
                       spec::ModelSpec, targets::Targets)
    data_pre_gap = 100.0 * (targets.f_R_pre - targets.f_N_pre)
    model_pre_gap = 100.0 * (pre_calib.find_R - pre_calib.find_N)
    data_post_gap = 100.0 * (targets.f_R_post - targets.f_N_post)
    model_post_gap = 100.0 * (post_calib.find_R - post_calib.find_N)

    return DataFrame([
        (moment = "f_N_pre", data = targets.f_N_pre,
         model = pre_calib.find_N, gap = pre_calib.find_N - targets.f_N_pre),
        (moment = "f_R_pre", data = targets.f_R_pre,
         model = pre_calib.find_R, gap = pre_calib.find_R - targets.f_R_pre),
        (moment = "f_N_post", data = targets.f_N_post,
         model = post_calib.find_N, gap = post_calib.find_N - targets.f_N_post),
        (moment = "f_R_post", data = targets.f_R_post,
         model = post_calib.find_R, gap = post_calib.find_R - targets.f_R_post),
        (moment = "sep_N_pre", data = targets.sep_N_pre,
         model = pre_calib.sep_N, gap = pre_calib.sep_N - targets.sep_N_pre),
        (moment = "sep_R_pre", data = targets.sep_R_pre,
         model = pre_calib.sep_R, gap = pre_calib.sep_R - targets.sep_R_pre),
        (moment = "sep_N_post", data = targets.sep_N_post,
         model = post_calib.sep_N, gap = post_calib.sep_N - targets.sep_N_post),
        (moment = "sep_R_post", data = targets.sep_R_post,
         model = post_calib.sep_R, gap = post_calib.sep_R - targets.sep_R_post),
        (moment = "jfr_pre_gap_pp", data = data_pre_gap,
         model = model_pre_gap, gap = model_pre_gap - data_pre_gap),
        (moment = "jfr_post_gap_pp", data = data_post_gap,
         model = model_post_gap, gap = model_post_gap - data_post_gap),
        (moment = "jfr_did_pp", data = data_post_gap - data_pre_gap,
         model = model_post_gap - model_pre_gap,
         gap = (model_post_gap - model_pre_gap) -
               (data_post_gap - data_pre_gap)),
        (moment = "mu_pre", data = NaN, model = pre_calib.mu, gap = NaN),
        (moment = "kappa_pre", data = NaN, model = pre_calib.kappa, gap = NaN),
        (moment = "mu_post", data = NaN, model = post_calib.mu, gap = NaN),
        (moment = "kappa_post", data = NaN, model = post_calib.kappa, gap = NaN),
        (moment = "health_N", data = NaN, model = post_calib.health_N, gap = NaN),
        (moment = "health_R", data = NaN, model = post_calib.health_R, gap = NaN),
        (moment = "differential_health_pct_wage", data = NaN,
         model = 100.0 * (post_calib.health_R - post_calib.health_N) / spec.wage,
         gap = NaN),
        (moment = "search_curvature", data = NaN,
         model = spec.search_curvature, gap = NaN),
        (moment = "sep_health_sensitivity", data = NaN,
         model = spec.sep_health_sensitivity, gap = NaN),
        (moment = "sep_base", data = NaN, model = sep.base, gap = NaN),
        (moment = "sep_amp", data = NaN, model = sep.amp, gap = NaN),
    ])
end

function spec_with_upper_params(base::ModelSpec, eta::Float64,
                                sep_health_sensitivity::Float64)
    return ModelSpec(
        beta = base.beta,
        firm_delta = base.firm_delta,
        match_elasticity = base.match_elasticity,
        vacancy_cost = base.vacancy_cost,
        wage = base.wage,
        y = base.y,
        base_replacement_rate = base.base_replacement_rate,
        benefit_multiplier = base.benefit_multiplier,
        leisure_value = base.leisure_value,
        offer_threshold = base.offer_threshold,
        offer_dispersion = base.offer_dispersion,
        separation_threshold = base.separation_threshold,
        separation_dispersion = base.separation_dispersion,
        sep_health_sensitivity = sep_health_sensitivity,
        search_curvature = eta,
        max_search = base.max_search,
        probability_cap = base.probability_cap,
        flow_floor = base.flow_floor,
        T = base.T,
        shock_start = base.shock_start,
        shock_end = base.shock_end,
    )
end

function calibration_for_upper_params(eta::Float64,
                                      sep_health_sensitivity::Float64,
                                      base_spec::ModelSpec,
                                      targets::Targets)
    spec = spec_with_upper_params(base_spec, eta,
                                  sep_health_sensitivity)
    sep = fit_pre_separation(spec, targets)
    pre_calib = calibrate_pre(sep, spec, targets)
    post_calib = calibrate_post(pre_calib, sep, spec, targets)
    return spec, sep, pre_calib, post_calib
end

function upper_fit_row(eta::Float64,
                       sep_health_sensitivity::Float64,
                       base_spec::ModelSpec,
                       targets::Targets)
    spec, sep, pre_calib, post_calib =
        calibration_for_upper_params(eta, sep_health_sensitivity,
                                     base_spec, targets)

    data_pre_gap = 100.0 * (targets.f_R_pre - targets.f_N_pre)
    model_pre_gap = 100.0 * (pre_calib.find_R - pre_calib.find_N)
    data_post_gap = 100.0 * (targets.f_R_post - targets.f_N_post)
    model_post_gap = 100.0 * (post_calib.find_R - post_calib.find_N)

    # Levels keep the model anchored; gaps make the baseline benefit and
    # supplement/COVID contrasts carry the calibration, which is the object here.
    level_loss =
        100.0 * ((pre_calib.find_N - targets.f_N_pre)^2 +
                 (pre_calib.find_R - targets.f_R_pre)^2 +
                 (post_calib.find_N - targets.f_N_post)^2 +
                 (post_calib.find_R - targets.f_R_post)^2)
    gap_loss = (model_pre_gap - data_pre_gap)^2 +
               (model_post_gap - data_post_gap)^2
    loss = gap_loss + level_loss

    return (
        search_curvature = eta,
        sep_health_sensitivity = sep_health_sensitivity,
        loss = loss,
        gap_loss = gap_loss,
        level_loss = level_loss,
        jfr_pre_gap_data_pp = data_pre_gap,
        jfr_pre_gap_model_pp = model_pre_gap,
        jfr_post_gap_data_pp = data_post_gap,
        jfr_post_gap_model_pp = model_post_gap,
        jfr_did_data_pp = data_post_gap - data_pre_gap,
        jfr_did_model_pp = model_post_gap - model_pre_gap,
        f_N_pre_model = pre_calib.find_N,
        f_R_pre_model = pre_calib.find_R,
        f_N_post_model = post_calib.find_N,
        f_R_post_model = post_calib.find_R,
        sep_N_pre_model = pre_calib.sep_N,
        sep_R_pre_model = pre_calib.sep_R,
        sep_N_post_model = post_calib.sep_N,
        sep_R_post_model = post_calib.sep_R,
        health_N = post_calib.health_N,
        health_R = post_calib.health_R,
        differential_health_pct_wage =
            100.0 * (post_calib.health_R - post_calib.health_N) / spec.wage,
        mu_pre = pre_calib.mu,
        kappa_pre = pre_calib.kappa,
        mu_post = post_calib.mu,
        kappa_post = post_calib.kappa,
    ), spec, sep, pre_calib, post_calib
end

function calibrate_upper_parameters(base_spec::ModelSpec,
                                    targets::Targets)
    curvature_grid = [20.0, 30.0, 40.0, 50.0, 60.0, 80.0, 100.0, 150.0]
    sep_sensitivity_grid = [0.35, 0.45, 0.50, 0.60, 0.75]

    rows = NamedTuple[]
    best_row = nothing
    best_spec = nothing
    best_sep = nothing
    best_pre = nothing
    best_post = nothing

    for eta in curvature_grid, sep_sens in sep_sensitivity_grid
        row, spec, sep, pre_calib, post_calib =
            upper_fit_row(eta, sep_sens, base_spec, targets)
        push!(rows, row)

        if best_row === nothing || row.loss < best_row.loss
            best_row = row
            best_spec = spec
            best_sep = sep
            best_pre = pre_calib
            best_post = post_calib
        end
    end

    return best_spec, best_sep, best_pre, best_post,
           best_row.loss, DataFrame(rows)
end

function calibrate_post_from_N(pre_calib, health_R::Float64,
                               health_N::Float64,
                               sep::SeparationParams,
                               spec::ModelSpec, targets::Targets)
    # Restricted post calibration: the common COVID market shock is disciplined
    # by NZ job finding only. Search costs are held fixed at the pre value.
    objective(log_mu) = begin
        mu = exp(log_mu)
        if !(0.001 <= mu <= 3.0)
            return 1e4
        end

        shock_env = shock_environment(mu, pre_calib.kappa, spec;
                                      b_R = benefit_post(spec),
                                      health_R = health_R,
                                      health_N = health_N)
        means = shock_means(simulate_path(pre_calib.env, shock_env,
                                          sep, spec))
        return (means.find_N - targets.f_N_post)^2
    end

    opt = optimize(objective, log(0.001), log(3.0))
    mu = exp(Optim.minimizer(opt))
    env = shock_environment(mu, pre_calib.kappa, spec;
                            b_R = benefit_post(spec),
                            health_R = health_R,
                            health_N = health_N)
    means = shock_means(simulate_path(pre_calib.env, env, sep, spec))

    return (
        env = env,
        mu = mu,
        kappa = pre_calib.kappa,
        health_R = health_R,
        health_N = health_N,
        loss = Optim.minimum(opt),
        find_R = means.find_R,
        find_N = means.find_N,
        sep_R = means.sep_R,
        sep_N = means.sep_N,
        search_R = means.search_R,
        search_N = means.search_N,
        accept_R = means.accept_R,
        accept_N = means.accept_N,
        contact = means.contact,
        theta = means.theta,
    )
end

function restricted_fit_row(eta::Float64,
                            sep_health_sensitivity::Float64,
                            base_spec::ModelSpec,
                            targets::Targets)
    spec = spec_with_upper_params(base_spec, eta,
                                  sep_health_sensitivity)
    sep = fit_pre_separation(spec, targets)
    pre_calib = calibrate_pre(sep, spec, targets)

    health_N = fit_health_for_sep(:N, targets.sep_N_post,
                                  pre_calib.mu, pre_calib.kappa,
                                  benefit_post(spec), 0.0,
                                  sep, spec)
    health_R = fit_health_for_sep(:R, targets.sep_R_post,
                                  pre_calib.mu, pre_calib.kappa,
                                  benefit_post(spec), health_N,
                                  sep, spec)
    post_calib = calibrate_post_from_N(pre_calib, health_R, health_N,
                                       sep, spec, targets)

    data_pre_gap = 100.0 * (targets.f_R_pre - targets.f_N_pre)
    model_pre_gap = 100.0 * (pre_calib.find_R - pre_calib.find_N)
    data_post_gap = 100.0 * (targets.f_R_post - targets.f_N_post)
    model_post_gap = 100.0 * (post_calib.find_R - post_calib.find_N)

    # Excludes AUS post JFR from the fitted loss. It is the held-out prediction.
    fitted_loss =
        100.0 * ((pre_calib.find_N - targets.f_N_pre)^2 +
                 (pre_calib.find_R - targets.f_R_pre)^2 +
                 (post_calib.find_N - targets.f_N_post)^2) +
        (model_pre_gap - data_pre_gap)^2

    aus_post_prediction_gap_pp =
        100.0 * (post_calib.find_R - targets.f_R_post)

    return (
        search_curvature = eta,
        sep_health_sensitivity = sep_health_sensitivity,
        fitted_loss = fitted_loss,
        heldout_AUS_post_jfr_gap_pp = aus_post_prediction_gap_pp,
        jfr_pre_gap_data_pp = data_pre_gap,
        jfr_pre_gap_model_pp = model_pre_gap,
        jfr_post_gap_data_pp = data_post_gap,
        jfr_post_gap_model_pp = model_post_gap,
        jfr_did_data_pp = data_post_gap - data_pre_gap,
        jfr_did_model_pp = model_post_gap - model_pre_gap,
        f_N_pre_model = pre_calib.find_N,
        f_R_pre_model = pre_calib.find_R,
        f_N_post_model = post_calib.find_N,
        f_R_post_predicted = post_calib.find_R,
        sep_N_pre_model = pre_calib.sep_N,
        sep_R_pre_model = pre_calib.sep_R,
        sep_N_post_model = post_calib.sep_N,
        sep_R_post_model = post_calib.sep_R,
        health_N = post_calib.health_N,
        health_R = post_calib.health_R,
        differential_health_pct_wage =
            100.0 * (post_calib.health_R - post_calib.health_N) / spec.wage,
        mu_pre = pre_calib.mu,
        kappa_common = pre_calib.kappa,
        mu_post = post_calib.mu,
    ), spec, sep, pre_calib, post_calib
end

function calibrate_restricted_model(base_spec::ModelSpec,
                                    targets::Targets)
    curvature_grid = [5.0, 7.0, 10.0, 15.0, 25.0, 40.0, 60.0,
                      80.0, 100.0, 150.0]
    sep_sensitivity_grid = [0.35, 0.45, 0.50, 0.60, 0.75]

    rows = NamedTuple[]
    best_row = nothing
    best_spec = nothing
    best_sep = nothing
    best_pre = nothing
    best_post = nothing

    for eta in curvature_grid, sep_sens in sep_sensitivity_grid
        row, spec, sep, pre_calib, post_calib =
            restricted_fit_row(eta, sep_sens, base_spec, targets)
        push!(rows, row)

        candidate_score =
            row.fitted_loss +
            1e-8 * abs(sep_sens - base_spec.sep_health_sensitivity)
        best_score = best_row === nothing ? Inf :
            best_row.fitted_loss +
            1e-8 * abs(best_row.sep_health_sensitivity -
                       base_spec.sep_health_sensitivity)

        if candidate_score < best_score
            best_row = row
            best_spec = spec
            best_sep = sep
            best_pre = pre_calib
            best_post = post_calib
        end
    end

    return best_spec, best_sep, best_pre, best_post,
           best_row.fitted_loss, DataFrame(rows)
end

function print_results(summary::DataFrame, decomp::DataFrame)
    println("\n14_ joint-margin calibration")
    println("----------------------------")
    for r in eachrow(summary)
        if isfinite(r.data)
            @printf("%-16s data=%8.4f  model=%8.4f  gap=% .4e\n",
                    r.moment, r.data, r.model, r.gap)
        else
            @printf("%-16s model=%8.4f\n", r.moment, r.model)
        end
    end

    println("\nDiD decomposition")
    println("-----------------")
    for r in eachrow(decomp)
        @printf("%-12s %-24s data=%7.3f pp  model=%7.3f pp  gap=% .3f pp\n",
                r.outcome, r.scenario, r.data_did_pp,
                r.model_did_pp, r.gap_pp)
    end
end

function run_model()
    base_spec = ModelSpec()
    targets = Targets()

    spec, sep, pre_calib, post_calib, restricted_loss, restricted_grid =
        calibrate_restricted_model(base_spec, targets)

    summary = build_summary(pre_calib, post_calib, sep, spec, targets)
    decomp, scenarios = build_decomposition(pre_calib, post_calib,
                                            sep, spec, targets)
    rr_validation = replacement_rate_validation(pre_calib, post_calib,
                                                sep, spec)

    CSV.write("14_joint_margin_summary.csv", summary)
    CSV.write("14_joint_margin_decomposition.csv", decomp)
    CSV.write("14_joint_margin_scenarios.csv", scenarios)
    CSV.write("14_joint_margin_restricted_grid.csv", restricted_grid)
    CSV.write("14_replacement_rate_gradient_validation.csv",
              rr_validation)

    print_results(summary, decomp)
    @printf("\nRestricted fitted loss: %.6f\n", restricted_loss)
    @printf("Held-out AUS post JFR prediction gap: %.3f p.p.\n",
            100.0 * (post_calib.find_R - targets.f_R_post))

    println("\nReplacement-rate gradient validation")
    println("------------------------------------")
    for r in eachrow(rr_validation)
        @printf("%-8s pre RR=%.2f post RR=%.2f  data=%7.3f pp  model=%7.3f pp  gap=% .3f pp\n",
                r.group, r.assumed_pre_replacement_rate,
                r.assumed_post_replacement_rate, r.data_did_pp,
                r.model_full_did_pp, r.full_gap_pp)
    end

    println("\nSaved:")
    println("  14_joint_margin_summary.csv")
    println("  14_joint_margin_decomposition.csv")
    println("  14_joint_margin_scenarios.csv")
    println("  14_joint_margin_restricted_grid.csv")
    println("  14_replacement_rate_gradient_validation.csv")

    return summary, decomp, scenarios, restricted_grid, rr_validation
end

if abspath(PROGRAM_FILE) == @__FILE__
    run_model()
end
