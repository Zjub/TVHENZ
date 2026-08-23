# 16_three_agent_toy_model.jl
#
# Three-agent toy search-and-matching model for the COVID Supplement paper.
#
# Structural agents:
#   AB  Australian, receives the base JobSeeker payment and Supplement
#   A0  Australian, does not receive JobSeeker/Supplement
#   NZ  New Zealander, does not receive JobSeeker/Supplement
#
# The empirical work contains two separately matched NZ panels.  They are
# treated as two measurements of the same structural NZ agent, not as two
# economic types.  Panel-specific, time-invariant measurement offsets preserve
# the matched pre-period comparisons.  Both panels must share the same model NZ
# post response; their different estimated Post coefficients are therefore an
# overidentifying residual.
#
# Public whole-population vacancy, employer and JobKeeper evidence is not used
# as a level target.  It is external validation for the common shocks estimated
# from the selected empirical groups.

using CSV
using DataFrames
using Optim
using Printf
using Statistics

const GROUPS_16 = (:AB, :A0, :NZ)
const AB_16 = 1
const A0_16 = 2
const NZ_16 = 3

Base.@kwdef struct ModelSpec16
    # A 0.96 annual factor converted to weekly frequency.
    beta::Float64 = 0.96^(1 / 52)
    firm_delta::Float64 = 0.01
    match_elasticity::Float64 = 0.42
    vacancy_cost::Float64 = 1.0
    wage::Float64 = 0.55
    productivity::Float64 = 1.0
    base_replacement_rate::Float64 = 0.28
    benefit_multiplier::Float64 = 2.0
    leisure_value::Float64 = 0.10 * 0.55
    offer_threshold::Float64 = 0.15
    offer_dispersion::Float64 = 0.35
    separation_threshold::Float64 = 0.10
    separation_dispersion::Float64 = 0.04
    search_curvature::Float64 = 7.0
    max_search::Float64 = 50.0
    probability_cap::Float64 = 0.999
    flow_floor::Float64 = 1e-8
    T::Int = 60
    shock_start::Int = 12
    shock_end::Int = 36
    # :combined, :search_only, or :acceptance_only
    channel::Symbol = :combined
end

Base.@kwdef struct SeparationParams16
    base::Float64
    amp::Float64
end

Base.@kwdef struct CalibrationParams16
    mu_pre::Float64
    kappa::Float64
    mu_post::Float64
    common_sep_shock::Float64
    nz_work_cost_diff::Float64
    sep::SeparationParams16
end

Base.@kwdef struct Environment16
    mu::Float64
    kappa::Float64
    benefits::NTuple{3,Float64}
    work_costs::NTuple{3,Float64}
    common_sep_shock::Float64 = 0.0
end

struct ValueState16
    W::NTuple{3,Float64}
    U::NTuple{3,Float64}
end

Base.@kwdef struct PeriodOutcome16
    value::ValueState16
    search::NTuple{3,Float64}
    accept::NTuple{3,Float64}
    separation::NTuple{3,Float64}
    finding::NTuple{3,Float64}
    flow_surplus::NTuple{3,Float64}
    value_surplus::NTuple{3,Float64}
    contact::Float64
    theta::Float64
end

logistic16(x) = 1.0 / (1.0 + exp(-x))
benefit_pre16(spec::ModelSpec16) = spec.base_replacement_rate * spec.wage
benefit_post16(spec::ModelSpec16) =
    spec.benefit_multiplier * benefit_pre16(spec)

function regression_targets16()
    # Main estimates are Tables 3 and 4.  Placebo estimates are Table 9,
    # Panel B (Australians not receiving JobSeeker and their matched NZ panel).
    return DataFrame([
        (outcome=:finding, panel=:main, term=:intercept,
         estimate=0.1002, se=0.0044),
        (outcome=:finding, panel=:main, term=:australian,
         estimate=-0.0134, se=0.0063),
        (outcome=:finding, panel=:main, term=:post,
         estimate=-0.0179, se=0.0058),
        (outcome=:finding, panel=:main, term=:interaction,
         estimate=-0.0171, se=0.0082),
        (outcome=:finding, panel=:placebo, term=:intercept,
         estimate=0.0948, se=0.0071),
        (outcome=:finding, panel=:placebo, term=:australian,
         estimate=0.0033, se=0.0101),
        (outcome=:finding, panel=:placebo, term=:post,
         estimate=-0.0138, se=0.0095),
        (outcome=:finding, panel=:placebo, term=:interaction,
         estimate=-0.0011, se=0.0132),
        (outcome=:separation, panel=:main, term=:intercept,
         estimate=0.0480, se=0.0076),
        (outcome=:separation, panel=:main, term=:australian,
         estimate=0.0004, se=0.0107),
        (outcome=:separation, panel=:main, term=:post,
         estimate=0.0070, se=0.0099),
        (outcome=:separation, panel=:main, term=:interaction,
         estimate=0.0372, se=0.0140),
        (outcome=:separation, panel=:placebo, term=:intercept,
         estimate=0.0480, se=0.0054),
        (outcome=:separation, panel=:placebo, term=:australian,
         estimate=0.0025, se=0.0077),
        (outcome=:separation, panel=:placebo, term=:post,
         estimate=0.0060, se=0.0071),
        (outcome=:separation, panel=:placebo, term=:interaction,
         estimate=-0.0036, se=0.0100),
    ])
end

function target_lookup16(targets::DataFrame, outcome::Symbol,
                         panel::Symbol, term::Symbol)
    rows = targets[(targets.outcome .== outcome) .&
                   (targets.panel .== panel) .&
                   (targets.term .== term), :]
    nrow(rows) == 1 || error("Expected one empirical target")
    return rows.estimate[1], rows.se[1]
end

function weighted_nz_intercept16(targets::DataFrame, outcome::Symbol)
    vals = Float64[]
    weights = Float64[]
    for panel in (:main, :placebo)
        est, se = target_lookup16(targets, outcome, panel, :intercept)
        push!(vals, est)
        push!(weights, 1 / se^2)
    end
    return sum(vals .* weights) / sum(weights)
end

function utility16(flow::Float64, spec::ModelSpec16)
    return max(flow, spec.flow_floor)
end

outside16(env::Environment16, i::Int, spec::ModelSpec16) =
    env.benefits[i] + spec.leisure_value
work_flow16(env::Environment16, i::Int, spec::ModelSpec16) =
    spec.wage - env.work_costs[i]
flow_surplus16(env::Environment16, i::Int, spec::ModelSpec16) =
    work_flow16(env, i, spec) - outside16(env, i, spec)

function accept_prob16(flow_surplus::Float64, spec::ModelSpec16)
    spec.channel == :search_only && return 1.0
    return clamp(logistic16((flow_surplus - spec.offer_threshold) /
                            spec.offer_dispersion), 0.0, 1.0)
end

function sep_shape16(flow_surplus::Float64, spec::ModelSpec16)
    return logistic16((spec.separation_threshold - flow_surplus) /
                      spec.separation_dispersion)
end

function separation_rate16(flow_surplus::Float64,
                           common_sep_shock::Float64,
                           sep::SeparationParams16,
                           spec::ModelSpec16)
    endogenous = clamp(sep.base + sep.amp * sep_shape16(flow_surplus, spec),
                       0.0, spec.probability_cap)
    common = clamp(common_sep_shock, 0.0, spec.probability_cap)
    # Competing risks: a common employer/background event and a
    # surplus-sensitive match-continuation event.
    return clamp(1.0 - (1.0 - common) * (1.0 - endogenous),
                 0.0, spec.probability_cap)
end

function labor_market16(env::Environment16, spec::ModelSpec16)
    firm_surplus = spec.productivity - spec.wage
    firm_surplus > 0.0 || error("Firm surplus is non-positive")
    firm_value = firm_surplus /
                 (1.0 - spec.beta * (1.0 - spec.firm_delta))
    theta = ((spec.beta * env.mu * firm_value) /
             spec.vacancy_cost)^(1.0 / spec.match_elasticity)
    contact = min(env.mu * theta^(1.0 - spec.match_elasticity),
                  spec.probability_cap)
    return theta, contact
end

function offer_probability16(search::Float64, contact::Float64,
                             spec::ModelSpec16)
    return clamp(1.0 - exp(-contact * max(search, 0.0)),
                 0.0, spec.probability_cap)
end

function job_finding16(search::Float64, contact::Float64,
                       accept::Float64, spec::ModelSpec16)
    return clamp(accept * offer_probability16(search, contact, spec),
                 0.0, spec.probability_cap)
end

function search_cost16(search::Float64, kappa::Float64,
                       spec::ModelSpec16)
    spec.channel == :acceptance_only && return 0.0
    return kappa * max(search, 0.0)^(1.0 + spec.search_curvature) /
           (1.0 + spec.search_curvature)
end

function choose_search16(next_surplus::Float64, contact::Float64,
                         accept::Float64, kappa::Float64,
                         spec::ModelSpec16)
    spec.channel == :acceptance_only && return 1.0
    if next_surplus <= 0.0 || contact <= 0.0 || accept <= 0.0
        return 0.0
    end

    marginal_gain(s) = spec.beta * accept * contact *
                       exp(-contact * s) * next_surplus
    marginal_cost(s) = kappa * s^spec.search_curvature
    foc(s) = marginal_cost(s) - marginal_gain(s)
    foc(spec.max_search) <= 0.0 && return spec.max_search

    lo, hi = 0.0, spec.max_search
    for _ in 1:70
        mid = 0.5 * (lo + hi)
        if foc(mid) <= 0.0
            lo = mid
        else
            hi = mid
        end
    end
    return 0.5 * (lo + hi)
end

function stationary_group16(env::Environment16, i::Int,
                            sep::SeparationParams16,
                            spec::ModelSpec16, contact::Float64)
    fs = flow_surplus16(env, i, spec)
    accept = accept_prob16(fs, spec)
    separation = separation_rate16(fs, env.common_sep_shock, sep, spec)

    function residual(D)
        search = choose_search16(D, contact, accept, env.kappa, spec)
        finding = job_finding16(search, contact, accept, spec)
        cost = search_cost16(search, env.kappa, spec)
        denom = 1.0 - spec.beta * (1.0 - separation - finding)
        return D - (fs + cost) / denom
    end

    lo, hi = -10.0, 10.0
    while residual(lo) > 0.0
        lo *= 2.0
        abs(lo) > 1e6 && error("Could not bracket stationary surplus")
    end
    while residual(hi) < 0.0
        hi *= 2.0
        hi > 1e6 && error("Could not bracket stationary surplus")
    end
    for _ in 1:100
        mid = 0.5 * (lo + hi)
        if residual(mid) <= 0.0
            lo = mid
        else
            hi = mid
        end
    end
    D = 0.5 * (lo + hi)
    uw = utility16(work_flow16(env, i, spec), spec)
    W = (uw - spec.beta * separation * D) / (1.0 - spec.beta)
    U = W - D
    return W, U
end

function stationary_values16(env::Environment16,
                             sep::SeparationParams16,
                             spec::ModelSpec16)
    _, contact = labor_market16(env, spec)
    pairs = ntuple(i -> stationary_group16(env, i, sep, spec, contact), 3)
    return ValueState16(ntuple(i -> pairs[i][1], 3),
                        ntuple(i -> pairs[i][2], 3))
end

function bellman_update16(env::Environment16, next::ValueState16,
                          sep::SeparationParams16, spec::ModelSpec16)
    theta, contact = labor_market16(env, spec)
    fs = ntuple(i -> flow_surplus16(env, i, spec), 3)
    accept = ntuple(i -> accept_prob16(fs[i], spec), 3)
    separation = ntuple(i -> separation_rate16(fs[i],
                                                env.common_sep_shock,
                                                sep, spec), 3)
    value_surplus = ntuple(i -> next.W[i] - next.U[i], 3)
    search = ntuple(i -> choose_search16(value_surplus[i], contact,
                                         accept[i], env.kappa, spec), 3)
    finding = ntuple(i -> job_finding16(search[i], contact,
                                        accept[i], spec), 3)
    W = ntuple(i ->
        utility16(work_flow16(env, i, spec), spec) +
        spec.beta * ((1.0 - separation[i]) * next.W[i] +
                     separation[i] * next.U[i]), 3)
    U = ntuple(i ->
        utility16(outside16(env, i, spec), spec) -
        search_cost16(search[i], env.kappa, spec) +
        spec.beta * (finding[i] * next.W[i] +
                     (1.0 - finding[i]) * next.U[i]), 3)

    return PeriodOutcome16(
        value=ValueState16(W, U), search=search, accept=accept,
        separation=separation, finding=finding, flow_surplus=fs,
        value_surplus=value_surplus, contact=contact, theta=theta)
end

function compute_value_path16(pre_env::Environment16,
                              shock_env::Environment16,
                              sep::SeparationParams16,
                              spec::ModelSpec16)
    values = Vector{ValueState16}(undef, spec.T + 1)
    pre_ss = stationary_values16(pre_env, sep, spec)
    values[spec.T + 1] = pre_ss
    for t in spec.T:-1:1
        env = spec.shock_start <= t <= spec.shock_end ? shock_env : pre_env
        next = t < spec.shock_start ? pre_ss : values[t + 1]
        values[t] = bellman_update16(env, next, sep, spec).value
    end
    return values, pre_ss
end

function simulate_path16(pre_env::Environment16,
                         shock_env::Environment16,
                         sep::SeparationParams16,
                         spec::ModelSpec16)
    values, pre_ss = compute_value_path16(pre_env, shock_env, sep, spec)
    rows = NamedTuple[]
    for t in 1:spec.T
        in_shock = spec.shock_start <= t <= spec.shock_end
        env = in_shock ? shock_env : pre_env
        next = t < spec.shock_start ? pre_ss : values[t + 1]
        out = bellman_update16(env, next, sep, spec)
        push!(rows, (
            week=t, in_shock=in_shock,
            find_AB=out.finding[AB_16], find_A0=out.finding[A0_16],
            find_NZ=out.finding[NZ_16],
            sep_AB=out.separation[AB_16], sep_A0=out.separation[A0_16],
            sep_NZ=out.separation[NZ_16],
            search_AB=out.search[AB_16], search_A0=out.search[A0_16],
            search_NZ=out.search[NZ_16],
            accept_AB=out.accept[AB_16], accept_A0=out.accept[A0_16],
            accept_NZ=out.accept[NZ_16],
            flow_surplus_AB=out.flow_surplus[AB_16],
            flow_surplus_A0=out.flow_surplus[A0_16],
            flow_surplus_NZ=out.flow_surplus[NZ_16],
            value_surplus_AB=out.value_surplus[AB_16],
            value_surplus_A0=out.value_surplus[A0_16],
            value_surplus_NZ=out.value_surplus[NZ_16],
            contact=out.contact, theta=out.theta,
        ))
    end
    return DataFrame(rows)
end

function shock_means16(df::DataFrame)
    s = df[df.in_shock .== true, :]
    return (
        finding=(mean(s.find_AB), mean(s.find_A0), mean(s.find_NZ)),
        separation=(mean(s.sep_AB), mean(s.sep_A0), mean(s.sep_NZ)),
        search=(mean(s.search_AB), mean(s.search_A0), mean(s.search_NZ)),
        accept=(mean(s.accept_AB), mean(s.accept_A0), mean(s.accept_NZ)),
        contact=mean(s.contact), theta=mean(s.theta),
    )
end

function stationary_outcome16(env::Environment16,
                              sep::SeparationParams16,
                              spec::ModelSpec16)
    values = stationary_values16(env, sep, spec)
    out = bellman_update16(env, values, sep, spec)
    return (finding=out.finding, separation=out.separation,
            search=out.search, accept=out.accept,
            contact=out.contact, theta=out.theta)
end

function pre_environment16(p::CalibrationParams16, spec::ModelSpec16)
    return Environment16(
        mu=p.mu_pre, kappa=p.kappa,
        benefits=(benefit_pre16(spec), 0.0, 0.0),
        work_costs=(0.0, 0.0, 0.0), common_sep_shock=0.0)
end

function post_environment16(p::CalibrationParams16, spec::ModelSpec16;
                            benefit_on::Bool=true,
                            common_on::Bool=true,
                            nz_diff_on::Bool=true)
    return Environment16(
        mu=common_on ? p.mu_post : p.mu_pre,
        kappa=p.kappa,
        benefits=(benefit_on ? benefit_post16(spec) : benefit_pre16(spec),
                  0.0, 0.0),
        work_costs=(0.0, 0.0,
                    nz_diff_on ? p.nz_work_cost_diff : 0.0),
        common_sep_shock=common_on ? p.common_sep_shock : 0.0)
end

function predict_regressions16(pre, post, targets::DataFrame)
    predictions = Dict{Tuple{Symbol,Symbol,Symbol},Float64}()
    for outcome in (:finding, :separation)
        pre_y = getproperty(pre, outcome)
        post_y = getproperty(post, outcome)
        delta = ntuple(i -> post_y[i] - pre_y[i], 3)
        pool = weighted_nz_intercept16(targets, outcome)

        for panel in (:main, :placebo)
            empirical_intercept, _ =
                target_lookup16(targets, outcome, panel, :intercept)
            offset = empirical_intercept - pool
            predictions[(outcome, panel, :intercept)] =
                pre_y[NZ_16] + offset
            predictions[(outcome, panel, :post)] = delta[NZ_16]
        end

        main_nz_intercept = predictions[(outcome, :main, :intercept)]
        placebo_nz_intercept =
            predictions[(outcome, :placebo, :intercept)]
        predictions[(outcome, :main, :australian)] =
            pre_y[AB_16] - main_nz_intercept
        predictions[(outcome, :placebo, :australian)] =
            pre_y[A0_16] - placebo_nz_intercept
        predictions[(outcome, :main, :interaction)] =
            delta[AB_16] - delta[NZ_16]
        predictions[(outcome, :placebo, :interaction)] =
            delta[A0_16] - delta[NZ_16]
    end
    return predictions
end

bounded16(z, lo, hi) = lo + (hi - lo) * logistic16(z)

function invbounded16(v, lo, hi)
    q = clamp((v - lo) / (hi - lo), 1e-8, 1.0 - 1e-8)
    return log(q / (1.0 - q))
end

function decode_params16(x, spec::ModelSpec16)
    if spec.channel == :acceptance_only
        length(x) == 6 || error("Acceptance-only calibration needs 6 parameters")
        return CalibrationParams16(
            mu_pre=bounded16(x[1], 0.005, 0.50),
            kappa=0.35,
            mu_post=bounded16(x[2], 0.005, 0.50),
            common_sep_shock=bounded16(x[3], 0.0, 0.15),
            nz_work_cost_diff=bounded16(x[4], -0.15, 0.15),
            sep=SeparationParams16(
                base=bounded16(x[5], 0.001, 0.12),
                amp=bounded16(x[6], 0.0, 0.80)))
    end
    length(x) == 7 || error("Search/combined calibration needs 7 parameters")
    return CalibrationParams16(
        mu_pre=bounded16(x[1], 0.005, 0.50),
        kappa=bounded16(x[2], 0.005, 200.0),
        mu_post=bounded16(x[3], 0.005, 0.50),
        common_sep_shock=bounded16(x[4], 0.0, 0.15),
        nz_work_cost_diff=bounded16(x[5], -0.15, 0.15),
        sep=SeparationParams16(
            base=bounded16(x[6], 0.001, 0.12),
            amp=bounded16(x[7], 0.0, 0.80)))
end

function starting_point16(spec::ModelSpec16)
    if spec.channel == :acceptance_only
        return [
            invbounded16(0.08, 0.005, 0.50),
            invbounded16(0.065, 0.005, 0.50),
            invbounded16(0.006, 0.0, 0.15),
            invbounded16(0.0, -0.15, 0.15),
            invbounded16(0.048, 0.001, 0.12),
            invbounded16(0.35, 0.0, 0.80),
        ]
    end
    return [
        invbounded16(0.08, 0.005, 0.50),
        invbounded16(20.0, 0.005, 200.0),
        invbounded16(0.065, 0.005, 0.50),
        invbounded16(0.006, 0.0, 0.15),
        invbounded16(0.0, -0.15, 0.15),
        invbounded16(0.048, 0.001, 0.12),
        invbounded16(0.35, 0.0, 0.80),
    ]
end

function evaluate_calibration16(p::CalibrationParams16,
                                spec::ModelSpec16,
                                targets::DataFrame)
    pre_env = pre_environment16(p, spec)
    post_env = post_environment16(p, spec)
    pre = stationary_outcome16(pre_env, p.sep, spec)
    path = simulate_path16(pre_env, post_env, p.sep, spec)
    post = shock_means16(path)
    predictions = predict_regressions16(pre, post, targets)
    return pre, post, path, predictions
end

function calibration_loss16(x, spec::ModelSpec16, targets::DataFrame)
    try
        p = decode_params16(x, spec)
        _, _, _, predictions = evaluate_calibration16(p, spec, targets)
        loss = 0.0
        for row in eachrow(targets)
            key = (row.outcome, row.panel, row.term)
            residual = (predictions[key] - row.estimate) / row.se
            loss += residual^2
        end
        isfinite(loss) || return 1e12
        return loss
    catch
        return 1e12
    end
end

function calibrate_model16(spec::ModelSpec16, targets::DataFrame)
    x0 = starting_point16(spec)
    starts = [x0,
              x0 .+ collect(range(-0.25, 0.25, length=length(x0))),
              x0 .+ reverse(collect(range(-0.20, 0.20,
                                           length=length(x0))))]
    best_x = x0
    best_loss = Inf
    for start in starts
        opt = optimize(x -> calibration_loss16(x, spec, targets),
                       start, NelderMead(),
                       Optim.Options(iterations=2_500,
                                     f_tol=1e-10, x_tol=1e-9,
                                     show_trace=false))
        if Optim.minimum(opt) < best_loss
            best_loss = Optim.minimum(opt)
            best_x = Optim.minimizer(opt)
        end
    end
    p = decode_params16(best_x, spec)
    pre, post, path, predictions = evaluate_calibration16(p, spec, targets)
    return p, pre, post, path, predictions, best_loss
end

function regression_output16(targets::DataFrame, predictions)
    rows = NamedTuple[]
    for row in eachrow(targets)
        key = (row.outcome, row.panel, row.term)
        pred = predictions[key]
        push!(rows, (
            outcome=String(row.outcome), panel=String(row.panel),
            term=String(row.term), data=row.estimate, standard_error=row.se,
            model=pred, gap=pred - row.estimate,
            standardized_gap=(pred - row.estimate) / row.se,
            role=(row.panel == :placebo && row.term == :interaction) ?
                 "disciplines NZ differential shock" :
                 (row.term == :post ? "repeated measurement of common NZ response" :
                  "calibration/fit moment"),
        ))
    end
    return DataFrame(rows)
end

function cell_output16(pre, post, targets::DataFrame)
    rows = NamedTuple[]
    mappings = [
        (:finding, :AB, AB_16, :main, :recipient),
        (:finding, :A0, A0_16, :placebo, :nonrecipient),
        (:separation, :AB, AB_16, :main, :recipient),
        (:separation, :A0, A0_16, :placebo, :nonrecipient),
    ]
    for (outcome, group, idx, panel, _) in mappings
        intercept, _ = target_lookup16(targets, outcome, panel, :intercept)
        aus, _ = target_lookup16(targets, outcome, panel, :australian)
        post_coef, _ = target_lookup16(targets, outcome, panel, :post)
        interaction, _ = target_lookup16(targets, outcome, panel, :interaction)
        data_pre = intercept + aus
        data_post = intercept + aus + post_coef + interaction
        model_pre = getproperty(pre, outcome)[idx]
        model_post = getproperty(post, outcome)[idx]
        push!(rows, (outcome=String(outcome), empirical_panel=String(panel),
                     group=String(group), period="pre", data=data_pre,
                     model=model_pre, gap=model_pre-data_pre))
        push!(rows, (outcome=String(outcome), empirical_panel=String(panel),
                     group=String(group), period="post", data=data_post,
                     model=model_post, gap=model_post-data_post))
    end

    for outcome in (:finding, :separation), panel in (:main, :placebo)
        intercept, _ = target_lookup16(targets, outcome, panel, :intercept)
        post_coef, _ = target_lookup16(targets, outcome, panel, :post)
        pool = weighted_nz_intercept16(targets, outcome)
        offset = intercept - pool
        pre_nz = getproperty(pre, outcome)[NZ_16] + offset
        post_nz = getproperty(post, outcome)[NZ_16] + offset
        push!(rows, (outcome=String(outcome), empirical_panel=String(panel),
                     group="NZ", period="pre", data=intercept,
                     model=pre_nz, gap=pre_nz-intercept))
        push!(rows, (outcome=String(outcome), empirical_panel=String(panel),
                     group="NZ", period="post", data=intercept+post_coef,
                     model=post_nz, gap=post_nz-(intercept+post_coef)))
    end
    return DataFrame(rows)
end

did_pp16(pre_g, pre_n, post_g, post_n) =
    100.0 * ((post_g - pre_g) - (post_n - pre_n))

function factorial_scenarios16(p::CalibrationParams16, pre,
                               spec::ModelSpec16)
    pre_env = pre_environment16(p, spec)
    rows = NamedTuple[]
    values = Dict{NTuple{3,Bool},NamedTuple}()
    for benefit_on in (false, true), common_on in (false, true),
        nz_diff_on in (false, true)
        env = post_environment16(p, spec; benefit_on=benefit_on,
                                 common_on=common_on,
                                 nz_diff_on=nz_diff_on)
        post = shock_means16(simulate_path16(pre_env, env, p.sep, spec))
        key = (benefit_on, common_on, nz_diff_on)
        values[key] = post
        push!(rows, (
            scenario="B$(Int(benefit_on))_C$(Int(common_on))_N$(Int(nz_diff_on))",
            benefit_on=benefit_on, common_shocks_on=common_on,
            nz_differential_on=nz_diff_on,
            find_AB=post.finding[AB_16], find_A0=post.finding[A0_16],
            find_NZ=post.finding[NZ_16],
            sep_AB=post.separation[AB_16], sep_A0=post.separation[A0_16],
            sep_NZ=post.separation[NZ_16],
            search_AB=post.search[AB_16], search_A0=post.search[A0_16],
            search_NZ=post.search[NZ_16],
            accept_AB=post.accept[AB_16], accept_A0=post.accept[A0_16],
            accept_NZ=post.accept[NZ_16],
            contact=post.contact,
            main_find_did_pp=did_pp16(pre.finding[AB_16],
                                      pre.finding[NZ_16],
                                      post.finding[AB_16],
                                      post.finding[NZ_16]),
            placebo_find_did_pp=did_pp16(pre.finding[A0_16],
                                         pre.finding[NZ_16],
                                         post.finding[A0_16],
                                         post.finding[NZ_16]),
            main_sep_did_pp=did_pp16(pre.separation[AB_16],
                                     pre.separation[NZ_16],
                                     post.separation[AB_16],
                                     post.separation[NZ_16]),
            placebo_sep_did_pp=did_pp16(pre.separation[A0_16],
                                        pre.separation[NZ_16],
                                        post.separation[A0_16],
                                        post.separation[NZ_16]),
        ))
    end
    return DataFrame(rows), values
end

function shapley_output16(pre, values)
    permutations = ((1,2,3), (1,3,2), (2,1,3),
                    (2,3,1), (3,1,2), (3,2,1))
    names = ("benefit", "common_shocks", "nz_differential")
    rows = NamedTuple[]

    outcome_value(post, outcome::Symbol) = begin
        ypre = getproperty(pre, outcome)
        ypost = getproperty(post, outcome)
        did_pp16(ypre[AB_16], ypre[NZ_16],
                 ypost[AB_16], ypost[NZ_16])
    end

    for outcome in (:finding, :separation)
        contributions = zeros(3)
        for perm in permutations
            state = (false, false, false)
            old_value = outcome_value(values[state], outcome)
            for factor in perm
                state_vec = collect(state)
                state_vec[factor] = true
                new_state = Tuple(state_vec)
                new_value = outcome_value(values[new_state], outcome)
                contributions[factor] += new_value - old_value
                state = new_state
                old_value = new_value
            end
        end
        contributions ./= length(permutations)
        total = outcome_value(values[(true,true,true)], outcome) -
                outcome_value(values[(false,false,false)], outcome)
        for i in 1:3
            push!(rows, (outcome=String(outcome), factor=names[i],
                         shapley_contribution_pp=contributions[i],
                         full_change_pp=total,
                         share_of_full=abs(total) < 1e-10 ? NaN :
                                       contributions[i] / total))
        end
    end
    return DataFrame(rows)
end

function parameter_output16(p::CalibrationParams16, spec::ModelSpec16,
                            loss::Float64)
    return DataFrame([
        (parameter="beta_weekly", value=spec.beta,
         status="fixed", evidence="0.96 annual factor converted to weekly"),
        (parameter="mu_pre", value=p.mu_pre,
         status="estimated", evidence="selected-sample pre job-finding moments"),
        (parameter="kappa", value=p.kappa,
         status=spec.channel == :acceptance_only ? "fixed" : "estimated",
         evidence="selected-sample job-finding levels"),
        (parameter="mu_post", value=p.mu_post,
         status="estimated", evidence="untreated Australian and repeated NZ post changes"),
        (parameter="common_sep_shock", value=p.common_sep_shock,
         status="estimated", evidence="untreated Australian and repeated NZ separations"),
        (parameter="nz_work_cost_diff", value=p.nz_work_cost_diff,
         status="estimated", evidence="non-recipient Australian-NZ placebo DiDs"),
        (parameter="sep_base", value=p.sep.base,
         status="estimated", evidence="pre-period separation levels"),
        (parameter="sep_amp", value=p.sep.amp,
         status="estimated", evidence="recipient separation response conditional on common shock"),
        (parameter="benefit_pre", value=benefit_pre16(spec),
         status="institutional", evidence="base replacement-rate normalisation"),
        (parameter="benefit_post", value=benefit_post16(spec),
         status="institutional", evidence="Coronavirus Supplement multiplier"),
        (parameter="search_curvature", value=spec.search_curvature,
         status="fixed/sensitivity", evidence="toy-model functional form"),
        (parameter="offer_threshold", value=spec.offer_threshold,
         status="fixed/sensitivity", evidence="toy-model functional form"),
        (parameter="offer_dispersion", value=spec.offer_dispersion,
         status="fixed/sensitivity", evidence="toy-model functional form"),
        (parameter="separation_threshold", value=spec.separation_threshold,
         status="fixed/sensitivity", evidence="version-15 preferred restricted specification"),
        (parameter="separation_dispersion", value=spec.separation_dispersion,
         status="fixed/sensitivity", evidence="version-15 preferred restricted specification"),
        (parameter="weighted_loss", value=loss,
         status="diagnostic", evidence="sum of squared standardised moment residuals"),
    ])
end

function identification_output16()
    return DataFrame([
        (object="AB structural agent", treatment="modelled",
         source="Australian JobSeeker recipients",
         interpretation="homogeneous representative recipient in selected sample"),
        (object="A0 structural agent", treatment="modelled",
         source="Australians not receiving JobSeeker",
         interpretation="homogeneous untreated Australian/placebo group"),
        (object="NZ structural agent", treatment="modelled once",
         source="both matched NZ panels",
         interpretation="one homogeneous NZ type with one value function and post response"),
        (object="NZ main measurement offset", treatment="measurement only",
         source="NZ panel matched to recipients",
         interpretation="time-invariant level adjustment; no separate behaviour or shock"),
        (object="NZ placebo measurement offset", treatment="measurement only",
         source="NZ panel matched to non-recipients",
         interpretation="time-invariant level adjustment; no separate behaviour or shock"),
        (object="common job-opportunity shock", treatment="estimated from selected sample",
         source="A0 and repeated NZ post changes",
         interpretation="public vacancy series is external validation, not a level target"),
        (object="common separation shock", treatment="estimated from selected sample",
         source="A0 and repeated NZ separation changes",
         interpretation="public employer evidence is external validation"),
        (object="NZ differential work cost", treatment="estimated/weakly disciplined",
         source="placebo Australian-NZ DiDs",
         interpretation="reduced-form health, family support, or risk-preference difference"),
        (object="Supplement mechanism", treatment="modelled",
         source="recipient main DiDs after common/NZ shocks",
         interpretation="change in value of non-employment affects finding and continuation"),
    ])
end

function spec_for_channel16(channel::Symbol)
    return ModelSpec16(channel=channel)
end

function channel_comparison16(targets::DataFrame)
    rows = NamedTuple[]
    calibrations = Dict{Symbol,Any}()
    for channel in (:combined, :search_only, :acceptance_only)
        spec = spec_for_channel16(channel)
        p, pre, post, path, predictions, loss =
            calibrate_model16(spec, targets)
        scenarios, _ = factorial_scenarios16(p, pre, spec)
        full = scenarios[(scenarios.benefit_on .== true) .&
                         (scenarios.common_shocks_on .== true) .&
                         (scenarios.nz_differential_on .== true), :][1, :]
        benefit_only = scenarios[(scenarios.benefit_on .== true) .&
                                 (scenarios.common_shocks_on .== false) .&
                                 (scenarios.nz_differential_on .== false), :][1, :]
        push!(rows, (
            channel=String(channel), weighted_loss=loss,
            mu_pre=p.mu_pre, kappa=p.kappa, mu_post=p.mu_post,
            common_sep_shock=p.common_sep_shock,
            nz_work_cost_diff=p.nz_work_cost_diff,
            sep_base=p.sep.base, sep_amp=p.sep.amp,
            full_main_find_did_pp=full.main_find_did_pp,
            benefit_only_main_find_did_pp=benefit_only.main_find_did_pp,
            data_main_find_did_pp=-1.71,
            full_placebo_find_did_pp=full.placebo_find_did_pp,
            data_placebo_find_did_pp=-0.11,
            full_main_sep_did_pp=full.main_sep_did_pp,
            benefit_only_main_sep_did_pp=benefit_only.main_sep_did_pp,
            data_main_sep_did_pp=3.72,
            full_placebo_sep_did_pp=full.placebo_sep_did_pp,
            data_placebo_sep_did_pp=-0.36,
            find_AB_pre=pre.finding[AB_16], find_AB_post=post.finding[AB_16],
            find_NZ_pre=pre.finding[NZ_16], find_NZ_post=post.finding[NZ_16],
            sep_AB_pre=pre.separation[AB_16], sep_AB_post=post.separation[AB_16],
            sep_NZ_pre=pre.separation[NZ_16], sep_NZ_post=post.separation[NZ_16],
        ))
        calibrations[channel] = (spec=spec, params=p, pre=pre, post=post,
                                 path=path, predictions=predictions, loss=loss)
    end
    return DataFrame(rows), calibrations
end

function print_results16(regression::DataFrame, params::DataFrame,
                         shapley::DataFrame, channel_comparison::DataFrame)
    println("\n16_ three-agent toy-model calibration")
    println("--------------------------------------")
    for row in eachrow(regression)
        @printf("%-10s %-8s %-12s data=% .4f model=% .4f gap=% .4f (% .2f se)\n",
                row.outcome, row.panel, row.term, row.data, row.model,
                row.gap, row.standardized_gap)
    end
    println("\nPreferred combined-model parameters")
    println("-----------------------------------")
    for row in eachrow(params)
        @printf("%-25s % .6f  [%s]\n", row.parameter, row.value, row.status)
    end
    println("\nShapley decomposition of the main DiD")
    println("-------------------------------------")
    for row in eachrow(shapley)
        @printf("%-12s %-18s % .3f pp\n", row.outcome, row.factor,
                row.shapley_contribution_pp)
    end
    println("\nChannel comparison")
    println("------------------")
    for row in eachrow(channel_comparison)
        @printf("%-16s loss=%7.3f main JFR=% .3f pp main sep=% .3f pp\n",
                row.channel, row.weighted_loss,
                row.full_main_find_did_pp, row.full_main_sep_did_pp)
    end
end

function run_model16()
    targets = regression_targets16()
    channel_table, calibrations = channel_comparison16(targets)
    preferred = calibrations[:combined]
    p = preferred.params
    spec = preferred.spec
    pre = preferred.pre
    post = preferred.post
    path = preferred.path
    predictions = preferred.predictions
    loss = preferred.loss

    regression = regression_output16(targets, predictions)
    cells = cell_output16(pre, post, targets)
    scenarios, factorial_values = factorial_scenarios16(p, pre, spec)
    shapley = shapley_output16(pre, factorial_values)
    params = parameter_output16(p, spec, loss)
    identification = identification_output16()

    CSV.write("16_three_agent_regression_moments.csv", regression)
    CSV.write("16_three_agent_cells.csv", cells)
    CSV.write("16_three_agent_path.csv", path)
    CSV.write("16_three_agent_scenarios.csv", scenarios)
    CSV.write("16_three_agent_shapley.csv", shapley)
    CSV.write("16_three_agent_parameters.csv", params)
    CSV.write("16_three_agent_identification.csv", identification)
    CSV.write("16_channel_comparison.csv", channel_table)

    print_results16(regression, params, shapley, channel_table)
    println("\nSaved version-16 outputs (8 CSV files).")
    return regression, cells, path, scenarios, shapley, params,
           identification, channel_table
end

if abspath(PROGRAM_FILE) == @__FILE__
    run_model16()
end
