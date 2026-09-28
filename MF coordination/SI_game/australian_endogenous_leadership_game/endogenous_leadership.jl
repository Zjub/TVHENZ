module EndogenousLeadership

using LinearAlgebra

export Calibration, Mandate, SocialWeights, GameModel, Regime,
       model_matrices, authority_loss, social_loss,
       solve_static_games, solve_stationary_nash, solve_stationary_cooperation,
       search_stationary_nash, simulate_rule, evaluate_path,
       leadership_index, rule_distance, strategic_wedge_share

Base.@kwdef struct Calibration
    rho_x::Float64 = 0.6287494507727328
    rho_pi::Float64 = 0.5625497798601921
    sigma::Float64 = 0.036948002726257784
    fiscal_multiplier::Float64 = 0.051421355523204385
    kappa::Float64 = 0.15510594039123907
    rho_b::Float64 = 0.995
    debt_from_interest::Float64 = 0.015
    debt_from_fiscal::Float64 = 0.060
    debt_from_inflation::Float64 = 0.100
    rho_d::Float64 = 0.0
    rho_u::Float64 = 0.0
end

"""An agency mandate. Each authority bears only its own instrument costs."""
Base.@kwdef struct Mandate
    inflation::Float64
    output::Float64
    debt::Float64
    own_level::Float64 = 0.10
    discount::Float64 = 0.99
end

"""Fixed true social weights; actual adjustment costs are supplied by the regime."""
Base.@kwdef struct SocialWeights
    inflation::Float64 = 1.0
    output::Float64 = 0.5
    debt::Float64 = 0.15
    monetary_level::Float64 = 0.10
    fiscal_level::Float64 = 0.10
    discount::Float64 = 0.99
end

Base.@kwdef struct GameModel
    calibration::Calibration = Calibration()
    monetary::Mandate = Mandate(inflation=1.5, output=0.25, debt=0.15)
    fiscal::Mandate = Mandate(inflation=0.5, output=0.75, debt=0.15)
    social::SocialWeights = SocialWeights()
end

"""Exogenous commitment capacities. These coefficients are never chosen by players."""
Base.@kwdef struct Regime
    name::String
    monetary_adjustment::Float64 = 0.0
    fiscal_adjustment::Float64 = 0.0
end

"""
Recursive Australian block with state
`[x_lag, pi_lag, debt, m_lag, f_lag, demand_shock, cost_push_shock]`.
Positive `m` is tightening and positive `f` is fiscal expansion.
"""
function model_matrices(c::Calibration)
    n, mi, fi = 7, 8, 9
    outcomes = zeros(3, 9)
    outcomes[1, 1] = c.rho_x
    outcomes[1, 2] = c.sigma
    outcomes[1, 6] = 1.0
    outcomes[1, mi] = -c.sigma
    outcomes[1, fi] = c.fiscal_multiplier
    outcomes[2, :] .= c.kappa .* outcomes[1, :]
    outcomes[2, 2] += c.rho_pi
    outcomes[2, 7] += 1.0
    outcomes[3, 3] = c.rho_b
    outcomes[3, mi] += c.debt_from_interest
    outcomes[3, fi] += c.debt_from_fiscal
    outcomes[3, :] .-= c.debt_from_inflation .* outcomes[2, :]

    transition = zeros(n, 9)
    transition[1:3, :] .= outcomes
    transition[4, mi] = 1.0
    transition[5, fi] = 1.0
    transition[6, 6] = c.rho_d
    transition[7, 7] = c.rho_u
    return transition[:, 1:n], transition[:, mi], transition[:, fi], outcomes
end

"""Map `[state; m; f]` to macro outcomes, policy levels, and policy changes."""
function target_map(c::Calibration)
    _, _, _, outcomes = model_matrices(c)
    map = zeros(7, 9)
    map[1:3, :] .= outcomes
    map[4, 8] = 1.0
    map[5, 9] = 1.0
    map[6, 4] = -1.0
    map[6, 8] = 1.0
    map[7, 5] = -1.0
    map[7, 9] = 1.0
    return map
end

"""Agency loss: macro mandate plus only the authority's own implementation cost."""
function authority_loss(model::GameModel, regime::Regime, authority::Symbol)
    map = target_map(model.calibration)
    if authority == :monetary
        w = model.monetary
        diagonal = [w.output, w.inflation, w.debt, w.own_level, 0.0,
                    regime.monetary_adjustment, 0.0]
    elseif authority == :fiscal
        w = model.fiscal
        diagonal = [w.output, w.inflation, w.debt, 0.0, w.own_level,
                    0.0, regime.fiscal_adjustment]
    else
        error("authority must be :monetary or :fiscal")
    end
    return Matrix(Symmetric(map' * Diagonal(diagonal) * map))
end

"""True social loss with the physical adjustment costs present in the regime."""
function social_loss(model::GameModel, regime::Regime; include_adjustment=true)
    w = model.social
    lm = include_adjustment ? regime.monetary_adjustment : 0.0
    lf = include_adjustment ? regime.fiscal_adjustment : 0.0
    diagonal = [w.output, w.inflation, w.debt,
                w.monetary_level, w.fiscal_level, lm, lf]
    map = target_map(model.calibration)
    return Matrix(Symmetric(map' * Diagonal(diagonal) * map))
end

"""Return a common rule container and its closed-loop transition."""
function make_rule(name, km, kf, A, bm, bf; metadata=NamedTuple())
    transition = A + reshape(bm, :, 1) * km' + reshape(bf, :, 1) * kf'
    return (name=name, monetary_rule=Vector(km), fiscal_rule=Vector(kf),
            transition=Matrix(transition), spectral_radius=maximum(abs, eigvals(transition)),
            metadata=metadata)
end

"""
Static one-period benchmark rules: simultaneous Nash, both Stackelberg orders,
and cooperation. Adjustment costs are absent. Applying a rule repeatedly is a
benchmark, not a dynamic optimum.
"""
function solve_static_games(model::GameModel)
    zero_cost = Regime(name="static", monetary_adjustment=0.0, fiscal_adjustment=0.0)
    Qm = authority_loss(model, zero_cost, :monetary)
    Qf = authority_loss(model, zero_cost, :fiscal)
    Qs = social_loss(model, zero_cost)
    A, bm, bf, _ = model_matrices(model.calibration)
    n, z, mi, fi = 7, 1:7, 8, 9

    # Simultaneous static Nash.
    response = [Qm[mi, mi] Qm[mi, fi]; Qf[fi, mi] Qf[fi, fi]]
    rules_nash = -(response \ vcat(reshape(Qm[mi, z], 1, :), reshape(Qf[fi, z], 1, :)))

    # Monetary leadership: fiscal policy is the within-period follower.
    rf_s = -vec(Qf[fi, z]) / Qf[fi, fi]
    rf_m = -Qf[fi, mi] / Qf[fi, fi]
    transform_m = zeros(9, 8)
    transform_m[1:7, 1:7] .= Matrix{Float64}(I, 7, 7)
    transform_m[mi, 8] = 1.0
    transform_m[fi, 1:7] .= rf_s
    transform_m[fi, 8] = rf_m
    Hm = Matrix(Symmetric(transform_m' * Qm * transform_m))
    km_lead = -vec(Hm[8, 1:7]) / Hm[8, 8]
    kf_follow = rf_s + rf_m .* km_lead

    # Fiscal leadership: monetary policy is the within-period follower.
    rm_s = -vec(Qm[mi, z]) / Qm[mi, mi]
    rm_f = -Qm[mi, fi] / Qm[mi, mi]
    transform_f = zeros(9, 8)
    transform_f[1:7, 1:7] .= Matrix{Float64}(I, 7, 7)
    transform_f[fi, 8] = 1.0
    transform_f[mi, 1:7] .= rm_s
    transform_f[mi, 8] = rm_f
    Hf = Matrix(Symmetric(transform_f' * Qf * transform_f))
    kf_lead = -vec(Hf[8, 1:7]) / Hf[8, 8]
    km_follow = rm_s + rm_f .* kf_lead

    # Static cooperative regulator.
    coop_rules = -(Qs[[mi, fi], [mi, fi]] \ Qs[[mi, fi], z])

    return Dict(
        "Static Nash" => make_rule("Static Nash", vec(rules_nash[1, :]), vec(rules_nash[2, :]), A, bm, bf),
        "Monetary leader" => make_rule("Monetary leader", km_lead, kf_follow, A, bm, bf;
            metadata=(follower_state=rf_s, follower_leader_slope=rf_m)),
        "Fiscal leader" => make_rule("Fiscal leader", km_follow, kf_lead, A, bm, bf;
            metadata=(follower_state=rm_s, follower_leader_slope=rm_f)),
        "Static cooperation" => make_rule("Static cooperation", vec(coop_rules[1, :]), vec(coop_rules[2, :]), A, bm, bf),
    )
end

"""Stationary Markov-perfect simultaneous Nash equilibrium from one Riccati seed."""
function solve_stationary_nash(model::GameModel, regime::Regime;
                               seed_scale=1.0, tolerance=1e-11,
                               max_iterations=50_000)
    A, bm, bf, _ = model_matrices(model.calibration)
    Bm, Bf = reshape(bm, :, 1), reshape(bf, :, 1)
    Qm = authority_loss(model, regime, :monetary)
    Qf = authority_loss(model, regime, :fiscal)
    n, z, mi, fi = 7, 1:7, 8, 9
    Pm = seed_scale .* Matrix(Qm[z, z])
    Pf = seed_scale .* Matrix(Qf[z, z])
    km, kf = zeros(n), zeros(n)
    beta_m, beta_f = model.monetary.discount, model.fiscal.discount
    residual = Inf
    for iteration in 1:max_iterations
        response = [
            Qm[mi, mi] + beta_m * dot(Bm, Pm * Bm)  Qm[mi, fi] + beta_m * dot(Bm, Pm * Bf)
            Qf[fi, mi] + beta_f * dot(Bf, Pf * Bm)  Qf[fi, fi] + beta_f * dot(Bf, Pf * Bf)
        ]
        abs(det(response)) > 1e-13 || error("singular dynamic response system for $(regime.name)")
        state_terms = vcat(
            reshape(Qm[mi, z] + beta_m * vec(Bm' * Pm * A), 1, :),
            reshape(Qf[fi, z] + beta_f * vec(Bf' * Pf * A), 1, :),
        )
        rules = -(response \ state_terms)
        km_new, kf_new = vec(rules[1, :]), vec(rules[2, :])
        acl = A + Bm * km_new' + Bf * kf_new'
        selector = vcat(Matrix{Float64}(I, n, n), rules)
        Pm_new = Matrix(Symmetric(selector' * Qm * selector + beta_m * acl' * Pm * acl))
        Pf_new = Matrix(Symmetric(selector' * Qf * selector + beta_f * acl' * Pf * acl))
        residual = maximum((maximum(abs, Pm_new - Pm), maximum(abs, Pf_new - Pf),
                            maximum(abs, km_new - km), maximum(abs, kf_new - kf)))
        Pm, Pf, km, kf = Pm_new, Pf_new, km_new, kf_new
        if residual < tolerance
            rule = make_rule(regime.name, km, kf, A, bm, bf;
                metadata=(regime=regime, monetary_value=Pm, fiscal_value=Pf,
                          iterations=iteration, residual=residual, converged=true,
                          seed_scale=seed_scale))
            return rule
        end
    end
    rule = make_rule(regime.name, km, kf, A, bm, bf;
        metadata=(regime=regime, monetary_value=Pm, fiscal_value=Pf,
                  iterations=max_iterations, residual=residual, converged=false,
                  seed_scale=seed_scale))
    return rule
end

"""Multi-start diagnostic for stationary Markov-perfect equilibria."""
function search_stationary_nash(model::GameModel, regime::Regime;
                                seeds=[0.0, 0.01, 0.1, 1.0, 10.0, 100.0])
    attempts = [solve_stationary_nash(model, regime; seed_scale=s) for s in seeds]
    stable = [a for a in attempts if a.metadata.converged && a.spectral_radius < 1.0]
    unique = NamedTuple[]
    for candidate in stable
        duplicate = any(rule_distance(candidate, item) < 1e-7 for item in unique)
        duplicate || push!(unique, candidate)
    end
    return (attempts=attempts, solutions=unique)
end

"""Stationary cooperative regulator under the regime's physical costs."""
function solve_stationary_cooperation(model::GameModel, regime::Regime;
                                      tolerance=1e-11, max_iterations=50_000)
    A, bm, bf, _ = model_matrices(model.calibration)
    B = hcat(bm, bf)
    Q = social_loss(model, regime)
    z, u = 1:7, 8:9
    P, rules = Matrix(Q[z, z]), zeros(2, 7)
    beta = model.social.discount
    residual = Inf
    for iteration in 1:max_iterations
        new_rules = -((Q[u, u] + beta * B' * P * B) \
                      (Q[u, z] + beta * B' * P * A))
        acl = A + B * new_rules
        selector = vcat(Matrix{Float64}(I, 7, 7), new_rules)
        Pnew = Matrix(Symmetric(selector' * Q * selector + beta * acl' * P * acl))
        residual = max(maximum(abs, Pnew - P), maximum(abs, new_rules - rules))
        P, rules = Pnew, new_rules
        if residual < tolerance
            return make_rule("Coordination", vec(rules[1, :]), vec(rules[2, :]), A, bm, bf;
                metadata=(regime=regime, value=P, iterations=iteration,
                          residual=residual, converged=true))
        end
    end
    return make_rule("Coordination", vec(rules[1, :]), vec(rules[2, :]), A, bm, bf;
        metadata=(regime=regime, value=P, iterations=max_iterations,
                  residual=residual, converged=false))
end

"""Simulate a stationary policy rule after an initial shock."""
function simulate_rule(rule, model::GameModel, initial_state; horizon=40)
    A, bm, bf, _ = model_matrices(model.calibration)
    state = Float64.(initial_state)
    states = zeros(7, horizon + 1)
    states[:, 1] .= state
    monetary, fiscal = zeros(horizon), zeros(horizon)
    for t in 1:horizon
        monetary[t] = dot(rule.monetary_rule, state)
        fiscal[t] = dot(rule.fiscal_rule, state)
        state = A * state + bm * monetary[t] + bf * fiscal[t]
        states[:, t + 1] .= state
    end
    return (states=states, output=vec(states[1, 2:end]),
            inflation=vec(states[2, 2:end]), debt=vec(states[3, 2:end]),
            monetary=monetary, fiscal=fiscal)
end

"""Evaluate agency loss, actual social loss, and a common macro-welfare yardstick."""
function evaluate_path(path, model::GameModel, regime::Regime)
    matrices = (
        authority_loss(model, regime, :monetary),
        authority_loss(model, regime, :fiscal),
        social_loss(model, regime),
        social_loss(model, regime; include_adjustment=false),
    )
    discounts = (model.monetary.discount, model.fiscal.discount,
                 model.social.discount, model.social.discount)
    values = zeros(4)
    for t in eachindex(path.monetary)
        joint = vcat(path.states[:, t], path.monetary[t], path.fiscal[t])
        for k in 1:4
            values[k] += discounts[k]^(t - 1) * dot(joint, matrices[k] * joint)
        end
    end
    return (monetary=values[1], fiscal=values[2], social=values[3],
            common_macro=values[4])
end

rule_distance(a, b) = norm(vcat(a.monetary_rule - b.monetary_rule,
                                a.fiscal_rule - b.fiscal_rule))

"""Fraction of the flexible-Nash distance to a Stackelberg rule closed by a regime."""
function leadership_index(rule, flexible, leader)
    baseline = rule_distance(flexible, leader)
    baseline > 1e-12 || return NaN
    return 1.0 - rule_distance(rule, leader) / baseline
end

"""
Envelope decomposition of the current authority's continuation derivative.

The authority's current action changes next period's state.  In the next
period the rival's Markov rule responds to that state.  `strategic` is the
exact component of the current first-order condition that operates through
this rival response; `total_continuation` is the complete continuation term.
The bounded share is `|strategic|/(|strategic|+|non-strategic|)`, so it remains
well behaved when the two components partly cancel.
"""
function strategic_wedge_share(rule, model::GameModel, regime::Regime,
                                initial_state, authority::Symbol)
    A, bm, bf, _ = model_matrices(model.calibration)
    state0 = Float64.(initial_state)
    m0 = dot(rule.monetary_rule, state0)
    f0 = dot(rule.fiscal_rule, state0)
    state1 = A * state0 + bm * m0 + bf * f0
    m1 = dot(rule.monetary_rule, state1)
    f1 = dot(rule.fiscal_rule, state1)
    state2 = A * state1 + bm * m1 + bf * f1
    joint1 = vcat(state1, m1, f1)

    if authority == :monetary
        Q = authority_loss(model, regime, :monetary)
        P = rule.metadata.monetary_value
        beta = model.monetary.discount
        # Total marginal value to M of the rival's next fiscal action.
        rival_hamiltonian = 2.0 * (dot(Q[9, :], joint1) + beta * dot(bf, P * state2))
        strategic = beta * dot(rule.fiscal_rule, bm) * rival_hamiltonian
        total = 2.0 * beta * dot(bm, P * state1)
    elseif authority == :fiscal
        Q = authority_loss(model, regime, :fiscal)
        P = rule.metadata.fiscal_value
        beta = model.fiscal.discount
        # Total marginal value to F of the rival's next monetary action.
        rival_hamiltonian = 2.0 * (dot(Q[8, :], joint1) + beta * dot(bm, P * state2))
        strategic = beta * dot(rule.monetary_rule, bf) * rival_hamiltonian
        total = 2.0 * beta * dot(bf, P * state1)
    else
        error("authority must be :monetary or :fiscal")
    end

    nonstrategic = total - strategic
    bounded_share = abs(strategic) /
                    (abs(strategic) + abs(nonstrategic) + eps(Float64))
    return (strategic=strategic, nonstrategic=nonstrategic,
            total_continuation=total, bounded_share=bounded_share)
end

end # module
