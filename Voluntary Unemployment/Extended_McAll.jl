# Implement a simple McCall model to work out what we would like to adjust
# 10/04/2024
# Author: Matt Nolan

import Pkg
Pkg.add(["Distributions", "LinearAlgebra", "Statistics","Expectations","LaTeXStrings","NLsolve","Roots","Random","Interpolations", "Plots", "Optim","QuantEcon"])

using LinearAlgebra, Statistics
using Distributions, Expectations, LaTeXStrings
using NLsolve, Roots, Random, Plots, QuantEcon

function simulate_linear(A, x_0, T)
    X = zeros(length(x_0), T + 1)
    X[:, 1] = x_0
    for t in 2:(T + 1)
        X[:, t] = A * X[:, t - 1]
    end
    return X
end

function linear_steady_state(A, x_0 = ones(size(A, 1)) / size(A, 1))
    sol = fixedpoint(x -> A * x, x_0)
    converged(sol) || error("Failed to converge in $(sol.iterations) iter")
    return sol.zero
end

function LakeModel(; lambda = 0.283, alpha = 0.013, b = 0.0124, d = 0.00822)
    # calculate transition matrices
    g = b - d
    A = [(1 - lambda) * (1 - d)+b (1 - d) * alpha+b
         (1 - d)*lambda (1 - d)*(1 - alpha)]
    A_hat = A ./ (1 + g)

    # Solve for fixed point to find the steady-state u and e
    x_bar = linear_steady_state(A_hat)
    return (; lambda, alpha, b, d, A, A_hat, x_bar)
end

lm = LakeModel()
lm.A
lm.A_hat

lm = LakeModel(; alpha = 0.2)
lm.A
lm.A_hat



## Extension of base McCall model that uses exogenous separation rate.


# Set globals and define expected utility
using Optim

# Assume these are defined globally or passed in as parameters
linear_increase_factor = 0.05 # Adjusts the sensitivity of the switch probability to effort
quadratic_cost_factor = 0.1  # Adjusts how quickly costs rise with effort
maximum_effort = 1.0         # Maximum feasible effort

# Expected utility function incorporating effort
function expected_utility(u, w, sigma, effort, base_job_switch_cost, w_probs, beta, alpha, gamma, c)
    # Calculate job switch probability and cost based on effort
    p_switch = min(effort * linear_increase_factor, 1.0)
    cost_switch = base_job_switch_cost + quadratic_cost_factor * effort^2
    
    # Adjusted utility for each wage considering the cost of switching
    u_w_adjusted = u.(w .- cost_switch, sigma) - effort^2  # Disutility of effort
    u_c = u(c, sigma)  # Utility from unemployment compensation
    # Expected utility calculation considering job switch probability and staying unemployed
    EU = p_switch * dot(w_probs, max.(u_c, u_w_adjusted)) + (1 - p_switch) * u_c - effort^2
    
    return EU
end

# Function to optimize effort for a given set of parameters
function optimize_effort_for_wage(u, w, sigma, base_job_switch_cost, w_probs, beta, alpha, gamma, c)
    objective(effort) = -expected_utility(u, w, sigma, effort[1], base_job_switch_cost, w_probs, beta, alpha, gamma, c)  # Minimize negative utility
    result = optimize(objective, [0.0], [maximum_effort], [0.5], Fminbox(BFGS()))  # Optimizing within effort bounds, starting from 0.5
    
    return result.minimizer[1], -result.minimum  # Return optimal effort and maximum utility
end

# Main function solving the McCall model with job switch decisions based on optimized effort
function solve_mccall_model(mcm; U_iv = 1.0, V_iv = ones(length(mcm.w)), base_job_switch_cost = 0.5, tol = 1e-5, iter = 2_000)
    # Unpack parameters
    (; alpha, beta, sigma, c, gamma, w, w_probs, u) = mcm

    # Adjusted Bellman operator for job switching with optimized effort
    function T(x)
        V = x[1:(end - 1)]
        U = x[end]
        optimal_utilities = zeros(length(w))
        optimal_efforts = zeros(length(w))
        
        for (i, wage) in enumerate(w)
            # Optimize effort for each wage
            opt_effort, utility = optimize_effort_for_wage(u, wage, sigma, base_job_switch_cost, w_probs, beta, alpha, gamma, c)
            optimal_utilities[i] = utility
            optimal_efforts[i] = opt_effort
        end
        
        # Proceed with expected utility calculations using optimal efforts
        E_switch = dot(w_probs, max.(U, optimal_utilities))
        
        return [optimal_utilities + beta * ((1 - alpha) * V .+ alpha * U); u(c, sigma) + beta * (1 - gamma) * U + beta * gamma * E_switch]
    end

    # Value function iteration 
    x_iv = [V_iv; U_iv] # Initial values
    xstar = fixedpoint(T, x_iv, iterations = iter, xtol = tol, m = 0).zero
    V = xstar[1:(end - 1)]
    U = xstar[end]

    # Compute the reservation wage 
    w_barindex = searchsortedfirst(V .- U, 0.0)
    w_bar = w_barindex >= length(w) ? Inf : w[w_barindex]

    # Return a NamedTuple, so we can select values by name
    return (; V, U, w_bar, optimal_efforts)
end



function McCallModel(; alpha, beta, gamma, c, sigma, w, w_probs,
    u = (c, sigma) -> c > 0 ?
                      (c^(1 - sigma) - 1) / (1 - sigma) :
                      -10e-6)
return (; alpha, beta, gamma, c, sigma, u, w, w_probs)
end

function compute_optimal_quantities(c_pretax, tau; w_probs, sigma, gamma, beta,
    alpha, w_pretax)
mcm = McCallModel(; alpha, beta, gamma, sigma, w_probs,
c = c_pretax - tau, # post-tax compensation
w = w_pretax .- tau)

(; V, U, w_bar) = solve_mccall_model(mcm)
accept_wage = w_pretax .- tau .> w_bar

# sum up proportion accepting the wages
lambda = gamma * dot(w_probs, accept_wage)
return w_bar, lambda, V, U
end

function compute_steady_state_quantities(c_pretax, tau; w_probs, sigma, gamma,
         beta, alpha, w_pretax, b, d)
w_bar, lambda, V, U = compute_optimal_quantities(c_pretax, tau; w_probs,
                     sigma,
                     gamma, beta, alpha,
                     w_pretax)

# compute steady state employment and unemployment rates
lm = LakeModel(; lambda, alpha, b, d)
u_rate, e_rate = lm.x_bar

# compute steady state welfare
accept_wage = w_pretax .- tau .> w_bar
w = (dot(w_probs, V .* accept_wage)) / dot(w_probs, accept_wage)
welfare = e_rate .* w + u_rate .* U

return u_rate, e_rate, welfare
end

function find_balanced_budget_tax(c_pretax; w_probs, sigma, gamma, beta, alpha,
  w_pretax, b, d)
function steady_state_budget(t)
u_rate, e_rate, w = compute_steady_state_quantities(c_pretax, t;
                            w_probs, sigma,
                            gamma, beta, alpha,
                            w_pretax, b, d)
return t - u_rate * c_pretax
end

# Assuming `steady_state_budget` is your function and `initial_guess` is your best guess for where the root might be
#initial_guess = 0.5  # Example initial guess, adjust based on your expectations
#tau = find_zero(steady_state_budget, initial_guess, Roots.Newton())

tau = find_zero(steady_state_budget, (0.0, 1.9 * c_pretax))
return tau
end

function calculate_equilibriums(c_pretax; w_probs, sigma, gamma, beta, alpha, w_pretax, b, d)
    println("Starting calculate_equilibriums with c_pretax range: ", c_pretax)  # Debug print for input range
    tau_vec = similar(c_pretax)
    u_vec = similar(c_pretax)
    e_vec = similar(c_pretax)
    welfare_vec = similar(c_pretax)
    
    for (i, c_pre) in enumerate(c_pretax)
        println("Calculating for c_pretax value: ", c_pre, " at index: ", i)  # Debug print for each iteration due to floating point errors (urg)
        tau = find_balanced_budget_tax(c_pre; w_probs, sigma, gamma, beta, alpha, w_pretax, b, d)
        u_rate, e_rate, welfare = compute_steady_state_quantities(c_pre, tau; w_probs, sigma, gamma, beta, alpha, w_pretax, b, d)
        tau_vec[i] = tau
        u_vec[i] = u_rate
        e_vec[i] = e_rate
        welfare_vec[i] = welfare
    end
    
    return tau_vec, u_vec, e_vec, welfare_vec
end

alpha_base = 0.013
alpha = (1 - (1 - alpha_base)^3)
b = 0.0124
d = 0.00822
beta = 0.98
gamma = 1.0
sigma = 2.0

# the default wage distribution: a discretized log normal
log_wage_mean, wage_grid_size, max_wage = 20, 200, 170
w_pretax = range(1e-3, max_wage, length = wage_grid_size + 1)
logw_dist = Normal(log(log_wage_mean), 1)
cdf_logw = cdf.(logw_dist, log.(w_pretax))
pdf_logw = cdf_logw[2:end] - cdf_logw[1:(end - 1)]
w_probs = pdf_logw ./ sum(pdf_logw) # probabilities
w_pretax = (w_pretax[1:(end - 1)] + w_pretax[2:end]) / 2

# levels of unemployment insurance we wish to study
c_pretax = range(5, 140, length = 60)
tau_vec, u_vec, e_vec, welfare_vec = calculate_equilibriums(c_pretax; w_probs,
                                                            sigma, gamma, beta,
                                                            alpha, w_pretax, b,
                                                            d)

# plots
plt_unemp = plot(title = "Unemployment", c_pretax, u_vec, color = :blue,
                 lw = 2, alpha = 0.7, label = "", grid = true)
plt_tax = plot(title = "Tax", c_pretax, tau_vec, color = :blue, lw = 2,
               alpha = 0.7, label = "", grid = true)
plt_emp = plot(title = "Employment", c_pretax, e_vec, color = :blue, lw = 2,
               alpha = 0.7, label = "", grid = true)
plt_welf = plot(title = "Welfare", c_pretax, welfare_vec, color = :blue, lw = 2,
                alpha = 0.7, label = "", grid = true)

plot(plt_unemp, plt_emp, plt_tax, plt_welf, layout = (2, 2), size = (800, 700))