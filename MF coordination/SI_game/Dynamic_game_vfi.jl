### Attempt at writing up the game in dynamic form - it has been a while so this might be rough.
## Current version iterates value functions, which is slow.
## Also no adjustment cost included (although the parameters have been defined), so need to adjust loss functions for this by adding loss and the previous value as a state variable.

# using Pkg
# Pkg.add("SymPy")
# Pkg.add("NLsolve")
# Pkg.add("Optim")
# Pkg.add("Plots")
# Pkg.add("DataFrames")
# Pkg.add("StatsPlots")

using SymPy
using Optim
using Plots
using DataFrames
using StatsPlots

params_gov = [
    1.5,   # gamma - intercept of inflation
    2.5,   # alpha - intercept of output
    0.5,   # bG - sensitivity of output to fiscal policy
    0.5,   # bB - sensitivity of output to monetary policy
    0.5,   # dG - sensitivity of inflation to fiscal policy
    0.5,   # dB - sensitivity of information to monetary policy
    0.5,   # eG - 
    0.5,   # eB
    0.5,   # theta_G_star
    0.5,   # theta_B_star
    0.0,   # mu_G
    1.0,   # mu_B
    0.9,   # delta - discount rate
    0.2,   # lambda_G - cost of adjusting f
    0.2,   # lambda_B - cost of adjusting m
    2.0,   # pi_G_star
    2.0,   # pi_B_star
    2.0,   # y_G_star
    2.0,   # y_B_star
    0.0,   # beta_G_star
    0.0    # beta_B_star
]

params_cb = params_gov

# Define the recursive Bellman equation for Government
function gov_bellman(f_val, m_val, params, V_G_next)
    # Unpack parameter values
    gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params

    # Substitute parameter values into the best response functions
    full_vals = Dict(
        gamma => gamma_val, alpha => alpha_val, bG => bG_val, bB => bB_val, 
        dG => dG_val, dB => dB_val, eG => eG_val, eB => eB_val, 
        theta_G_star => theta_G_star_val, theta_B_star => theta_B_star_val, 
        mu_G => mu_G_val, mu_B => mu_B_val, delta => delta_val, 
        lambda_G => lambda_G_val, lambda_B => lambda_B_val,
        pi_G_star => pi_G_star_val, pi_B_star => pi_B_star_val, 
        y_G_star => y_G_star_val, y_B_star => y_B_star_val, 
        beta_G_star => beta_G_star_val, beta_B_star => beta_B_star_val
    )

    # Government's loss function at time t
    L_G = mu_G * (pi_G_star - (gamma - dG * f_val - dB * m_val))^2 +
    (1 - mu_G) * (y_G_star - (alpha - bG * f_val - bB * m_val))^2 +
    theta_G_star * (beta_G_star - eG * f_val)^2
    
    L_G_result = subs(L_G, full_vals)
    
    # Recursive Bellman equation for government
    return L_G_result + beta * V_G_next(f_val, m_val)
end

# Define the recursive Bellman equation for Central Bank
function cb_bellman(f_val, m_val, params, V_B_next)
    # Unpack parameter values
    gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params

    # Substitute parameter values into the best response functions
    full_vals = Dict(
        gamma => gamma_val, alpha => alpha_val, bG => bG_val, bB => bB_val, 
        dG => dG_val, dB => dB_val, eG => eG_val, eB => eB_val, 
        theta_G_star => theta_G_star_val, theta_B_star => theta_B_star_val, 
        mu_G => mu_G_val, mu_B => mu_B_val, delta => delta_val, 
        lambda_G => lambda_G_val, lambda_B => lambda_B_val,
        pi_G_star => pi_G_star_val, pi_B_star => pi_B_star_val, 
        y_G_star => y_G_star_val, y_B_star => y_B_star_val, 
        beta_G_star => beta_G_star_val, beta_B_star => beta_B_star_val
    )

    # Central Bank's loss function at time t
    L_B = mu_B * (pi_B_star - (gamma - dG * f_val - dB * m_val))^2 +(1 - mu_B) * (y_B_star - (alpha - bG * f_val - bB * m_val))^2 + theta_B_star * (beta_B_star - eB * m_val)^2

    L_B_result = subs(L_B, full_vals)

    # Recursive Bellman equation for central bank
    return L_B + beta * V_B_next(f_val, m_val)
end

# Step 4: Value function iteration
# Define initial guesses for value functions (V_G and V_B)
function initial_value_function(f, m)
    return 0.0  # Initial guess: zero value for simplicity
end

# Policy iteration to solve for optimal policies
function policy_iteration(params_gov, params_cb, max_iters=100, tol=1e-6)
    V_G = initial_value_function
    V_B = initial_value_function

    f_val, m_val = 0.0, 0.0  # Initial guesses
    
    for iter in 1:max_iters
        println("Iteration $iter")

        # Solve for optimal government policy f given central bank policy m
        res_f = optimize(f -> gov_bellman(f, m_val, params_gov, V_G), -10.0, 10.0)
        f_star = Optim.minimizer(res_f)

        # Solve for optimal central bank policy m given government policy f
        res_m = optimize(m -> cb_bellman(f_star, m, params_cb, V_B), -10.0, 10.0)
        m_star = Optim.minimizer(res_m)

        # Check for convergence (change in policies)
        if abs(f_star - f_val) < tol && abs(m_star - m_val) < tol
            println("Convergence achieved!")
            break
        end

        # Update value functions with new policies
        V_G = f -> gov_bellman(f, m_star, params_gov, V_G)
        V_B = m -> cb_bellman(f_star, m, params_cb, V_B)

        # Update policies for the next iteration
        f_val, m_val = f_star, m_star
    end

    return f_val, m_val
end

# Solve for the infinite horizon optimal policies
f_star, m_star = policy_iteration(params_gov, params_cb)
println("Infinite Horizon Solution: f* = $f_star, m* = $m_star")

# Plot policy values
bar(["Government", "Central Bank"], [f_star, m_star], title="Optimal Policies in Infinite Horizon")