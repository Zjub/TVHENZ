### Attempt at writing up the game in dynamic form - it has been a while so this might be rough.
## Using Euler equation iteration

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

# Step 1: Define symbolic variables
@syms f_t f_tplus1 f_tminus1 m_t m_tplus1 m_tminus1
@syms gamma alpha bG bB dG dB eG eB theta_G_star theta_B_star
@syms mu_G mu_B delta lambda_G lambda_B pi_G_star pi_B_star y_G_star y_B_star beta_G_star beta_B_star

# Parameters
params_gov = [
    1.5,   # gamma - intercept of inflation
    2.0,   # alpha - intercept of output
    0.5,   # bG - sensitivity of output to fiscal policy
    0.5,   # bB - sensitivity of output to monetary policy
    0.5,   # dG
    0.5,   # dB
    0.5,   # eG
    0.5,   # eB
    0.5,   # theta_G_star
    0.5,   # theta_B_star
    0.0,   # mu_G
    1.0,   # mu_B
    0.9,   # delta
    0.5,   # lambda_G
    0.5,   # lambda_B
    2.0,   # pi_G_star
    2.0,   # pi_B_star
    2.0,   # y_G_star
    2.0,   # y_B_star
    0.0,   # beta_G_star
    0.0    # beta_B_star
]

params_cb = params_gov  # Same parameters for the central bank

# Step 2: Define loss functions with adjustment costs
L_G_t = mu_G * (pi_G_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_G) * (y_G_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_G * (f_t - f_tminus1)^2

L_B_t = mu_B * (pi_B_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_B) * (y_B_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_B * (m_t - m_tminus1)^2

# Step 3: Derive the Euler equations (FOC for f_t and m_t)
Euler_G = diff(L_G_t, f_t)
Euler_B = diff(L_B_t, m_t)

println("Government Euler Equation: $Euler_G")
println("Central Bank Euler Equation: $Euler_B")

# Step 4: Simulate over time using Euler equations (solve for f_t and m_t at each step)
function simulate_euler(params, T; f_init=0.0, m_init=0.0)
    # Unpack parameter values for substitution
    gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params

    # Store initial values for f_tminus1 and m_tminus1
    f_vals = [f_init]
    m_vals = [m_init]

    # Iterate through time steps to simulate the evolution of f_t and m_t
    for t in 2:T
        # Substitute numerical values for f_tminus1 and m_tminus1 into the Euler equations
        full_vals = Dict(
            f_tminus1 => f_vals[end], m_tminus1 => m_vals[end],
            gamma => gamma_val, alpha => alpha_val, bG => bG_val, bB => bB_val,
            dG => dG_val, dB => dB_val, eG => eG_val, eB => eB_val,
            theta_G_star => theta_G_star_val, theta_B_star => theta_B_star_val,
            mu_G => mu_G_val, mu_B => mu_B_val, delta => delta_val,
            lambda_G => lambda_G_val, lambda_B => lambda_B_val,
            pi_G_star => pi_G_star_val, pi_B_star => pi_B_star_val,
            y_G_star => y_G_star_val, y_B_star => y_B_star_val,
            beta_G_star => beta_G_star_val, beta_B_star => beta_B_star_val
        )

        # Solve Euler equations for f_t and m_t by solving the system
        euler_f = subs(Euler_G, full_vals)
        euler_m = subs(Euler_B, full_vals)

        # Solve for f_t and m_t numerically
        sol = solve([euler_f, euler_m], [f_t, m_t])

        # Extract solutions for f_t and m_t
        f_new = float(sol[f_t])
        m_new = float(sol[m_t])

        # Append the new values to the time series
        push!(f_vals, f_new)
        push!(m_vals, m_new)
    end

    return f_vals, m_vals
end

# Step 5: Run the simulation
f_vals, m_vals = simulate_euler(params_gov, 50; f_init=0.0, m_init=0.0)

# Step 6: Plot the results
plot(1:50, f_vals, label="Government Policy (f)", xlabel="Time", ylabel="Policy Values", title="Government and Central Bank Policies Over Time")
plot!(1:50, m_vals, label="Central Bank Policy (m)")
