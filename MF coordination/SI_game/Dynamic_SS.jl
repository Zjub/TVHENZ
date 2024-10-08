# Attempting to solve the steady state by looking at the situation when the lagged, current, and lead values are the same. Just end up with the static NE here.

#using Pkg
#Pkg.add("SymPy")
#Pkg.add("NLsolve")
#Pkg.add("Optim")
#Pkg.add("Plots")
#Pkg.add("DataFrames")
#Pkg.add("StatsPlots")

using SymPy

# Step 1: Define symbolic variables
@syms f_t f_tplus1 f_tminus1 m_t m_tplus1 m_tminus1
@syms gamma alpha bG bB dG dB eG eB theta_G_star theta_B_star
@syms mu_G mu_B delta lambda_G lambda_B pi_G_star pi_B_star y_G_star y_B_star beta_G_star beta_B_star

# Parameters for the loss functions
params = Dict(
    gamma => 1.5,   # intercept of inflation
    alpha => 1.5,   # intercept of output
    bG => 0.5,      # sensitivity of output to fiscal policy
    bB => 0.5,      # sensitivity of output to monetary policy
    dG => 0.5,
    dB => 0.5,
    eG => 0.5,
    eB => 0.5,
    theta_G_star => 0.5,
    theta_B_star => 0.5,
    mu_G => 0.0,
    mu_B => 1.0,
    delta => 0.9,
    lambda_G => 0.5,
    lambda_B => 0.5,
    pi_G_star => 2.0,
    pi_B_star => 2.0,
    y_G_star => 2.0,
    y_B_star => 2.0,
    beta_G_star => 0.0,
    beta_B_star => 0.0
)

# Step 2: Define the intertemporal loss functions for government and central bank
L_G_t = mu_G * (pi_G_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_G) * (y_G_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_G * (f_t - f_tminus1)^2 + theta_G_star * (beta_G_star - eG * f_t)^2 +
        delta * (mu_G * (pi_G_star - (gamma - dG * f_tplus1 - dB * m_tplus1))^2 +
                 (1 - mu_G) * (y_G_star - (alpha - bG * f_tplus1 - bB * m_tplus1))^2 +
                 lambda_G * (f_tplus1 - f_t)^2 + theta_G_star * (beta_G_star - eG * f_tplus1)^2) # + m_tplus1 - m_t

L_B_t = mu_B * (pi_B_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_B) * (y_B_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_B * (m_t - m_tminus1)^2 + theta_B_star * (beta_B_star - eB * m_t)^2 +
        delta * (mu_B * (pi_B_star - (gamma - dG * f_tplus1 - dB * m_tplus1))^2 + 
                 (1 - mu_B) * (y_B_star - (alpha - bG * f_tplus1 - bB * m_tplus1))^2 + 
                 lambda_B * (m_tplus1 - m_t)^2 + theta_B_star * (beta_B_star - eB * m_tplus1)^2)

# Step 3: Differentiate to get the Euler equations (FOC for f_t and m_t)
Euler_G = diff(L_G_t, f_t)
Euler_B = diff(L_B_t, m_t)

# Step 4: Substitute parameter values into the Euler equations
Euler_G_subs = subs(Euler_G, params)
Euler_B_subs = subs(Euler_B, params)

# Step 5: Fully substitute for steady-state conditions: f_t = f_tplus1 = f_tminus1 and m_t = m_tplus1 = m_tminus1
steady_state_G = subs(Euler_G_subs, Dict(f_tplus1 => f_t, f_tminus1 => f_t))
steady_state_B = subs(Euler_B_subs, Dict(m_tplus1 => m_t, m_tminus1 => m_t))

# Simplify the steady-state equations
steady_state_G = simplify(steady_state_G)
steady_state_B = simplify(steady_state_B)

println("Simplified Steady State Government Equation: $steady_state_G")
println("Simplified Steady State Central Bank Equation: $steady_state_B")

# Step 6: Solve the system of steady-state equations simultaneously
steady_sol = solve([steady_state_G, steady_state_B], [f_t, m_t])

# Extract the steady-state solutions
steady_sol_G = steady_sol[f_t]
steady_sol_B = steady_sol[m_t]

println("Steady State Solution for Government (f_t): $steady_sol_G")
println("Steady State Solution for Central Bank (m_t): $steady_sol_B")

# Step 7: Cooperative Solution (differentiating total loss)
Total_Loss = L_G_t + L_B_t
Coop_Euler_G = diff(Total_Loss, f_t)
Coop_Euler_B = diff(Total_Loss, m_t)

# Substitute parameter values into the cooperative Euler equations
Coop_Euler_G_subs = subs(Coop_Euler_G, params)
Coop_Euler_B_subs = subs(Coop_Euler_B, params)

# Steady-state cooperative setup: f_t = f_tplus1 = f_tminus1 and m_t = m_tplus1 = m_tminus1
steady_state_coop_G = subs(Coop_Euler_G_subs, Dict(f_tplus1 => f_t, f_tminus1 => f_t))
steady_state_coop_B = subs(Coop_Euler_B_subs, Dict(m_tplus1 => m_t, m_tminus1 => m_t))

# Simplify the cooperative steady-state equations
steady_state_coop_G = simplify(steady_state_coop_G)
steady_state_coop_B = simplify(steady_state_coop_B)

# Solve for the cooperative steady-state values of f and m
coop_sol = solve([steady_state_coop_G, steady_state_coop_B], [f_t, m_t])

coop_sol_G = coop_sol[f_t]
coop_sol_B = coop_sol[m_t]

println("Cooperative Steady State Solution for Government (f_t): $coop_sol_G")
println("Cooperative Steady State Solution for Central Bank (m_t): $coop_sol_B")

# Step 8: Compare the Steady States
println("Comparison of Steady States:")
println("Nash (Government f_t): $steady_sol_G")
println("Nash (Central Bank m_t): $steady_sol_B")
println("Cooperative (Government f_t): $coop_sol_G")
println("Cooperative (Central Bank m_t): $coop_sol_B")
