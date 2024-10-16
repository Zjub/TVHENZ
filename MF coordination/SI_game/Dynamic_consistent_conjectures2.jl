## Set up the file to follow actual time and solve

# using Pkg
# Pkg.add("SymPy")
# Pkg.add("NLsolve")
# Pkg.add("Optim")
# Pkg.add("Plots")
# Pkg.add("DataFrames")
# Pkg.add("StatsPlots")

using SymPy, NLsolve, Optim, Plots

# Step 1: Define symbolic variables
@syms ω_f ψ_f ω_m ψ_m
@syms gamma alpha bG bB dG dB eG eB theta_G_star theta_B_star
@syms mu_G mu_B delta lambda_G lambda_B pi_G_star pi_B_star y_G_star y_B_star beta_G_star beta_B_star

n = 4  # Considering time periods 1, 2, 3, and 4 for this example
f = [symbols("f_$i") for i in 1:n]  # Fiscal policy choices at times 1, 2, 3, and 4
m = [symbols("m_$i") for i in 1:n]  # Monetary policy choices at times 1, 2, 3, and 4

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

# Step 2: Define time functions for losses and conjectures

L_G = Sym[0 for _ in 1:n]  # Loss functions for government
L_B = Sym[0 for _ in 1:n]  # Loss functions for monetary authority
conjecture_f = Sym[0 for _ in 1:n]  # Conjecture for fiscal policy f_t as a function of f_t-1 and m_t-1
conjecture_m = Sym[0 for _ in 1:n]  # Conjecture for monetary policy m_t as a function of f_t-1 and m_t-1

for t in 2:4
    L_G[t] = mu_G * (pi_G_star - (gamma - dG * f[t] - dB * m[t]))^2 +
        (1 - mu_G) * (y_G_star - (alpha - bG * f[t] - bB * m[t]))^2 +
        lambda_G * (f[t] - f[t-1])^2 + theta_G_star * (beta_G_star - eG * f[t])^2 

    L_B[t] = mu_B * (pi_B_star - (gamma - dG * f[t] - dB * m[t]))^2 +
        (1 - mu_B) * (y_B_star - (alpha - bG * f[t] - bB * m[t]))^2 +
        lambda_B * (m[t] - m[t-1])^2 + theta_B_star * (beta_B_star - eB * m[t])^2 

    # Conjecture for fiscal policy at time t based on previous period (t-1)
    conjecture_f[t] = ω_f * f[t-1] + ψ_f * m[t-1]

    # Conjecture for monetary policy at time t based on previous period (t-1)
    conjecture_m[t] = ω_m * f[t-1] + ψ_m * m[t-1]
end

Euler_G = diff(L_G[2],f[2]) + delta*(diff(conjecture_m[3],f[2])*diff(L_G[3],m[3]) + diff(L_G[4],f[3])) - (diff(conjecture_m[3],f[2])*diff(conjecture_m[4],m[3])/diff(conjecture_m[4],f[3]))*(delta*diff(L_G[3],f[3]) + delta^2*diff(L_G[4],f[3]))

Euler_B = diff(L_B[2],m[2]) + delta*(diff(conjecture_f[3],m[2])*diff(L_B[3],f[3]) + diff(L_B[4],m[3])) - (diff(conjecture_f[3],m[2])*diff(conjecture_f[4],f[3])/diff(conjecture_f[4],m[3]))*(delta*diff(L_B[3],m[3]) + delta^2*diff(L_B[4],m[3]))

Euler_G_subs = subs(Euler_G, Dict(f[4] => conjecture_f[4], m[4] => conjecture_m[4]))
Euler_G_subs = subs(Euler_G_subs, Dict(f[3] => conjecture_f[3], m[3] => conjecture_m[3]))
Euler_G_subs = subs(Euler_G_subs, Dict(f[2] => conjecture_f[2], m[2] => conjecture_m[2]))

Euler_G_solve = solve(Euler_G_subs,f[1]) # Choice of f_1 as a function of m_1 and unknown four conjecture parameters

Euler_B_subs = subs(Euler_B, Dict(f[4] => conjecture_f[4], m[4] => conjecture_m[4]))
Euler_B_subs = subs(Euler_B_subs, Dict(f[3] => conjecture_f[3], m[3] => conjecture_m[3]))
Euler_B_subs = subs(Euler_B_subs, Dict(f[2] => conjecture_f[2], m[2] => conjecture_m[2]))

Euler_B_solve = solve(Euler_B_subs,m[1]) # Choice of m_1 as a function of f_1 and unknown four conjecture parameters

## In steady state MPE the conjecture based on the values of f and m and the parameters will equal the observed value.  We can solve by starting with an initial value of the other persons choice and initial unknown parameters, and calculate the individuals choice and the conjectured values of f and m.  If the choices are not equal to the conjectured values, then we reiterate.

# Initialize the parameters
global f1_guess = 1.0
global m1_guess = 1.0
global ω_f_guess = 1.0
global ω_m_guess = 1.0
global ψ_f_guess = 1.0
global ψ_m_guess = 1.0

global tolerance = 1e-5
global max_iter = 10000
global iter_count = 0
global converged = false


# Step 3: Implement the fixed-point iteration manually
while !converged && iter_count < max_iter
    global iter_count += 1  # Ensure iter_count is incremented as a global variable
    println("Iteration: $iter_count")
    full_vals_B = Dict(
        f[1] => f1_guess, ω_f => ω_f_guess,ω_m => ω_m_guess,ψ_f => ψ_f_guess, ψ_m => ψ_m_guess,
        gamma => params_gov[1], alpha => params_gov[2], bG => params_gov[3], bB => params_gov[4], 
        dG => params_gov[5], dB => params_gov[6], eG => params_gov[7], eB => params_gov[8], 
        theta_G_star => params_gov[9], theta_B_star => params_gov[10], 
        mu_G => params_gov[11], mu_B => params_gov[12], delta => params_gov[13], 
        lambda_G => params_gov[14], lambda_B => params_gov[15],
        pi_G_star => params_gov[16], pi_B_star => params_gov[17], 
        y_G_star => params_gov[18], y_B_star => params_gov[19], 
        beta_G_star => params_gov[20], beta_B_star => params_gov[21]
    )

    full_vals_G = Dict(
        m[1] => m1_guess,ω_f => ω_f_guess,ω_m => ω_m_guess,ψ_f => ψ_f_guess,ψ_m => ψ_m_guess,
        gamma => params_gov[1], alpha => params_gov[2], bG => params_gov[3], bB => params_gov[4], 
        dG => params_gov[5], dB => params_gov[6], eG => params_gov[7], eB => params_gov[8], 
        theta_G_star => params_gov[9], theta_B_star => params_gov[10], 
        mu_G => params_gov[11], mu_B => params_gov[12], delta => params_gov[13], 
        lambda_G => params_gov[14], lambda_B => params_gov[15],
        pi_G_star => params_gov[16], pi_B_star => params_gov[17], 
        y_G_star => params_gov[18], y_B_star => params_gov[19], 
        beta_G_star => params_gov[20], beta_B_star => params_gov[21]
    )

    # Compute new values for m1 based on f1 using Euler_B_solve
    global new_m1 = subs(Euler_B_solve[1], full_vals_B)  # Substitute f1 into Euler_B_solve and evaluate numerically
    
    # Compute new values for f1 based on m1 using Euler_G_solve
    global new_f1 = (subs(Euler_G_solve[1], full_vals_G))  # Substitute m1 into Euler_G_solve and evaluate numerically

    # Now check the conjecture f1 = ω_f * f1 + ψ_f * m1
    global conjectured_f1 = ω_f_guess * f1_guess + ψ_f_guess * m1_guess
    global conjectured_m1 = ω_m_guess * f1_guess + ψ_m_guess * m1_guess

    # Adjust the conjecture parameters if f1 and m1 do not match the optimal values
    if abs(new_f1 - conjectured_f1) > tolerance
        global ω_f_guess += 0.1 * (new_f1 - conjectured_f1) / f1_guess  # Adjust ω_f
        global ψ_f_guess += 0.1 * (new_f1 - conjectured_f1) / m1_guess  # Adjust ψ_f
    end

    if abs(new_m1 - conjectured_m1) > tolerance
        global ω_m_guess += 0.1 * (new_m1 - conjectured_m1) / f1_guess  # Adjust ω_m
        global ψ_m_guess += 0.1 * (new_m1 - conjectured_m1) / m1_guess  # Adjust ψ_m
    end

    # Check for convergence (if changes are smaller than the tolerance)
    if abs(new_m1 - m1_guess) < tolerance && abs(new_f1 - f1_guess) < tolerance
        global converged = true
        println("Converged after $iter_count iterations")
    end

    # Update the guesses
    global f1_guess = new_f1
    global m1_guess = new_m1

    # Print current guesses for inspection
    println("f1: $f1_guess, m1: $m1_guess, ω_f: $ω_f_guess, ω_m: $ω_m_guess, ψ_f: $ψ_f_guess, ψ_m: $ψ_m_guess")
end

if !converged
    println("Failed to converge after $max_iter iterations")
end

# Output the final solution
println("Final f1: $f1_guess, Final m1: $m1_guess")
println("Final ω_f: $ω_f_guess, Final ω_m: $ω_m_guess")
println("Final ψ_f: $ψ_f_guess, Final ψ_m: $ψ_m_guess")
