using SymPy, Optim, LinearAlgebra

# Step 1: Define symbolic variables
@syms ω_f ψ_f c_f ω_m ψ_m c_m
@syms gamma alpha bG bB dG dB eG eB theta_G_star theta_B_star
@syms mu_G mu_B delta lambda_G lambda_B pi_G_star pi_B_star y_G_star y_B_star beta_G_star beta_B_star

n = 100  # Considering time periods for simulatoin/plots
f = [symbols("f_$i") for i in 1:n]  # Fiscal policy choices at times 1, 2, 3, and 4
m = [symbols("m_$i") for i in 1:n]  # Monetary policy choices at times 1, 2, 3, and 4

# Parameters
params_gov = [
    1.5,   # gamma - intercept of inflation
    1.5,   # alpha - intercept of output
    0.5,   # bG - sensitivity of output to fiscal policy
    0.5,   # bB - sensitivity of output to monetary policy
    0.5,   # dG
    0.5,   # dB
    0.5,   # eG
    0.5,   # eB
    0.5,   # theta_G_star - cost for differing from zero
    0.5,   # theta_B_star - cost for differing from zero
    0.0,   # mu_G
    1.0,   # mu_B
    0.9,   # delta
    0.0,   # lambda_G - adjustment cost parameter
    0.0,   # lambda_B - adjustment cost parameter
    2.0,   # pi_G_star
    2.0,   # pi_B_star
    2.0,   # y_G_star
    2.0,   # y_B_star
    0.0,   # beta_G_star
    0.0    # beta_B_star
]

param_subs = Dict(
    gamma => params_gov[1], alpha => params_gov[2], bG => params_gov[3], bB => params_gov[4], 
    dG => params_gov[5], dB => params_gov[6], eG => params_gov[7], eB => params_gov[8], 
    theta_G_star => params_gov[9], theta_B_star => params_gov[10], 
    mu_G => params_gov[11], mu_B => params_gov[12], delta => params_gov[13], 
    lambda_G => params_gov[14], lambda_B => params_gov[15], 
    pi_G_star => params_gov[16], pi_B_star => params_gov[17], 
    y_G_star => params_gov[18], y_B_star => params_gov[19], 
    beta_G_star => params_gov[20], beta_B_star => params_gov[21]
)

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

    # Conjecture for fiscal policy at time t based on previous period (t-1), with constant
    conjecture_f[t] = ω_f * f[t-1] + ψ_f * m[t-1] + c_f

    # Conjecture for monetary policy at time t based on previous period (t-1), with constant
    conjecture_m[t] = ω_m * f[t-1] + ψ_m * m[t-1] + c_m
end

Euler_G = diff(L_G[2], f[2]) + delta * (diff(conjecture_m[3], f[2]) * diff(L_G[3], m[3]) + diff(L_G[3], f[2])) - (diff(conjecture_m[3], f[2]) * diff(conjecture_m[4], m[3]) / diff(conjecture_m[4], f[3])) * (delta * diff(L_G[3], f[3]) + delta^2 * diff(L_G[4], f[3]))

Euler_B = diff(L_B[2], m[2]) + delta * (diff(conjecture_f[3], m[2]) * diff(L_B[3], f[3]) + diff(L_B[3], m[2])) - (diff(conjecture_f[3], m[2]) * diff(conjecture_f[4], f[3]) / diff(conjecture_f[4], m[3])) * (delta * diff(L_B[3], m[3]) + delta^2 * diff(L_B[4], m[3]))

Euler_G_subs = subs(Euler_G, Dict(f[4] => conjecture_f[4], m[4] => conjecture_m[4]))
Euler_G_subs = subs(Euler_G_subs, Dict(f[3] => conjecture_f[3], m[3] => conjecture_m[3]))
Euler_G_subs = subs(Euler_G_subs, Dict(f[2] => conjecture_f[2], m[2] => conjecture_m[2]))

Euler_B_subs = subs(Euler_B, Dict(f[4] => conjecture_f[4], m[4] => conjecture_m[4]))
Euler_B_subs = subs(Euler_B_subs, Dict(f[3] => conjecture_f[3], m[3] => conjecture_m[3]))
Euler_B_subs = subs(Euler_B_subs, Dict(f[2] => conjecture_f[2], m[2] => conjecture_m[2]))

# Substitute params into Euler equations
Euler_G_subs = subs(Euler_G_subs, param_subs)
Euler_B_subs = subs(Euler_B_subs, param_subs)

# Generate 6 equations by setting values for f[1] and m[1]
cases = [
    Dict(f[1] => 0, m[1] => 0),
    Dict(f[1] => 1, m[1] => 0),
    Dict(f[1] => 0, m[1] => 1)
]

equations = []
for case in cases
    push!(equations, subs(Euler_G_subs, case))
    push!(equations, subs(Euler_B_subs, case))
end

# Solve for [ω_f, ψ_f, c_f, ω_m, ψ_m, c_m]
function residuals(x)
    ω_f_val, ψ_f_val, c_f_val, ω_m_val, ψ_m_val, c_m_val = x
    subs_values = Dict(ω_f => ω_f_val, ψ_f => ψ_f_val, c_f => c_f_val, ω_m => ω_m_val, ψ_m => ψ_m_val, c_m => c_m_val)
    return [Float64(subs(eq, subs_values)) for eq in equations]
end

# Objective function for least squares
function objective(x)
    sum(residual^2 for residual in residuals(x))  # Minimize the sum of squared residuals
end

# Initial guesses for the unknowns
initial_guess = [0.1, 0.1, 0.1, 0.1, 0.1, 0.1]

# Solve using Optim for least squares
result = optimize(objective, initial_guess, BFGS())

# Extract the solution
solution = Optim.minimizer(result)

# Output the solution
println("Solution for [ω_f, ψ_f, c_f, ω_m, ψ_m, c_m]: ", solution)

# Step 5: Compute inflation, output, and loss
# Recompute conjectures using the solved coefficients
f_values = [0.0 for _ in 1:n]
m_values = [0.0 for _ in 1:n]

for t in 2:n
    f_values[t] = solution[1] * f_values[t-1] + solution[2] * m_values[t-1] + solution[3]
    m_values[t] = solution[4] * f_values[t-1] + solution[5] * m_values[t-1] + solution[6]
end

# Define placeholders for inflation, output, and loss
inflation = [0.0 for _ in 1:n]
output = [0.0 for _ in 1:n]
loss_G = [0.0 for _ in 1:n]
loss_B = [0.0 for _ in 1:n]

# Compute inflation, output, and loss for each period
for t in 2:n
    inflation[t] = params_gov[1] - params_gov[5] * f_values[t] - params_gov[6] * m_values[t]
    output[t] = params_gov[2] - params_gov[3] * f_values[t] - params_gov[4] * m_values[t]
    loss_G[t] = (params_gov[16] - inflation[t])^2 + (params_gov[18] - output[t])^2
    loss_B[t] = (params_gov[17] - inflation[t])^2 + (params_gov[19] - output[t])^2
end

# Print results
println("Period\tf\tm\tInflation\tOutput\tLoss_G\tLoss_B")
for t in 2:n
    println("$t\t$(f_values[t])\t$(m_values[t])\t$(inflation[t])\t$(output[t])\t$(loss_G[t])\t$(loss_B[t])")
end
