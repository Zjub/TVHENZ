## Set up the file to follow actual time and solve
## Note issue - I haven't put a "constant" in the conjectures.  Need to do that.

#using Pkg
#Pkg.add("SymPy")
#Pkg.add("NLsolve")
#Pkg.add("Optim")
#Pkg.add("Plots")
#Pkg.add("DataFrames")
#Pkg.add("StatsPlots")
#Pkg.add("LinearAlgebra")
#Pkg.add("ForwardDiff")

using SymPy, NLsolve, Optim, Plots, LinearAlgebra, ForwardDiff


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
    0.0,   # eG
    0.0,   # eB
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

    # Conjecture for fiscal policy at time t based on previous period (t-1)
    conjecture_f[t] = ω_f * f[t-1] + ψ_f * m[t-1] + c_f

    # Conjecture for monetary policy at time t based on previous period (t-1)
    conjecture_m[t] = ω_m * f[t-1] + ψ_m * m[t-1] + c_m
end

Euler_G = diff(L_G[2],f[2]) + delta*(diff(conjecture_m[3],f[2])*diff(L_G[3],m[3]) + diff(L_G[3],f[2])) - (diff(conjecture_m[3],f[2])*diff(conjecture_m[4],m[3])/diff(conjecture_m[4],f[3]))*(delta*diff(L_G[3],f[3]) + delta^2*diff(L_G[4],f[3]))

Euler_B = diff(L_B[2],m[2]) + delta*(diff(conjecture_f[3],m[2])*diff(L_B[3],f[3]) + diff(L_B[3],m[2])) - (diff(conjecture_f[3],m[2])*diff(conjecture_f[4],f[3])/diff(conjecture_f[4],m[3]))*(delta*diff(L_B[3],m[3]) + delta^2*diff(L_B[4],m[3]))

Euler_G_subs = subs(Euler_G, Dict(f[4] => conjecture_f[4], m[4] => conjecture_m[4]))
Euler_G_subs = subs(Euler_G_subs, Dict(f[3] => conjecture_f[3], m[3] => conjecture_m[3]))
Euler_G_subs = subs(Euler_G_subs, Dict(f[2] => conjecture_f[2], m[2] => conjecture_m[2]))

Euler_B_subs = subs(Euler_B, Dict(f[4] => conjecture_f[4], m[4] => conjecture_m[4]))
Euler_B_subs = subs(Euler_B_subs, Dict(f[3] => conjecture_f[3], m[3] => conjecture_m[3]))
Euler_B_subs = subs(Euler_B_subs, Dict(f[2] => conjecture_f[2], m[2] => conjecture_m[2]))

# Substitute params into Euler equations
Euler_G_subs = subs(Euler_G_subs, param_subs)
Euler_B_subs = subs(Euler_B_subs, param_subs)

Euler_G_solve = solve(Euler_G_subs, f[1]) # Choice of f_1 as a function of m_1 and unknown four conjecture parameters
Euler_B_solve = solve(Euler_B_subs, m[1]) # Choice of m_1 as a function of f_1 and unknown four conjecture parameters

if f[1] in free_symbols(Euler_G_solve)
    error("Substitution failed to resolve dependencies. Check equations.")
end

# Define system of equations based on Euler equations and conjectures
f1_eq = Euler_G_solve[1]  # Euler equation for f1, already substituted with params_gov
m1_eq = Euler_B_solve[1]  # Euler equation for m1, already substituted with params_gov

# Substitute m1 into f1_eq to eliminate m1
f1_resolved = solve(subs(f1_eq, Dict(:m_1 => m1_eq)),f[1])

if f[1] in free_symbols(f1_resolved) || m[1] in free_symbols(f1_resolved)
    error("Substitution failed to resolve dependencies. Check equations.")
end

# Substitute f1 into m1_eq to eliminate f1
m1_resolved = solve(subs(m1_eq, Dict(:f_1 => f1_eq)),m[1])



## We have f1 and m1 as a function of four unknowns. Use this to solve forward to create three equations, and solve for parameters from there.

function residuals(x::Vector{Float64}, f1_resolved, m1_resolved)
    # Extract coefficients
    ω_f, ψ_f, ω_m, ψ_m, c_f, c_m = x[1:6]

    # Create substitution dictionary
    substitutions = Dict(:ω_f => ω_f, :ψ_f => ψ_f, :ω_m => ω_m, :ψ_m => ψ_m, :c_f => c_f, :c_m => c_m)

    if typeof(f1_resolved) <: Vector
        println("Warning: f1_resolved is a vector. Selecting the first solution.")
        f1_resolved = first(f1_resolved)
    end

    if typeof(m1_resolved) <: Vector
        println("Warning: m1_resolved is a vector. Selecting the first solution.")
        m1_resolved = first(m1_resolved)
    end

    # Check if f1_resolved and m1_resolved are vectors or single expressions
    f1 = typeof(f1_resolved) <: Vector ? subs.(f1_resolved, Ref(substitutions)) : subs(f1_resolved, substitutions)
    m1 = typeof(m1_resolved) <: Vector ? subs.(m1_resolved, Ref(substitutions)) : subs(m1_resolved, substitutions)

    # Convert final solutions to numeric
    f1 = float(f1)
    m1 = float(m1)

    # Compute realized values of f2 and m2
    f2 = ω_f * f1 + ψ_f * m1 + c_f
    m2 = ω_m * f1 + ψ_m * m1 + c_m

    # Residuals for conjecture consistency
    R = [
        f1 - (ω_f * f1 + ψ_f * m1 + c_f),
        m1 - (ω_m * f1 + ψ_m * m1 + c_m),
        f2 - (ω_f * f2 + ψ_f * m2 + c_f),
        m2 - (ω_m * f2 + ψ_m * m2 + c_m)
    ]

    return R
end

function solve_equilibrium(f1_resolved, m1_resolved)
    # Initial guesses for the coefficients
    initial_guess = [0.1, 0.1, 0.1, 0.1,0.5,0.5]  # [ω_f, ψ_f, ω_m, ψ_m,c_f,c_m]

    # Solve using nlsolve
    result = nlsolve(x -> residuals(x, f1_resolved, m1_resolved), initial_guess)

    if result.f_converged
        # Extract solutions
        ω_f, ψ_f, ω_m, ψ_m,c_f,c_m = result.zero

        # Create substitution dictionary for final values
        substitutions = Dict(:ω_f => ω_f, :ψ_f => ψ_f, :ω_m => ω_m, :ψ_m => ψ_m, :c_f => c_f, :c_m => c_m)

        if typeof(f1_resolved) <: Vector
            println("Warning: f1_resolved is a vector. Selecting the first solution.")
            f1_resolved = first(f1_resolved)
        end

        if typeof(m1_resolved) <: Vector
            println("Warning: m1_resolved is a vector. Selecting the first solution.")
            m1_resolved = first(m1_resolved)
        end

        # Substitute solutions into f1 and m1 for final values
        f1 = typeof(f1_resolved) <: Vector ? subs.(f1_resolved, Ref(substitutions)) : subs(f1_resolved, substitutions)
        m1 = typeof(m1_resolved) <: Vector ? subs.(m1_resolved, Ref(substitutions)) : subs(m1_resolved, substitutions)

        f1 = float(f1)
        m1 = float(m1)

        # Calculate inflation, output, and losses (with substitutions applied)
        inflation_G = subs(gamma - dG * f1 - dB * m1, param_subs)
        output_G = subs(alpha - bG * f1 - bB * m1, param_subs)

        inflation_B = inflation_G  # Same inflation function for both
        output_B = output_G        # Same output function for both

        loss_G = subs(
            mu_G * (pi_G_star - inflation_G)^2 +
            (1 - mu_G) * (y_G_star - output_G)^2 +
            lambda_G * (f1)^2,
            param_subs
        )
        loss_B = subs(
            mu_B * (pi_B_star - inflation_B)^2 +
            (1 - mu_B) * (y_B_star - output_B)^2 +
            lambda_B * (m1)^2,
            param_subs
        )

        # Convert losses to numeric
        inflation_G = float(inflation_G)
        output_G = float(output_G)
        loss_G = float(loss_G)

        inflation_B = float(inflation_B)
        output_B = float(output_B)
        loss_B = float(loss_B)

        println("Converged: true")
        println("Solution: ω_f = $ω_f, ψ_f = $ψ_f, ω_m = $ω_m, ψ_m = $ψ_m")
        println("Equilibrium Policies: f1 = $f1, m1 = $m1")
        println("Inflation (Gov): $inflation_G, Output (Gov): $output_G, Loss (Gov): $loss_G")
        println("Inflation (Monetary): $inflation_B, Output (Monetary): $output_B, Loss (Monetary): $loss_B")

        return ω_f, ψ_f, ω_m, ψ_m, f1, m1, inflation_G, output_G, loss_G, inflation_B, output_B, loss_B
    else
        println("Converged: false")
        error("Solver did not converge")
    end
end

# Solve for equilibrium and print results
ω_f, ψ_f, ω_m, ψ_m,c_f,c_m f1, m1, inflation_G, output_G, loss_G, inflation_B, output_B, loss_B = solve_equilibrium(f1_resolved, m1_resolved)

println("Equilibrium Coefficients: ω_f = $ω_f, ψ_f = $ψ_f, ω_m = $ω_m, ψ_m = $ψ_m")
println("Equilibrium Policies: f1 = $f1, m1 = $m1")
println("Inflation (Gov): $inflation_G, Output (Gov): $output_G, Loss (Gov): $loss_G")
println("Inflation (Monetary): $inflation_B, Output (Monetary): $output_B, Loss (Monetary): $loss_B")
