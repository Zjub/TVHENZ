#using Pkg
#Pkg.add("SymPy")
#Pkg.add("NLsolve")
#Pkg.add("Optim")
#Pkg.add("Plots")
#Pkg.add("DataFrames")
#Pkg.add("StatsPlots")

using SymPy
using NLsolve
using Plots

# Step 1: Define symbolic variables
@syms f_t f_tplus1 f_tminus1 m_t m_tplus1 m_tminus1 α_f β_f α_m β_m
@syms gamma alpha bG bB dG dB eG eB theta_G_star theta_B_star
@syms mu_G mu_B delta lambda_G lambda_B pi_G_star pi_B_star y_G_star y_B_star beta_G_star beta_B_star

# Step 2: Define parameters
params_gov = [
    1.5,   # gamma - intercept of inflation
    2.5,   # alpha - intercept of output
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

# Assign parameter values
gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params_gov

# Step 3: Define Loss Functions with Conjectures
L_G_t = mu_G * (pi_G_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_G) * (y_G_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_G * (f_t - f_tminus1)^2 +
        delta * (mu_G * (pi_G_star - (gamma - dG * f_tplus1 - dB * m_tplus1))^2 +
                 (1 - mu_G) * (y_G_star - (alpha - bG * f_tplus1 - bB * m_tplus1))^2 +
                 lambda_G * (f_tplus1 - f_t)^2)

L_B_t = mu_B * (pi_B_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_B) * (y_B_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_B * (m_t - m_tminus1)^2 +
        delta * (mu_B * (pi_B_star - (gamma - dG * f_tplus1 - dB * m_tplus1))^2 +
                 (1 - mu_B) * (y_B_star - (alpha - bG * f_tplus1 - bB * m_tplus1))^2 +
                 lambda_B * (m_tplus1 - m_t)^2)

# Step 4: Derive Euler Equations (FOCs for f_t and m_t)
Euler_G = diff(L_G_t, f_t)
Euler_B = diff(L_B_t, m_t)

# Step 5: Substitute the conjectures into the Euler equations
conjecture_f = α_f + β_f * m_t
conjecture_m = α_m + β_m * f_t

Euler_G_sub = subs(Euler_G, Dict(f_tplus1 => conjecture_f, m_tplus1 => conjecture_m))
Euler_B_sub = subs(Euler_B, Dict(f_tplus1 => conjecture_f, m_tplus1 => conjecture_m))

println("Conjecture f:", conjecture_f)
println("Euler_G before substitution:", Euler_G)
println("Euler_G after substitution:", Euler_G_sub)

# Step 6: Solve for the conjecture parameters α and β while solving for f_t and m_t
function solve_conjectures_and_policies(params, tol=1e-6, max_iters=1000)
    # Initial values for f_tminus1 and m_tminus1
    f_tminus1_val = 0.0
    m_tminus1_val = 0.0

    # Initial guess for conjecture parameters
    α_f_val, β_f_val, α_m_val, β_m_val = 0.0, 0.0, 0.0, 0.0
    f_t_val, m_t_val = 0.0, 0.0  # Initial guesses for policies

    # Unpack parameter values for substitution
    gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params
    
    iter = 0
    converged = false
    
    while iter < max_iters && !converged
        iter += 1
        
        # Create a dictionary for the current values of conjectures and policies
        subst_dict = Dict(
            f_tminus1 => f_tminus1_val,
            m_tminus1 => m_tminus1_val,
            α_f => α_f_val, β_f => β_f_val, α_m => α_m_val, β_m => β_m_val,
            f_t => f_t,  # Keep f_t as a variable
            m_t => m_t,  # Keep m_t as a variable
            gamma => gamma_val, alpha => alpha_val, bG => bG_val, bB => bB_val,
            dG => dG_val, dB => dB_val, eG => eG_val, eB => eB_val,
            theta_G_star => theta_G_star_val, theta_B_star => theta_B_star_val,
            mu_G => mu_G_val, mu_B => mu_B_val, delta => delta_val,
            lambda_G => lambda_G_val, lambda_B => lambda_B_val,
            pi_G_star => pi_G_star_val, pi_B_star => pi_B_star_val,
            y_G_star => y_G_star_val, y_B_star => y_B_star_val,
            beta_G_star => beta_G_star_val, beta_B_star => beta_B_star_val
        )

        # Step 1: Solve the Euler equations for f_t and m_t given the current conjectures
        euler_g_numeric = subs(Euler_G_sub, subst_dict)
        euler_b_numeric = subs(Euler_B_sub, subst_dict)

        println("euler_b_numeric after substitution:", euler_b_numeric)

        # Solve for f_t and m_t based on the current conjecture parameters
        sol = solve([euler_g_numeric, euler_b_numeric], [f_t, m_t])

        # Extract solutions for f_t and m_t
        f_t_val_new = float(sol[f_t])
        m_t_val_new = float(sol[m_t])
        
        # Step 2: Update the conjecture parameters separately
        # Update α_f and β_f based on how f_t responds to m_t
        α_f_val_new = α_f_val + 0.1 * (f_t_val_new - α_f_val)  # Update α_f based on f_t
        β_f_val_new = β_f_val + 0.1 * (f_t_val_new / m_t_val_new - β_f_val)  # Adjust β_f based on the ratio f_t / m_t

        # Update α_m and β_m based on how m_t responds to f_t
        α_m_val_new = α_m_val + 0.1 * (m_t_val_new - α_m_val)  # Update α_m based on m_t
        β_m_val_new = β_m_val + 0.1 * (m_t_val_new / f_t_val_new - β_m_val)  # Adjust β_m based on the ratio m_t / f_t
        
        # Step 3: Check convergence of the conjectures and policies
        if abs(α_f_val_new - α_f_val) < tol && abs(β_f_val_new - β_f_val) < tol &&
           abs(α_m_val_new - α_m_val) < tol && abs(β_m_val_new - β_m_val) < tol &&
           abs(f_t_val_new - f_t_val) < tol && abs(m_t_val_new - m_t_val) < tol
            converged = true
        end
        
        # Update the values for the next iteration
        α_f_val, β_f_val = α_f_val_new, β_f_val_new
        α_m_val, β_m_val = α_m_val_new, β_m_val_new
        f_t_val, m_t_val = f_t_val_new, m_t_val_new
        
        # Update f_tminus1 and m_tminus1 for the next iteration
        f_tminus1_val, m_tminus1_val = f_t_val, m_t_val
    end
    
    if converged
        println("Converged after $iter iterations.")
    else
        println("Did not converge after $max_iters iterations.")
    end
    
    return (α_f_val, β_f_val, α_m_val, β_m_val), (f_t_val, m_t_val)
end

# Step 7: Run the consistent conjecture solution
conjecture_vals, policy_vals = solve_conjectures_and_policies(params_gov)

println("Solution for α_f: ", conjecture_vals[1])
println("Solution for β_f: ", conjecture_vals[2])
println("Solution for α_m: ", conjecture_vals[3])
println("Solution for β_m: ", conjecture_vals[4])

# Step 7: Run the consistent conjecture solution
(α_f_val, β_f_val, α_m_val, β_m_val), (f_t_val, m_t_val) = solve_conjectures_and_policies(params_gov)

println("Solution for α_f: ", α_f_val)
println("Solution for β_f: ", β_f_val)
println("Solution for α_m: ", α_m_val)
println("Solution for β_m: ", β_m_val)
println("Solution for f_t: ", f_t_val)
println("Solution for m_t: ", m_t_val)

# Step 8: Plot results (optional)
T = 15
f_vals = [f_t_val]
m_vals = [m_t_val]

for t in 2:T
    f_new = α_f_val + β_f_val * m_vals[t-1]
    m_new = α_m_val + β_m_val * f_vals[t-1]
    push!(f_vals, f_new)
    push!(m_vals, m_new)
end

# Plot policy decisions
plot(1:T, f_vals, label="Government Policy (f)", xlabel="Time", ylabel="Policy Values", title="Government and Central Bank Policies Over Time")
plot!(1:T, m_vals, label="Central Bank Policy (m)")