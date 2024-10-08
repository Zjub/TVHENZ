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

Euler_G_sub = subs(Euler_G, [f_tplus1 => conjecture_f, m_tplus1 => conjecture_m])
Euler_B_sub = subs(Euler_B, [f_tplus1 => conjecture_f, m_tplus1 => conjecture_m])

# Step 6: Solve for the conjecture parameters α and β while solving for f_t and m_t
function solve_conjectures_and_policies(params, tol=1e-6, max_iters=100)
    # Initial guess for conjecture parameters
    α_f_val, β_f_val, α_m_val, β_m_val = 0.0, 0.0, 0.0, 0.0
    f_t_val, m_t_val = 0.0, 0.0  # Initial guesses for policies
    
    iter = 0
    converged = false
    
    while iter < max_iters && !converged
        iter += 1
        
        # Create a dictionary for the current values of α_f, β_f, α_m, β_m, f_t, m_t
        subst_dict = Dict(
            α_f => α_f_val, β_f => β_f_val, α_m => α_m_val, β_m => β_m_val,
            f_t => f_t_val, m_t => m_t_val
        )
        
        # Step 1: Solve the Euler equations for f_t and m_t given the current conjectures
        euler_g_numeric = subs(Euler_G_sub, subst_dict)
        euler_b_numeric = subs(Euler_B_sub, subst_dict)
        
        # Solve for f_t and m_t based on the conjecture parameters
        sol = solve([euler_g_numeric, euler_b_numeric], [f_t, m_t])
        f_t_val_new = float(sol[f_t])
        m_t_val_new = float(sol[m_t])
        
        # Step 2: Update the conjecture parameters based on new f_t and m_t
        α_f_val_new = ...  # Update α_f based on f_t_val_new, m_t_val_new
        β_f_val_new = ...  # Update β_f based on f_t_val_new, m_t_val_new
        α_m_val_new = ...  # Update α_m based on f_t_val_new, m_t_val_new
        β_m_val_new = ...  # Update β_m based on f_t_val_new, m_t_val_new
        
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
    end
    
    if converged
        println("Converged after $iter iterations.")
    else
        println("Did not converge after $max_iters iterations.")
    end
    
    return (α_f_val, β_f_val, α_m_val, β_m_val), (f_t_val, m_t_val)
end


# Step 7: Run the consistent conjecture solution
α_f_val, β_f_val, α_m_val, β_m_val = solve_conjectures(params_gov)

println("Solution for α_f: ", α_f_val)
println("Solution for β_f: ", β_f_val)
println("Solution for α_m: ", α_m_val)
println("Solution for β_m: ", β_m_val)

# Step 8: Simulate policy and losses
T = 50
f_vals = [0.0]
m_vals = [0.0]

for t in 2:T
    f_new = α_f_val + β_f_val * m_vals[t-1]
    m_new = α_m_val + β_m_val * f_vals[t-1]
    push!(f_vals, f_new)
    push!(m_vals, m_new)
end

# Step 9: Plot the results
plot(1:T, f_vals, label="Government Policy (f)", xlabel="Time", ylabel="Policy Values", title="Government and Central Bank Policies Over Time")
plot!(1:T, m_vals, label="Central Bank Policy (m)")
