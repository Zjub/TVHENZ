### This is not right because we are dropping the "time" element. [Also feel a bit uncomfortable having symmetric conditions so the two conjectures - have to think about if this provides collaborative outcome.]


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
@syms f_t f_tplus1 f_tminus1 m_t m_tplus1 m_tminus1 ω_f ψ_f ω_m ψ_m
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

# Step 3: Define Loss Functions
L_G_t = mu_G * (pi_G_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_G) * (y_G_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_G * (f_t - f_tminus1)^2 

L_B_t = mu_B * (pi_B_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_B) * (y_B_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_B * (m_t - m_tminus1)^2 

#static_loss_G = 

#static_loss_B = 

# Step 4: Substitute the conjectures into the Euler equations
conjecture_f = ω_f * f_t + ψ_f * m_t
conjecture_m = ω_m * f_t + ψ_m * m_t

# Step 5: Derive Euler Equations [have dropped time subscripts off this]
Euler_G = diff(L_G_t, f_t) + delta*(diff(conjecture_m,f_t)*diff(L_G_t, m_t) + diff(L_G_t,f_tminus1)) + ((diff(conjecture_m,f_t)*diff(conjecture_m,m_t))/diff(conjecture_m,f_t))*(delta*diff(L_G_t, f_t)+delta^2*diff(L_G_t, f_tminus1))

Euler_B = diff(L_B_t, m_t) + delta*(diff(conjecture_f,m_t)*diff(L_B_t, f_t) + diff(L_B_t,m_tminus1)) + ((diff(conjecture_f,m_t)*diff(conjecture_f,f_t))/diff(conjecture_f,m_t))*(delta*diff(L_B_t, m_t)+delta^2*diff(L_B_t, m_tminus1))


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
    ω_f_val, ψ_f_val, ω_m_val, ψ_m_val = 0.0, 0.0, 0.0, 0.0
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
            ω_f => ω_f_val, ψ_f => ψ_f_val, ω_m => ω_m_val, ψ_m => ψ_m_val,
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
        ω_f_val_new = ω_f_val + 0.1 * (f_t_val_new - ω_f_val)
        ψ_f_val_new = ψ_f_val + 0.1 * (m_t_val_new - ψ_f_val)
        ω_m_val_new = ω_m_val + 0.1 * (f_t_val_new - ω_m_val)
        ψ_m_val_new = ψ_m_val + 0.1 * (m_t_val_new - ψ_m_val)
        
        # Step 3: Check convergence of the conjectures and policies
        if abs(ω_f_val_new - ω_f_val) < tol && abs(ψ_f_val_new - ψ_f_val) < tol &&
            abs(ω_m_val_new - ω_m_val) < tol && abs(ψ_m_val_new - ψ_m_val) < tol &&
            abs(f_t_val_new - f_t_val) < tol && abs(m_t_val_new - m_t_val) < tol
             converged = true
         end
         
         # Update the values for the next iteration
         ω_f_val, ψ_f_val = ω_f_val_new, ψ_f_val_new
         ω_m_val, ψ_m_val = ω_m_val_new, ψ_m_val_new
         f_t_val, m_t_val = f_t_val_new, m_t_val_new
     end
    
    if converged
        println("Converged after $iter iterations.")
    else
        println("Did not converge after $max_iters iterations.")
    end
    
    return (ω_f_val, ψ_f_val, ω_m_val, ψ_m_val), (f_t_val, m_t_val)
end

# Step 7: Run the consistent conjecture solution
conjecture_vals, policy_vals = solve_conjectures_and_policies(params_gov)

println("Solution for α_f: ", conjecture_vals[1])
println("Solution for β_f: ", conjecture_vals[2])
println("Solution for α_m: ", conjecture_vals[3])
println("Solution for β_m: ", conjecture_vals[4])

# Step 7: Run the consistent conjecture solution
(ω_f_val, ψ_f_val, ω_m_val, ψ_m_val), (f_t_val, m_t_val) = solve_conjectures_and_policies(params_gov)

println("Solution for α_f: ", ω_f_val)
println("Solution for β_f: ", ψ_f_val)
println("Solution for α_m: ", ω_m_val)
println("Solution for β_m: ", ψ_m_val)
println("Solution for f_t: ", f_t_val)
println("Solution for m_t: ", m_t_val)

# Step 8: Plot results (optional)
T = 15
f_vals = [f_t_val]
m_vals = [m_t_val]

for t in 2:T
    f_new = ω_f_val* f_vals[t-1] + ψ_f_val * m_vals[t-1]
    m_new = ω_m_val* f_vals[t-1] + ψ_m_val * m_vals[t-1]
    push!(f_vals, f_new)
    push!(m_vals, m_new)

    # Compute losses at each time step
    L_G_val = subs(L_G_t, Dict(f_t => f_new, m_t => m_new, f_tminus1 => f_vals[t-1], m_tminus1 => m_vals[t-1],gamma => gamma_val, alpha => alpha_val, bG => bG_val, bB => bB_val,
    dG => dG_val, dB => dB_val, eG => eG_val, eB => eB_val,
    theta_G_star => theta_G_star_val, theta_B_star => theta_B_star_val,
    mu_G => mu_G_val, mu_B => mu_B_val, delta => delta_val,
    lambda_G => lambda_G_val, lambda_B => lambda_B_val,
    pi_G_star => pi_G_star_val, pi_B_star => pi_B_star_val,
    y_G_star => y_G_star_val, y_B_star => y_B_star_val,
    beta_G_star => beta_G_star_val, beta_B_star => beta_B_star_val))
    
    L_B_val = subs(L_B_t, Dict(f_t => f_new, m_t => m_new, f_tminus1 => f_vals[t-1], m_tminus1 => m_vals[t-1],gamma => gamma_val, alpha => alpha_val, bG => bG_val, bB => bB_val,
    dG => dG_val, dB => dB_val, eG => eG_val, eB => eB_val,
    theta_G_star => theta_G_star_val, theta_B_star => theta_B_star_val,
    mu_G => mu_G_val, mu_B => mu_B_val, delta => delta_val,
    lambda_G => lambda_G_val, lambda_B => lambda_B_val,
    pi_G_star => pi_G_star_val, pi_B_star => pi_B_star_val,
    y_G_star => y_G_star_val, y_B_star => y_B_star_val,
    beta_G_star => beta_G_star_val, beta_B_star => beta_B_star_val))

    print(L_G_val)

    push!(loss_gov_vals, float(L_G_val))
    push!(loss_cb_vals, float(L_B_val))
end

# Plot policy decisions
plot(1:T, f_vals, label="Government Policy (f)", xlabel="Time", ylabel="Policy Values", title="Government and Central Bank Policies Over Time")
plot!(1:T, m_vals, label="Central Bank Policy (m)")

# Plot losses
plot(1:T, loss_gov_vals, label="Government Loss", xlabel="Time", ylabel="Loss Values", title="Government and Central Bank Losses Over Time")
plot!(1:T, loss_cb_vals, label="Central Bank Loss")