# using Pkg
# Pkg.add("SymPy")
# Pkg.add("NLsolve")
# Pkg.add("Optim")
# Pkg.add("Plots")

using SymPy
using Optim
using Plots

# Step 1: Define symbolic variables for the game
@syms f1 m1 f2 m2
@syms gamma alpha bG bB dG dB eG eB theta_G_star theta_B_star
@syms mu_G mu_B delta lambda_G lambda_B
@syms pi_G_star pi_B_star y_G_star y_B_star beta_G_star beta_B_star

# Parameters for the loss functions
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

# Step 2: Define the best response functions for f2 and m2 (Stage 2)
L_G_stage2 = mu_G * (pi_G_star - (gamma - dG * f2 - dB * m2))^2 +
(1 - mu_G) * (y_G_star - (alpha - bG * f2 - bB * m2))^2 +
theta_G_star * (beta_G_star - eG * f2)^2 +
lambda_G * (f2 - f1)^2

BR_G_f2(m2, f1) = diff(L_G_stage2, f2)

L_B_stage2 = mu_B * (pi_B_star - (gamma - dG * f2 - dB * m2))^2 +
(1 - mu_B) * (y_B_star - (alpha - bG * f2 - bB * m2))^2 +
theta_B_star * (beta_B_star - eB * m2)^2 +
lambda_B * (m2 - m1)^2

BR_B_m2(f2, m1) = diff(L_B_stage2, m2)

# Step 3: Solve for f2 and m2 in terms of f1 and m1
m2_solve = solve(BR_B_m2(f2, m1), m2)[1]
f2_solve = solve(BR_G_f2(m2, f1), f2)[1]

m2_substitution = subs(m2_solve, f2 => f2_solve)
f2_substitution = subs(f2_solve, m2 => m2_solve)

m2_solution = solve(m2_substitution - m2, m2)[1]
f2_solution = solve(f2_substitution - f2, f2)[1]

# Step 4: Define the government's loss function (Stage 1)
function gov_loss(f1_val, m1_val, params)
    # Unpack parameter values for the government
    gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, 
    theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, 
    lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, 
    y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params

    # Substitute parameter values into the best response functions
    full_vals = Dict(
        f1 => f1_val, m1 => m1_val,
        gamma => gamma_val, alpha => alpha_val, bG => bG_val, bB => bB_val, 
        dG => dG_val, dB => dB_val, eG => eG_val, eB => eB_val, 
        theta_G_star => theta_G_star_val, theta_B_star => theta_B_star_val, 
        mu_G => mu_G_val, mu_B => mu_B_val, delta => delta_val, 
        lambda_G => lambda_G_val, lambda_B => lambda_B_val,
        pi_G_star => pi_G_star_val, pi_B_star => pi_B_star_val, 
        y_G_star => y_G_star_val, y_B_star => y_B_star_val, 
        beta_G_star => beta_G_star_val, beta_B_star => beta_B_star_val
    )

    # Compute m2 and f2 using the best response functions
    subs_m2 = subs(m2_solution, full_vals)
    subs_f2 = subs(f2_solution, full_vals)

    m2_val = float(subs_m2)
    f2_val = float(subs_f2)

    # Compute the government loss function
    L_G_stage1 = mu_G * (pi_G_star - (gamma - dG * f1_val - dB * m1_val))^2 +
                 (1 - mu_G) * (y_G_star - (alpha - bG * f1_val - bB * m1_val))^2 +
                 theta_G_star * (beta_G_star - eG * f1_val)^2
    
    L_G_stage2 = mu_G * (pi_G_star - (gamma - dG * f2_val - dB * m2_val))^2 +
                 (1 - mu_G) * (y_G_star - (alpha - bG * f2_val - bB * m2_val))^2 +
                 theta_G_star * (beta_G_star - eG * f2_val)^2 +
                 lambda_G * (f2_val - f1_val)^2
    
    L_G_stage1_result = subs(L_G_stage1, full_vals)
    L_G_stage2_result = subs(L_G_stage2, full_vals)

    return float(L_G_stage1_result + delta_val * L_G_stage2_result), f2_val, m2_val
end

# Step 5: Define the central bank's loss function (Stage 1)
function cb_loss(f1_val, m1_val, params)
    # Unpack parameter values for the central bank
    gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, 
    theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, 
    lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, 
    y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params

    # Substitute parameter values into the best response functions
    full_vals = Dict(
        f1 => f1_val, m1 => m1_val,
        gamma => gamma_val, alpha => alpha_val, bG => bG_val, bB => bB_val, 
        dG => dG_val, dB => dB_val, eG => eG_val, eB => eB_val, 
        theta_G_star => theta_G_star_val, theta_B_star => theta_B_star_val, 
        mu_G => mu_G_val, mu_B => mu_B_val, delta => delta_val, 
        lambda_G => lambda_G_val, lambda_B => lambda_B_val,
        pi_G_star => pi_G_star_val, pi_B_star => pi_B_star_val, 
        y_G_star => y_G_star_val, y_B_star => y_B_star_val, 
        beta_G_star => beta_G_star_val, beta_B_star => beta_B_star_val
    )

    # Compute m2 and f2 using the best response functions
    subs_m2 = subs(m2_solution, full_vals)
    subs_f2 = subs(f2_solution, full_vals)
    
    m2_val = float(subs_m2)
    f2_val = float(subs_f2)

    # Compute the central bank loss function
    L_B_stage1 = mu_B * (pi_B_star - (gamma - dG * f1_val - dB * m1_val))^2 +
                 (1 - mu_B) * (y_B_star - (alpha - bG * f1_val - bB * m1_val))^2 +
                 theta_B_star * (beta_B_star - eB * m1_val)^2
    
    L_B_stage2 = mu_B * (pi_B_star - (gamma - dG * f2_val - dB * m2_val))^2 +
                 (1 - mu_B) * (y_B_star - (alpha - bG * f2_val - bB * m2_val))^2 +
                 theta_B_star * (beta_B_star - eB * m2_val)^2 +
                 lambda_B * (m2_val - m1_val)^2
    
    L_B_stage1_result = subs(L_B_stage1, full_vals)
    L_B_stage2_result = subs(L_B_stage2, full_vals)

    return float(L_B_stage1_result + delta_val * L_B_stage2_result), f2_val, m2_val
end


# Step 6: Iterative best-response algorithm
function iterative_best_response(params_gov, params_cb; tol=1e-6, max_iters=1000)
    f1_val, m1_val = 0.0, 0.0  # Initial guesses
    f1_old, m1_old = f1_val, m1_val
    iter = 0
    converged = false

    # Initialize arrays to store losses
    gov_losses = Float64[]
    cb_losses = Float64[]

    while iter < max_iters && !converged
        iter += 1
        
        # Government minimizes loss for f1 given m1
        res_f1 = optimize(f -> gov_loss(f, m1_val, params_gov)[1], -10.0, 10.0)
        f1_val = Optim.minimizer(res_f1)
        push!(gov_losses, Optim.minimum(res_f1))

        # Central bank minimizes loss for m1 given f1
        res_m1 = optimize(m -> cb_loss(f1_val, m, params_cb)[1], -10.0, 10.0)
        m1_val = Optim.minimizer(res_m1)
        push!(cb_losses, Optim.minimum(res_m1))

        # Check for convergence
        if abs(f1_val - f1_old) < tol && abs(m1_val - m1_old) < tol
            converged = true
        end

        f1_old, m1_old = f1_val, m1_val

        # Print intermediate values and losses for convergence tracking
        println("Iteration $iter: f1 = $f1_val, m1 = $m1_val")
        println("           Gov Loss = $(gov_losses[end]), CB Loss = $(cb_losses[end])")
    end

    return f1_val, m1_val, gov_losses, cb_losses
end

# Step 7: Solve for the Nash equilibrium in stage 1 using iterative best response
f1_star, m1_star = iterative_best_response(params_gov, params_cb)

# Step 8: Compute f2 and m2 given the optimal f1 and m1
_, f2_star_gov, m2_star_gov = gov_loss(f1_star, m1_star, params_gov)
_, f2_star_cb, m2_star_cb = cb_loss(f1_star, m1_star, params_cb)

# Step 9: Print the solution
println("Nash equilibrium found:")
println("Government: f1 = $f1_star, f2 = $f2_star_gov")
println("Central Bank: m1 = $m1_star, m2 = $m2_star_cb")

# Step 10: Plot the results
policy_choices = ["Stage 1", "Stage 2"]

# Government: f1 and f2
gov_policy_values = [f1_star, f2_star_gov]

# Central Bank: m1 and m2
cb_policy_values = [m1_star, m2_star_cb]

# Plotting policy decisions over two stages
bar(policy_choices, gov_policy_values, label="Government", xlabel="Stage", ylabel="Policy Values", title="Government and Central Bank Choices", bar_width=0.3, legend=:topleft)
bar!(policy_choices, cb_policy_values, label="Central Bank", bar_width=0.3)
bar(policy_choices, cb_policy_values, label="Central Bank", xlabel="Stage", ylabel="Policy Values", title="Central Bank Choices", bar_width=0.3, legend=:topleft)

# Plot for loss comparison between Government and Central Bank
bar(["Government", "Central Bank"], [gov_losses[end], cb_losses[end]],
    title="Loss Comparison", ylabel="Loss Value", legend=false)

# Output and inflation comparison
output_choices = ["Output", "Inflation"]

gov_output_inflation = [y_G_star, pi_G_star]
cb_output_inflation = [y_B_star, pi_B_star]

bar(output_choices, gov_output_inflation, label="Government", xlabel="Economic Indicators", ylabel="Values", title="Output and Inflation Comparison", bar_width=0.3, legend=:topleft)
bar!(output_choices, cb_output_inflation, label="Central Bank", bar_width=0.3)