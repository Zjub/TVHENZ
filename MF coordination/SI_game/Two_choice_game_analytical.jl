using SymPy
using NLsolve

# Step 1: Define symbolic variables for the game
@syms f1 m1 f2 m2
@syms gamma alpha bG bB dG dB eG eB theta_G_star theta_B_star
@syms mu_G mu_B delta lambda_G lambda_B
@syms pi_G_star pi_B_star y_G_star y_B_star beta_G_star beta_B_star

# Step 2: Define the best response functions for f2 and m2
# Best response for f2 as a function of m2 and f1
BR_G_f2(m2, f1) = -(bG * (1 - mu_G) * (y_G_star - alpha + bB * m2) + 
                     dG * mu_G * (pi_G_star - gamma + dB * m2) - 
                     theta_G_star * eG * beta_G_star + lambda_G * f1) / 
                   (bG^2 * (1 - mu_G) + dG^2 * mu_G + theta_G_star + lambda_G)

# Best response for m2 as a function of f2 and m1
BR_B_m2(f2, m1) = -(bB * (1 - mu_B) * (y_B_star - alpha + bG * f2) + 
                     dB * mu_B * (pi_B_star - gamma + dG * f2) - 
                     theta_B_star * eB * beta_B_star + lambda_B * m1) / 
                   (bB^2 * (1 - mu_B) + dB^2 * mu_B + theta_B_star + lambda_B)

# Step 3: Solve for f2 and m2 in terms of f1 and m1
# Solve for m2 in terms of f2 and f1
m2_solve = solve(BR_B_m2(f2, m1) - m2, m2)[1]
f2_solve = solve(BR_G_f2(m2, f1) - f2, f2)[1]

m2_solution = subs(m2_solve, f2 => f2_solve)
f2_solution = subs(f2_solve, m2 => m2_solve)

# Step 4: Substitute f2 and m2 into the stage 1 loss functions

# Government loss function at time 1
L_G_total_sub = mu_G * (pi_G_star - (gamma - dG * f1 - dB * m1))^2 +
                (1 - mu_G) * (y_G_star - (alpha - bG * f1 - bB * m1))^2 +
                theta_G_star * (beta_G_star - eG * f1)^2 +
                delta * (mu_G * (pi_G_star - (gamma - dG * f2_solution - dB * m2_solution))^2 +
                         (1 - mu_G) * (y_G_star - (alpha - bG * f2_solution - bB * m2_solution))^2 +
                         theta_G_star * (beta_G_star - eG * f2_solution)^2 +
                         lambda_G * (f2_solution - f1)^2)

# Central bank loss function at time 1
L_B_total_sub = mu_B * (pi_B_star - (gamma - dG * f1 - dB * m1))^2 +
                (1 - mu_B) * (y_B_star - (alpha - bG * f1 - bB * m1))^2 +
                theta_B_star * (beta_B_star - eB * m1)^2 +
                delta * (mu_B * (pi_B_star - (gamma - dG * f2_solution - dB * m2_solution))^2 +
                         (1 - mu_B) * (y_B_star - (alpha - bG * f2_solution - bB * m2_solution))^2 +
                         theta_B_star * (beta_B_star - eB * m2_solution)^2 +
                         lambda_B * (m2_solution - m1)^2)

# Step 5: Differentiate the total loss functions to get the FOCs
FOC_G_f1 = diff(L_G_total_sub, f1)
FOC_B_m1 = diff(L_B_total_sub, m1)

# Attempt to solve for f1 and m1 directly
solution = solve([FOC_G_f1, FOC_B_m1], [f1, m1])

# After solving for f1 and m1
f1_star, m1_star = solution[f1], solution[m1]

# Substitute the values into f2 and m2
f2_star = subs(f2_solution, [f1 => f1_star, m1 => m1_star])
m2_star = subs(m2_solution, [f1 => f1_star, m1 => m1_star])

# Print the results
println("f1 = $f1_star, m1 = $m1_star")
println("f2 = $f2_star, m2 = $m2_star")
