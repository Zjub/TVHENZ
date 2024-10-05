#using Pkg
#Pkg.add("SymPy")
#Pkg.add("NLsolve")

using SymPy
using NLsolve

# Helper function to check if the expression is still symbolic
is_symbolic(x) = x isa SymPy.Sym


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
m2_solve = solve(BR_B_m2(f2, m1) - m2, m2)[1]
f2_solve = solve(BR_G_f2(m2, f1) - f2, f2)[1]

m2_substitution = subs(m2_solve, f2 => f2_solve)
f2_substitution = subs(f2_solve, m2 => m2_solve)

m2_solution = solve(m2_substitution - m2, m2)[1]
f2_solution = solve(f2_substitution - f2, f2)[1]

#println("Resolved m2_solution: ", m2_solution)
#println("Resolved f2_solution: ", f2_solution)

# Step 4: Define the total loss functions for government and central bank
function gov_loss(f1_val, m1_val, params)
  # Unpack parameter values
  gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, 
  theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, 
  lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, 
  y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params

  # Substitute parameter values into the best response functions, including current values of f1 and m1
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
  
  # Compute m2 and f2 using the best response functions and substitute in the parameter values
  
  # Inspect unresolved variables before float()
  subs_m2 = subs(m2_solution, full_vals)
  #println("m2 value",subs_m2)
  #println("Free symbols in m2_solution after substitution: ", free_symbols(subs_m2))

  #m2_temp = m2_solution
  #for (symbol, value) in full_vals
  #    m2_temp = subs(m2_temp, symbol => value)
  #    println("Substituted $symbol => $value, resulting m2_temp: ", m2_temp)
  #end

  subs_f2 = subs(f2_solution, full_vals)
  #println("f2 value",subs_f2)

  m2_val = float(subs_m2)
  #println("m2 value: ", m2_val)

  f2_val = float(subs_f2)
  
  # Compute the first-stage and second-stage losses
  L_G_stage1 = mu_G * (pi_G_star - (gamma - dG * f1_val - dB * m1_val))^2 +
               (1 - mu_G) * (y_G_star - (alpha - bG * f1_val - bB * m1_val))^2 +
               theta_G_star * (beta_G_star - eG * f1_val)^2

  L_G_stage1_result = subs(L_G_stage1, full_vals)
  #free_symbols(L_G_stage1_result)
  #print("Stage 1 government loss",L_G_stage1_result)
  
  L_G_stage2 = mu_G * (pi_G_star - (gamma - dG * f2_val - dB * m2_val))^2 +
               (1 - mu_G) * (y_G_star - (alpha - bG * f2_val - bB * m2_val))^2 +
               theta_G_star * (beta_G_star - eG * f2_val)^2 +
               lambda_G * (f2_val - f1_val)^2

  L_G_stage2_result = subs(L_G_stage2, full_vals)
  #print("Stage 2 government loss",L_G_stage2_result)
  
  # Return the total discounted loss
  return float(L_G_stage1_result + delta_val*L_G_stage2_result)
end

# Step 4: Define the total loss functions for the central bank
function cb_loss(f1_val, m1_val, params)
  # Unpack parameter values
  gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, 
  theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, 
  lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, 
  y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params

  # Substitute parameter values into the best response functions, including current values of f1 and m1
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
  
  # Compute m2 and f2 using the best response functions and substitute in the parameter values
  
  # Inspect unresolved variables before float()
  subs_m2 = subs(m2_solution, full_vals)
  #println("m2 value", subs_m2)

  subs_f2 = subs(f2_solution, full_vals)
  #println("f2 value", subs_f2)

  m2_val = float(subs_m2)
  f2_val = float(subs_f2)
  
  # Compute the first-stage and second-stage losses for the central bank
  L_B_stage1 = mu_B * (pi_B_star - (gamma - dG * f1_val - dB * m1_val))^2 +
               (1 - mu_B) * (y_B_star - (alpha - bG * f1_val - bB * m1_val))^2 +
               theta_B_star * (beta_B_star - eB * m1_val)^2

  L_B_stage1_result = subs(L_B_stage1, full_vals)
  #print("Stage 1 central bank loss", L_B_stage1_result)
  
  L_B_stage2 = mu_B * (pi_B_star - (gamma - dG * f2_val - dB * m2_val))^2 +
               (1 - mu_B) * (y_B_star - (alpha - bG * f2_val - bB * m2_val))^2 +
               theta_B_star * (beta_B_star - eB * m2_val)^2 +
               lambda_B * (m2_val - m1_val)^2

  L_B_stage2_result = subs(L_B_stage2, full_vals)
  #print("Stage 2 central bank loss", L_B_stage2_result)
  
  # Return the total discounted loss
  return float(L_B_stage1_result + delta_val * L_B_stage2_result)
end

# Step 5: Set up a grid for f1 and m1
f1_grid = range(-10, 10, length=100)
m1_grid = range(-10, 10, length=100)

# Parameters for the loss functions
params_gov = [
  1.5,   # gamma
  2.0,   # alpha
  0.5,   # bG
  0.5,   # bB
  0.5,   # dG
  0.5,   # dB
  0.5,   # eG
  0.5,   # eB
  0.5,   # theta_G_star
  0.5,   # theta_B_star
  0.5,   # mu_G
  0.5,   # mu_B
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

params_cb = [
  1.5,   # gamma
  2.0,   # alpha
  0.5,   # bG
  0.5,   # bB
  0.5,   # dG
  0.5,   # dB
  0.5,   # eG
  0.5,   # eB
  0.5,   # theta_G_star
  0.5,   # theta_B_star
  0.5,   # mu_G
  0.5,   # mu_B
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

# Step 6: Evaluate the loss functions on the grid
loss_gov_grid = zeros(100, 100)
loss_cb_grid = zeros(100, 100)

for i in 1:100
    for j in 1:100
        f1_val = f1_grid[i]
        m1_val = m1_grid[j]
        loss_gov_grid[i, j] = gov_loss(f1_val, m1_val, params_gov)
        loss_cb_grid[i, j] = cb_loss(f1_val, m1_val, params_cb)
    end
end

# Step 7: Find the grid points that minimize the losses
min_loss_gov_idx = argmin(loss_gov_grid)
min_loss_cb_idx = argmin(loss_cb_grid)

f1_star_gov = f1_grid[min_loss_gov_idx[1]]
m1_star_gov = m1_grid[min_loss_gov_idx[2]]

f1_star_cb = f1_grid[min_loss_cb_idx[1]]
m1_star_cb = m1_grid[min_loss_cb_idx[2]]

# Step 8: Solve for f2 and m2 given the optimal f1 and m1
f2_star_gov = float(subs(f2_solution, [f1 => f1_star_gov, m1 => m1_star_gov]))
m2_star_gov = float(subs(m2_solution, [f1 => f1_star_gov, m1 => m1_star_gov]))

f2_star_cb = float(subs(f2_solution, [f1 => f1_star_cb, m1 => m1_star_cb]))
m2_star_cb = float(subs(m2_solution, [f1 => f1_star_cb, m1 => m1_star_cb]))

# Step 9: Print the results
println("Government solution:")
println("f1 = $f1_star_gov, m1 = $m1_star_gov")
println("f2 = $f2_star_gov, m2 = $m2_star_gov")

println("Central Bank solution:")
println("f1 = $f1_star_cb, m1 = $m1_star_cb")
println("f2 = $f2_star_cb, m2 = $m2_star_cb")

