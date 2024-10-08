### Attempt at writing up the game in dynamic form - it has been a while so this might be rough.
## Using Euler equation time iteration, moving from policy being 0,0 to the steady state. 
## This does not work when we set up the loss functions properly. Lets try an alternative approach. 

#using Pkg
#Pkg.add("SymPy")
#Pkg.add("NLsolve")
#Pkg.add("Optim")
#Pkg.add("Plots")
#Pkg.add("DataFrames")
#Pkg.add("StatsPlots")

using SymPy
using Optim
using Plots
using DataFrames
using StatsPlots

# Step 1: Define symbolic variables
@syms f_t f_tplus1 f_tminus1 m_t m_tplus1 m_tminus1
@syms gamma alpha bG bB dG dB eG eB theta_G_star theta_B_star
@syms mu_G mu_B delta lambda_G lambda_B pi_G_star pi_B_star y_G_star y_B_star beta_G_star beta_B_star

# Parameters
params_gov = [
    1.5,   # gamma - intercept of inflation
    2.0,   # alpha - intercept of output # Set at 1.5 for a demand shock, 2.5 for supply shock, and 2.0 to chat about policy asymmetry.
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

T_length = 15

# Step 2: Define loss functions with adjustment costs
# Version for describing the Nash Eqm where the intertemporal effect through adjustment costs are ignored
L_G_t = mu_G * (pi_G_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_G) * (y_G_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_G * (f_t - f_tminus1)^2 + theta_G_star * (beta_G_star - eG * f_t)^2

L_B_t = mu_B * (pi_B_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_B) * (y_B_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_B * (m_t - m_tminus1)^2  + theta_B_star * (beta_B_star - eB * m_t)^2

IT_L_G_t = mu_G * (pi_G_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_G) * (y_G_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_G * (f_t - f_tminus1)^2 + theta_G_star * (beta_G_star - eG * f_t)^2 + 
        delta * (mu_G * (pi_G_star - (gamma - dG * f_tplus1 - dB * m_tplus1))^2 + 
                 (1 - mu_G) * (y_G_star - (alpha - bG * f_tplus1 - bB * m_tplus1))^2 + lambda_G * (f_tplus1 - f_t)^2 + theta_G_star * (beta_G_star - eG * f_tplus1)^2)

IT_L_B_t = mu_B * (pi_B_star - (gamma - dG * f_t - dB * m_t))^2 +
        (1 - mu_B) * (y_B_star - (alpha - bG * f_t - bB * m_t))^2 +
        lambda_B * (m_t - m_tminus1)^2 + theta_B_star * (beta_B_star - eB * m_t)^2 +
        delta * (mu_B * (pi_B_star - (gamma - dG * f_tplus1 - dB * m_tplus1))^2 + 
                 (1 - mu_B) * (y_B_star - (alpha - bG * f_tplus1 - bB * m_tplus1))^2 + lambda_B * (m_tplus1 - m_t)^2 + theta_B_star * (beta_B_star - eB * m_tplus1)^2)

# Step 3: Derive the Euler equations (FOC for f_t and m_t)
Euler_G = diff(IT_L_G_t, f_t)
Euler_B = diff(IT_L_B_t, m_t)

println("Government Euler Equation: $Euler_G")
println("Central Bank Euler Equation: $Euler_B")

# Step 4: Simulate over time using Euler equations (solve for f_t and m_t at each step)
function simulate_euler(params, T; f_init=0.0, m_init=0.0)
    # Unpack parameter values for substitution
    gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params

    # Store initial values for f_tminus1 and m_tminus1
    f_vals = [f_init]
    m_vals = [m_init]

    # Iterate through time steps to simulate the evolution of f_t and m_t
    for t in 2:T
        # Substitute numerical values for f_tminus1 and m_tminus1 into the Euler equations
        full_vals = Dict(
            f_tminus1 => f_vals[end], m_tminus1 => m_vals[end],
            gamma => gamma_val, alpha => alpha_val, bG => bG_val, bB => bB_val,
            dG => dG_val, dB => dB_val, eG => eG_val, eB => eB_val,
            theta_G_star => theta_G_star_val, theta_B_star => theta_B_star_val,
            mu_G => mu_G_val, mu_B => mu_B_val, delta => delta_val,
            lambda_G => lambda_G_val, lambda_B => lambda_B_val,
            pi_G_star => pi_G_star_val, pi_B_star => pi_B_star_val,
            y_G_star => y_G_star_val, y_B_star => y_B_star_val,
            beta_G_star => beta_G_star_val, beta_B_star => beta_B_star_val
        )

        # Solve Euler equations for f_t and m_t by solving the system
        euler_f = subs(Euler_G, full_vals)
        euler_m = subs(Euler_B, full_vals)

        # Solve for f_t and m_t numerically
        sol = solve([euler_f, euler_m], [f_t, m_t])

        # Extract solutions for f_t and m_t
        f_new = float(sol[f_t])
        m_new = float(sol[m_t])

        # Append the new values to the time series
        push!(f_vals, f_new)
        push!(m_vals, m_new)
    end

    return f_vals, m_vals
end

# Calculate Output and Inflation at each time step
function calculate_output_inflation(f_vals, m_vals, params)
    alpha_val, bG_val, bB_val, gamma_val, dG_val, dB_val = params[2], params[3], params[4], params[1], params[5], params[6]
    
    y_vals = []
    pi_vals = []
    
    for (f, m) in zip(f_vals, m_vals)
        # Output (y_t)
        y_t = alpha_val - bG_val * f - bB_val * m
        # Inflation (pi_t)
        pi_t = gamma_val - dG_val * f - dB_val * m
        push!(y_vals, y_t)
        push!(pi_vals, pi_t)
    end
    
    return y_vals, pi_vals
end

# Calculate Loss for government and central bank at each time step
function calculate_losses(f_vals, m_vals, params)
    # Unpack parameters
    gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params
    
    gov_losses = []
    cb_losses = []
    
    for t in 2:length(f_vals)
        f_t, m_t = f_vals[t], m_vals[t]
        f_tminus1, m_tminus1 = f_vals[t-1], m_vals[t-1]

        # Government loss (L_G_t)
        gov_loss_t = mu_G_val * (pi_G_star_val - (gamma_val - dG_val * f_t - dB_val * m_t))^2 +
                     (1 - mu_G_val) * (y_G_star_val - (alpha_val - bG_val * f_t - bB_val * m_t))^2 +
                     lambda_G_val * (f_t - f_tminus1)^2 +
                     theta_G_star_val * (beta_G_star_val - eG_val * f_t)^2

        # Central bank loss (L_B_t)
        cb_loss_t = mu_B_val * (pi_B_star_val - (gamma_val - dG_val * f_t - dB_val * m_t))^2 +
                    (1 - mu_B_val) * (y_B_star_val - (alpha_val - bG_val * f_t - bB_val * m_t))^2 +
                    lambda_B_val * (m_t - m_tminus1)^2 +
                    theta_B_star_val * (beta_B_star_val - eB_val * m_t)^2

        push!(gov_losses, gov_loss_t)
        push!(cb_losses, cb_loss_t)
    end
    
    return gov_losses, cb_losses
end

# Step 5: Run the simulation
f_vals, m_vals = simulate_euler(params_gov, T_length; f_init=0.0, m_init=0.0)

# Step 6: Calculate output and inflation
y_vals, pi_vals = calculate_output_inflation(f_vals, m_vals, params_gov)

# Step 7: Calculate losses for government and central bank
gov_losses, cb_losses = calculate_losses(f_vals, m_vals, params_gov)

# Step 8: Plot the results
# These plots start at 0 and then transition to the steady state - so interpreting it as transition after a shock when starting from intercept as target
# Define the directory where the script is located
script_dir = @__DIR__

# Plot 1: Policies over time
plot(1:T_length, f_vals, label="Government Policy (f)", xlabel="Time", ylabel="Policy Values", title="Government and Central Bank Policies Over Time")
plot!(1:T_length, m_vals, label="Central Bank Policy (m)")
savefig(joinpath(script_dir, "policies_over_time.png"))  # Save this plot

# Plot 2: Output and Inflation over time
plot(1:T_length, y_vals, label="Output (y)", xlabel="Time", ylabel="Values", title="Output and Inflation Over Time")
plot!(1:T_length, pi_vals, label="Inflation (π)")
savefig(joinpath(script_dir, "output_inflation_over_time.png"))  # Save this plot

# Plot 3: Losses over time
plot(2:T_length, gov_losses, label="Government Loss", xlabel="Time", ylabel="Loss Value", title="Government and Central Bank Losses Over Time")
plot!(2:T_length, cb_losses, label="Central Bank Loss")
savefig(joinpath(script_dir, "losses_over_time.png"))  # Save this plot

######## Add cooperative game and comparison
## Have to adjust below to include a forward looking component

# Step 1: Define the total loss for the cooperative solution
Total_Loss = L_G_t + L_B_t

# Step 2: Simulate the cooperative solution by minimizing the total loss
function simulate_cooperative(params, T; f_init=0.0, m_init=0.0)
    f_vals = [f_init]
    m_vals = [m_init]

    # Unpack parameters
    gamma_val, alpha_val, bG_val, bB_val, dG_val, dB_val, eG_val, eB_val, theta_G_star_val, theta_B_star_val, mu_G_val, mu_B_val, delta_val, lambda_G_val, lambda_B_val, pi_G_star_val, pi_B_star_val, y_G_star_val, y_B_star_val, beta_G_star_val, beta_B_star_val = params

    for t in 2:T
        # The total loss function now takes a single vector as an argument
        function total_loss_fun(v)
            f, m = v  # Unpack the vector into f and m

            full_vals = Dict(
                f_t => f, m_t => m,  # Use f and m as the current decision variables
                f_tminus1 => f_vals[t-1], m_tminus1 => m_vals[t-1],  # Previous period values
                gamma => gamma_val, alpha => alpha_val, bG => bG_val, bB => bB_val,
                dG => dG_val, dB => dB_val, eG => eG_val, eB => eB_val,
                theta_G_star => theta_G_star_val, theta_B_star => theta_B_star_val,
                mu_G => mu_G_val, mu_B => mu_B_val, delta => delta_val,
                lambda_G => lambda_G_val, lambda_B => lambda_B_val,
                pi_G_star => pi_G_star_val, pi_B_star => pi_B_star_val,
                y_G_star => y_G_star_val, y_B_star => y_B_star_val,
                beta_G_star => beta_G_star_val, beta_B_star => beta_B_star_val
            )

            # Return the total loss after substitution
            return float(subs(Total_Loss, full_vals))  
        end

        # Minimize the total loss with respect to f and m
        # The decision variables are packed in a vector: [f, m]
        res = optimize(total_loss_fun, [f_vals[t-1], m_vals[t-1]], BFGS())  # Optimize for f and m
        f_new, m_new = Optim.minimizer(res)  # Extract the optimal f and m
        
        # Append the new values to the time series
        push!(f_vals, f_new)
        push!(m_vals, m_new)
    end

    return f_vals, m_vals
end



# Step 3: Run the simulation for cooperative solutions
f_vals_coop, m_vals_coop = simulate_cooperative(params_gov, T_length; f_init=0.0, m_init=0.0)

# Step 4: Plot the results (comparing non-cooperative and cooperative choices)
plot(1:T_length, f_vals, label="Non-Cooperative (f)", xlabel="Time", ylabel="Policy Values", title="Government and Central Bank Policies Over Time")
plot!(1:T_length, m_vals, label="Non-Cooperative (m)")
plot!(1:T_length, f_vals_coop, linestyle=:dash, label="Cooperative (f)")
plot!(1:T_length, m_vals_coop, linestyle=:dash, label="Cooperative (m)")
savefig(joinpath(script_dir, "policies_withcoop_over_time.png"))  # Save this plot

# Step 5: Plot the losses for both non-cooperative and cooperative solutions
gov_losses_coop, cb_losses_coop = calculate_losses(f_vals_coop, m_vals_coop, params_gov)

plot(2:T_length, gov_losses, label="Non-Cooperative Gov Loss", xlabel="Time", ylabel="Loss Value", title="Government and Central Bank Losses Over Time")
plot!(2:T_length, cb_losses, label="Non-Cooperative CB Loss")
plot!(2:T_length, gov_losses_coop, linestyle=:dash, label="Cooperative Gov Loss")
plot!(2:T_length, cb_losses_coop, linestyle=:dash, label="Cooperative CB Loss")
savefig(joinpath(script_dir, "output_inflation_withcoop_over_time.png"))  # Save this plot

# Step 6: Plot output and inflation for both non-cooperative and cooperative solutions
y_vals_coop, pi_vals_coop = calculate_output_inflation(f_vals_coop, m_vals_coop, params_gov)

plot(1:T_length, y_vals, label="Non-Cooperative Output", xlabel="Time", ylabel="Values", title="Output and Inflation Over Time")
plot!(1:T_length, pi_vals, label="Non-Cooperative Inflation")
plot!(1:T_length, y_vals_coop, linestyle=:dash, label="Cooperative Output")
plot!(1:T_length, pi_vals_coop, linestyle=:dash, label="Cooperative Inflation")
savefig(joinpath(script_dir, "losses_withcoop_over_time.png")) # Save plot
