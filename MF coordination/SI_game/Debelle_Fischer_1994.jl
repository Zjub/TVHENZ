# Load necessary libraries
using Optim
using Plots

# Define the model parameters
struct ModelParams
    β::Float64   # Central bank's weight on output
    θ_x::Float64 # Fiscal authority's weight on output
    θ_g::Float64 # Fiscal authority's weight on government spending
    x_star::Float64 # Target level of output
    g_star::Float64 # Target level of government spending
    a::Float64   # Productivity shock
    w_star::Float64 # Target real wage
    α::Float64   # Output sensitivity to inflation
end

# Define the social planner's loss function
function social_loss_function(π, x, g, params::ModelParams)
    # Loss function combines inflation, output, and government spending deviations
    L = 0.5 * ((π .^ 2) * params.β .+ (x .- params.x_star) .^ 2 .+ (g .- params.g_star) .^ 2 .* params.θ_g)
    return L
end

# Define output as a function of inflation and fiscal policy
function output(π, τ, a, params::ModelParams)
    return params.α * (π .- τ .- a .- params.w_star)
end

# Define the fiscal authority's loss function
function fiscal_loss_function(π, x, g, params::ModelParams)
    L_fiscal = 0.5 * (params.θ_x * (x - params.x_star)^2 + (g - params.g_star)^2)
    return L_fiscal
end

# Central bank reaction function (Nash or Stackelberg follower)
function central_bank_reaction(τ, a, params::ModelParams)
    C = params.g_star + params.w_star - log(params.α) + params.x_star
    return (C .+ τ .+ a) / (1 .+ params.β)
end

# Fiscal authority reaction function (Nash or Stackelberg follower)
function fiscal_reaction(π, a, params::ModelParams)
    C = params.g_star + params.w_star - log(params.α) + params.x_star
    return (C .- π .- a) ./ (1 .+ params.θ_x)  # Use .- for element-wise subtraction
end

# Solve for the Nash Equilibrium by alternating best responses
function solve_nash_equilibrium(params::ModelParams)
    π_guess = 0.0  # Initial guess for inflation
    τ_guess = 0.0  # Initial guess for fiscal policy
    tol = 1e-6     # Tolerance for convergence
    max_iter = 100 # Maximum number of iterations
    iter = 0
    diff = 1.0     # Difference between current and previous guesses

        # Function to solve for the best response of the central bank, given τ
        function best_response_central_bank(τ_fixed)
            function central_bank_objective(π_vec)
                π = π_vec[1]  # Extract scalar from 1-element vector
                x = output(π, τ_fixed, params.a, params)
                # Central bank's loss function, typically involving inflation and output
                central_bank_loss = 0.5 * ((π)^2 * params.β + (x - params.x_star)^2)
                return central_bank_loss
            end
            result = optimize(central_bank_objective, [π_guess])  # Note, [π_guess] is a vector
            return result.minimizer[1]  # Return scalar π
        end

        # Function to solve for the best response of the fiscal authority, given π
        function best_response_fiscal_authority(π_fixed)
            function fiscal_authority_objective(τ_vec)
                τ = τ_vec[1]  # Extract scalar from 1-element vector
                x = output(π_fixed, τ, params.a, params)
                # Fiscal authority's loss function, typically involving government spending and output
                fiscal_loss = 0.5 * (params.θ_x * (x - params.x_star)^2 + (τ - params.g_star)^2 * params.θ_g)
                return fiscal_loss
            end
            result = optimize(fiscal_authority_objective, [τ_guess])  # Note, [τ_guess] is a vector
            return result.minimizer[1]  # Return scalar τ
        end

    # Alternating best responses until convergence
    while diff > tol && iter < max_iter
        iter += 1
        π_new = best_response_central_bank(τ_guess)
        τ_new = best_response_fiscal_authority(π_new)
        
        # Check convergence
        diff = max(abs(π_new - π_guess), abs(τ_new - τ_guess))
        π_guess, τ_guess = π_new, τ_new
    end

    if iter >= max_iter
        println("Nash equilibrium did not converge after $max_iter iterations.")
    else
        println("Nash equilibrium converged after $iter iterations.")
    end
    
    return π_guess, τ_guess
end

# Solve for the commitment solution (where central bank precommits) - not this formula is only distinct from Stackelberg if we repeated the game as the policy rule was fixed from here.
function solve_commitment_solution(params::ModelParams)
    initial_guess = [0.0]  # Ensure it's a scalar optimization

    # Objective function for commitment solution
    function commitment_objective(π_vec)
        π = π_vec[1]  # Extract scalar from 1-element vector
        τ = fiscal_reaction(π, params.a, params)  # Pass scalar π
        x = output(π, τ, params.a, params)
        g = fiscal_reaction(π, params.a, params)
        return social_loss_function(π, x, g, params)  # Use scalar π
    end
    
    result = optimize(commitment_objective, initial_guess)
    return result.minimizer[1]  # Return the scalar result
end

# Solve for the Stackelberg equilibrium where the central bank leads
function solve_monetary_leader(params::ModelParams)
    initial_guess = [0.0]  # Optim expects a vector, so we start with a vector of length 1

    # Objective function for Stackelberg equilibrium (monetary leader)
    function stackelberg_objective(π_vec)
        π = π_vec[1]  # Extract scalar from 1-element vector
        τ = fiscal_reaction(π, params.a, params)  # Pass scalar π
        x = output(π, τ, params.a, params)
        g = fiscal_reaction(π, params.a, params)
        return social_loss_function(π, x, g, params)  # Use scalar π
    end
    
    result = optimize(stackelberg_objective, initial_guess)
    π_opt = result.minimizer[1]  # Extract scalar from result
    τ_opt = fiscal_reaction(π_opt, params.a, params)
    return π_opt, τ_opt
end

# Solve for the cooperative solution (joint welfare maximization)
function solve_cooperative_solution(params::ModelParams)
    initial_guess = [0.0, 0.0]  # Initial guess for π and τ

    # Objective function for cooperative solution (social planner problem)
    function cooperative_objective(vars_vec)
        π = vars_vec[1]  # Extract π from the vector
        τ = vars_vec[2]  # Extract τ from the vector
        x = output(π, τ, params.a, params)
        g = fiscal_reaction(π, params.a, params)  # Fiscal policy reaction is redundant in cooperative
        return social_loss_function(π, x, g, params)  # Minimize the social loss
    end

    result = optimize(cooperative_objective, initial_guess)
    π_opt = result.minimizer[1]  # Optimized inflation
    τ_opt = result.minimizer[2]  # Optimized fiscal policy
    return π_opt, τ_opt
end

# Compare all games, including the cooperative case
function compare_games(params::ModelParams)
    println("Solving for Nash Equilibrium:")
    π_nash, τ_nash = solve_nash_equilibrium(params)
    println("Nash equilibrium: π = $π_nash, τ = $τ_nash")

    println("\nSolving for Commitment/Monetary Leadership Solution:")
    π_commit = solve_commitment_solution(params)
    τ_commit = fiscal_reaction(π_commit, params.a, params)
    println("Commitment/Monetary leadership: π = $π_commit, τ = $τ_commit")

    println("\nSolving for Cooperative Solution (Joint Welfare Maximization):")
    π_coop, τ_coop = solve_cooperative_solution(params)
    println("Cooperative solution: π = $π_coop, τ = $τ_coop")

    # Calculate output (x) and government spending (g) for each case
    x_nash = output(π_nash, τ_nash, params.a, params)
    g_nash = fiscal_reaction(π_nash, params.a, params)
    loss_nash = social_loss_function(π_nash, x_nash, g_nash, params)
    
    x_commit = output(π_commit, τ_commit, params.a, params)
    g_commit = fiscal_reaction(π_commit, params.a, params)
    loss_commit = social_loss_function(π_commit, x_commit, g_commit, params)
    
    x_coop = output(π_coop, τ_coop, params.a, params)
    g_coop = fiscal_reaction(π_coop, params.a, params)
    loss_coop = social_loss_function(π_coop, x_coop, g_coop, params)

    println("\nLoss under Nash Equilibrium: $loss_nash")
    println("Loss under Commitment/Monetary Leadership: $loss_commit")
    println("Loss under Cooperative Solution: $loss_coop")

    # Collect results for plotting
    scenarios = ["Nash", "Commitment/Leader", "Cooperative"]
    π_values = [π_nash, π_commit, π_coop]
    τ_values = [τ_nash, τ_commit, τ_coop]
    loss_values = [loss_nash, loss_commit, loss_coop]
    x_values = [x_nash, x_commit, x_coop]
    g_values = [g_nash, g_commit, g_coop]

    return scenarios, π_values, τ_values, loss_values, x_values, g_values
end

# Function to plot the results
function plot_results(scenarios, π_values, τ_values, loss_values, x_values, g_values)
    # Plot inflation comparison
    p1 = bar(scenarios, π_values, title="Inflation Comparison (π)", ylabel="Inflation (π)", legend=false)

    # Plot fiscal policy comparison (τ)
    p2 = bar(scenarios, τ_values, title="Fiscal Policy Comparison (τ)", ylabel="Fiscal Policy (τ)", legend=false)

    # Plot loss comparison
    p3 = bar(scenarios, loss_values, title="Social Loss Comparison", ylabel="Loss", legend=false)

    # Plot output comparison
    p4 = bar(scenarios, x_values, title="Output Comparison (x)", ylabel="Output (x)", legend=false)

    # Plot government spending comparison (g)
    p5 = bar(scenarios, g_values, title="Government Spending Comparison (g)", ylabel="Government Spending (g)", legend=false)

    # Display τ and g plots separately from the rest
    plot(p1, p3, p4, layout=(3, 1)) # First plot: π, loss, and output
    #plot(p2, p5, layout=(2, 1))     # Second plot: fiscal policy (τ) and government spending (g)
end

# Example usage
#β::Float64   # Central bank's weight on output
#θ_x::Float64 # Fiscal authority's weight on output
#θ_g::Float64 # Fiscal authority's weight on government spending
#x_star::Float64 # Target level of output
#g_star::Float64 # Target level of government spending
#a::Float64   # Productivity shock
#w_star::Float64 # Target real wage
#α::Float64   # Output sensitivity to inflation

params = ModelParams(0.5, 0.7, 1.0, 1.0, 1.0, 0.1, 1.0, 1.2)
scenarios, π_values, τ_values, loss_values, x_values, g_values = compare_games(params)
plot_results(scenarios, π_values, τ_values, loss_values, x_values, g_values)

