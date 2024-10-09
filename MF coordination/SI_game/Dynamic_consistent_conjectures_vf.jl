### Attempt to use value functions, but again appears to be the NE.

using LinearAlgebra
using Optim

# Step 1: Define parameters
struct Params
    γ::Float64  # Intercept of inflation equation
    α::Float64  # Intercept of output equation
    bG::Float64 # Sensitivity of output to fiscal policy
    bB::Float64 # Sensitivity of output to monetary policy
    dG::Float64 # Sensitivity of inflation to fiscal policy
    dB::Float64 # Sensitivity of inflation to monetary policy
    eG::Float64 # Cost of fiscal policy adjustment from zero
    eB::Float64 # Cost of monetary policy adjustment from zero
    θG::Float64 # Government's target for fiscal policy
    θB::Float64 # Central bank's target for monetary policy
    μG::Float64 # Weight on inflation in government's loss function
    μB::Float64 # Weight on inflation in central bank's loss function
    δ::Float64  # Discount factor
    λG::Float64 # Adjustment cost for fiscal policy
    λB::Float64 # Adjustment cost for monetary policy
    πG_star::Float64 # Government's inflation target
    πB_star::Float64 # Central bank's inflation target
    yG_star::Float64 # Government's output target
    yB_star::Float64 # Central bank's output target
end

# Initialize parameters
p = Params(
    1.5, 1.5, 0.5, 0.5, 0.5, 0.5, 0.1, 0.1, 0.0, 0.0, 0.0, 1.0, 0.9, 0.5, 0.5, 2.0, 2.0, 2.0, 2.0
)

# Step 2: Define economic outcomes function
function economic_outcomes(f::Float64, m::Float64, y_prev::Float64, π_prev::Float64, p::Params)
    y = p.α + p.bG * f + p.bB * m
    π = p.γ + p.dG * f + p.dB * m
    return y, π
end

# Step 3: Define loss functions
function loss_G(f::Float64, m::Float64, f_prev::Float64, y::Float64, π::Float64, p::Params)
    return p.μG * (p.πG_star - π)^2 + 
           (1 - p.μG) * (p.yG_star - y)^2 + 
           p.λG * (f - f_prev)^2 + 
           p.eG * (f - p.θG)^2
end

function loss_B(f::Float64, m::Float64, m_prev::Float64, y::Float64, π::Float64, p::Params)
    return p.μB * (p.πB_star - π)^2 + 
           (1 - p.μB) * (p.yB_star - y)^2 + 
           p.λB * (m - m_prev)^2 + 
           p.eB * (m - p.θB)^2
end

# Step 4: Define value functions
function value_G(f::Float64, m::Float64, f_prev::Float64, m_prev::Float64, y::Float64, π::Float64, V_next_G::Function, p::Params)
    y_next, π_next = economic_outcomes(f, m, y, π, p)
    return loss_G(f, m, f_prev, y, π, p) + p.δ * V_next_G(f, m, f, m, y_next, π_next)
end

function value_B(f::Float64, m::Float64, f_prev::Float64, m_prev::Float64, y::Float64, π::Float64, V_next_B::Function, p::Params)
    y_next, π_next = economic_outcomes(f, m, y, π, p)
    return loss_B(f, m, m_prev, y, π, p) + p.δ * V_next_B(f, m, f, m, y_next, π_next)
end

# Step 5: Define reaction functions
function reaction_G(m::Float64, f_prev::Float64, m_prev::Float64, y::Float64, π::Float64, V_next_G::Function, p::Params)
    objective(f) = value_G(f, m, f_prev, m_prev, y, π, V_next_G, p)
    result = optimize(objective, -1.0, 1.0)
    return Optim.minimizer(result)
end

function reaction_B(f::Float64, f_prev::Float64, m_prev::Float64, y::Float64, π::Float64, V_next_B::Function, p::Params)
    objective(m) = value_B(f, m, f_prev, m_prev, y, π, V_next_B, p)
    result = optimize(objective, -1.0, 1.0)
    return Optim.minimizer(result)
end

# Step 6: Solve the dynamic game
function solve_dynamic_game(p::Params; max_iter::Int=1000, tol::Float64=1e-6)
    # Initialize value functions
    V_G(f, m, f_prev, m_prev, y, π) = loss_G(f, m, f_prev, y, π, p) / (1 - p.δ)
    V_B(f, m, f_prev, m_prev, y, π) = loss_B(f, m, m_prev, y, π, p) / (1 - p.δ)
    
    f, m = 0.0, 0.0  # Initial policy choices
    y, π = economic_outcomes(f, m, 0.0, 0.0, p)  # Initial economic conditions
    
    for iter in 1:max_iter
        V_G_old, V_B_old = V_G, V_B
        f_old, m_old = f, m
        
        # Update fiscal policy
        f = reaction_G(m, f_old, m_old, y, π, V_G, p)
        
        # Update monetary policy
        m = reaction_B(f, f_old, m_old, y, π, V_B, p)
        
        # Update economic outcomes
        y, π = economic_outcomes(f, m, y, π, p)
        
        # Update value functions
        V_G = (f, m, f_prev, m_prev, y, π) -> value_G(f, m, f_prev, m_prev, y, π, V_G_old, p)
        V_B = (f, m, f_prev, m_prev, y, π) -> value_B(f, m, f_prev, m_prev, y, π, V_B_old, p)
        
        # Check for convergence
        if max(abs(f - f_old), abs(m - m_old)) < tol
            println("Converged after $iter iterations")
            break
        end
    end
    
    return f, m, y, π, V_G, V_B
end

# Step 7: Run the model and print results
f_star, m_star, y_star, π_star, V_G_star, V_B_star = solve_dynamic_game(p)

println("Equilibrium policy choices:")
println("  Fiscal authority (f): ", f_star)
println("  Monetary authority (m): ", m_star)
println("Equilibrium output: ", y_star)
println("Equilibrium inflation: ", π_star)
println("Government's loss: ", loss_G(f_star, m_star, f_star, y_star, π_star, p))
println("Central Bank's loss: ", loss_B(f_star, m_star, m_star, y_star, π_star, p))

# Step 8: Compute impulse responses
function impulse_response(p::Params, periods::Int=20)
    f, m = f_star, m_star  # Start from equilibrium
    y, π = y_star, π_star
    
    # Apply a shock to output
    y += 1.0
    
    responses = [(f, m, y, π)]
    
    for _ in 1:periods-1
        f_old, m_old = f, m
        f = reaction_G(m, f_old, m_old, y, π, V_G_star, p)
        m = reaction_B(f, f_old, m_old, y, π, V_B_star, p)
        y, π = economic_outcomes(f, m, y, π, p)
        push!(responses, (f, m, y, π))
    end
    
    return responses
end

# Compute and print impulse responses
ir = impulse_response(p)
println("\nImpulse Responses:")
for (t, (f, m, y, π)) in enumerate(ir)
    println("t=$t: f=$f, m=$m, y=$y, π=$π")
end