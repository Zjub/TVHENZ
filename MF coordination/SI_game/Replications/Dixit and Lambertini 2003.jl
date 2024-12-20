## Dixit and Lambertini 2003

using Optim
using Plots

# Parameters
α = 0.5   # Sensitivity of output to unexpected inflation
β = 0.5   # Sensitivity of output to real interest rate
λ_m = 0.5 # Monetary authority's weight on output stabilization
λ_f = 1.0 # Fiscal authority's weight on output stabilization
π_star = 2.0  # Inflation target
y_star = 100.0 # Output target
η = 100.0     # Natural level of output

# Shock values
ϵ_s = 0.0  # Supply shock
ϵ_d = 0.0  # Demand shock

# Objective functions
function loss_monetary(π, y)
    return 0.5 * (π - π_star)^2 + 0.5 * λ_m * (y - y_star)^2
end

function loss_fiscal(π, y)
    return 0.5 * (π - π_star)^2 + 0.5 * λ_f * (y - y_star)^2
end

# Solve for Nash equilibrium
function nash_equilibrium()
    function objective(vars)
        π, y = vars
        L_m = loss_monetary(π, y)
        L_f = loss_fiscal(π, y)
        return L_m + L_f
    end
    result = optimize(objective, [π_star, y_star])
    return Optim.minimizer(result)
end

π_eq, y_eq = nash_equilibrium()

println("Nash Equilibrium:")
println("Inflation: ", π_eq)
println("Output: ", y_eq)

# Plotting results
bar(["Inflation", "Output"], [π_eq, y_eq], title="Nash Equilibrium Outcomes", ylabel="Value")
