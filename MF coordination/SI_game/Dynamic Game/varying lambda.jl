## The consistent conjecture approach, using a few discrete points.

#using Pkg
#Pkg.add("SymPy")
#Pkg.add("NLsolve")
#Pkg.add("Optim")
#Pkg.add("Plots")
#Pkg.add("DataFrames")
#Pkg.add("StatsPlots")
#Pkg.add("LinearAlgebra")
#Pkg.add("ForwardDiff")

using SymPy, Optim, Plots

# Step 1: Define symbolic variables
@syms ω_f ψ_f c_f ω_m ψ_m c_m
@syms gamma alpha bG bB dG dB eG eB theta_G_star theta_B_star
@syms mu_G mu_B delta lambda lambda_G pi_G_star pi_B_star y_G_star y_B_star beta_G_star beta_B_star

n = 50  # Considering time periods for simulation/plots
f = [symbols("f_$i") for i in 1:n]  # Fiscal policy choices at times
m = [symbols("m_$i") for i in 1:n]  # Monetary policy choices at times

# If this is 1 then we set lambda_G to a fixed value, if not we ignore it (set to 1 for the "leadership" example)
set_G = 0

# Parameters
params_gov = [
    1.5,   # gamma - intercept of inflation
    1.5,   # alpha - intercept of output
    0.5,   # bG - sensitivity of output to fiscal policy
    0.5,   # bB - sensitivity of output to monetary policy
    0.5,   # dG - sensitivity of inflation to fiscal policy
    0.5,   # dB - sensitivity of inflation to monetary policy
    1.0,   # eG - relation between government choice and its "neutral choice"
    1.0,   # eB - relation between monetary choice and its "neutral choice"
    0.25,   # theta_G_star - cost for differing from zero
    0.25,   # theta_B_star - cost for differing from zero
    0.0,   # mu_G - government weight on inflation (1-mu_G is the weight on output)
    1.0,   # mu_B - monetary weight on inflation (1-mu_B is the weight on output)
    0.9,   # delta - time discount rate
    2.0,   # pi_G_star - government inflation target
    2.0,   # pi_B_star - monetary inflation target
    2.0,   # y_G_star - government output target
    2.0,   # y_B_star - monetary output target
    0.0,    # lambda_G - comment this out if you do not want the lambdas to differ. This sets the lambda_G fixed.
    0.0,   # beta_G_star - government instrument target
    0.0    # beta_B_star - monetary instrument target
]

param_subs = Dict(
    gamma => params_gov[1], alpha => params_gov[2], bG => params_gov[3], bB => params_gov[4], 
    dG => params_gov[5], dB => params_gov[6], eG => params_gov[7], eB => params_gov[8], 
    theta_G_star => params_gov[9], theta_B_star => params_gov[10], 
    mu_G => params_gov[11], mu_B => params_gov[12], delta => params_gov[13], 
    pi_G_star => params_gov[14], pi_B_star => params_gov[15], 
    y_G_star => params_gov[16], y_B_star => params_gov[17], 
    beta_G_star => params_gov[19], beta_B_star => params_gov[20], lambda_G => params_gov[18]
)

# Step 2: Define time functions for losses and conjectures

L_G = Sym[0 for _ in 1:n]  # Loss functions for government
L_B = Sym[0 for _ in 1:n]  # Loss functions for monetary authority
conjecture_f = Sym[0 for _ in 1:n]  # Conjecture for fiscal policy
conjecture_m = Sym[0 for _ in 1:n]  # Conjecture for monetary policy

for t in 2:4
    if set_G == 1
        L_G[t] = mu_G * (pi_G_star - (gamma - dG * f[t] - dB * m[t]))^2 +
                 (1 - mu_G) * (y_G_star - (alpha - bG * f[t] - bB * m[t]))^2 +
                 lambda_G * (f[t] - f[t-1])^2 + theta_G_star * (beta_G_star - eG * f[t])^2
    else
        L_G[t] = mu_G * (pi_G_star - (gamma - dG * f[t] - dB * m[t]))^2 +
                 (1 - mu_G) * (y_G_star - (alpha - bG * f[t] - bB * m[t]))^2 +
                 lambda * (f[t] - f[t-1])^2 + theta_G_star * (beta_G_star - eG * f[t])^2
    end

    L_B[t] = mu_B * (pi_B_star - (gamma - dG * f[t] - dB * m[t]))^2 +
        (1 - mu_B) * (y_B_star - (alpha - bG * f[t] - bB * m[t]))^2 +
        lambda * (m[t] - m[t-1])^2 + theta_B_star * (beta_B_star - eB * m[t])^2 

    conjecture_f[t] = ω_f * f[t-1] + ψ_f * m[t-1] + c_f
    conjecture_m[t] = ω_m * f[t-1] + ψ_m * m[t-1] + c_m
end

Euler_G = diff(L_G[2], f[2]) + delta * (diff(conjecture_m[3], f[2]) * diff(L_G[3], m[3]) + diff(L_G[3], f[2])) - (diff(conjecture_m[3], f[2]) * diff(conjecture_m[4], m[3]) / diff(conjecture_m[4], f[3])) * (delta * diff(L_G[3], f[3]) + delta^2 * diff(L_G[4], f[3]))

Euler_B = diff(L_B[2], m[2]) + delta * (diff(conjecture_f[3], m[2]) * diff(L_B[3], f[3]) + diff(L_B[3], m[2])) - (diff(conjecture_f[3], m[2]) * diff(conjecture_f[4], f[3]) / diff(conjecture_f[4], m[3])) * (delta * diff(L_B[3], m[3]) + delta^2 * diff(L_B[4], m[3]))

Euler_G_subs = subs(Euler_G, Dict(f[4] => conjecture_f[4], m[4] => conjecture_m[4]))
Euler_G_subs = subs(Euler_G_subs, Dict(f[3] => conjecture_f[3], m[3] => conjecture_m[3]))
Euler_G_subs = subs(Euler_G_subs, Dict(f[2] => conjecture_f[2], m[2] => conjecture_m[2]))

Euler_B_subs = subs(Euler_B, Dict(f[4] => conjecture_f[4], m[4] => conjecture_m[4]))
Euler_B_subs = subs(Euler_B_subs, Dict(f[3] => conjecture_f[3], m[3] => conjecture_m[3]))
Euler_B_subs = subs(Euler_B_subs, Dict(f[2] => conjecture_f[2], m[2] => conjecture_m[2]))

# Generate 6 equations
cases = [
    Dict(f[1] => 0, m[1] => 0),
    Dict(f[1] => 1, m[1] => 0),
    Dict(f[1] => 0, m[1] => 1)
]

results = []
lambdas = 0:0.1:0.5

# Inside the loop, compute L_G_value
for λ in lambdas
    param_subs[lambda] = λ

    Euler_G_subs2 = subs(Euler_G_subs, param_subs)
    Euler_B_subs2 = subs(Euler_B_subs, param_subs)
    
    equations = []
    for case in cases
        push!(equations, subs(Euler_G_subs2, case))
        push!(equations, subs(Euler_B_subs2, case))
    end

    function residuals(x)
        ω_f_val, ψ_f_val, c_f_val, ω_m_val, ψ_m_val, c_m_val = x
        subs_values = Dict(ω_f => ω_f_val, ψ_f => ψ_f_val, c_f => c_f_val, ω_m => ω_m_val, ψ_m => ψ_m_val, c_m => c_m_val)
        return [Float64(subs(eq, subs_values)) for eq in equations]
    end

    function objective(x)
        sum(residual^2 for residual in residuals(x))  # Minimize the sum of squared residuals
    end

    initial_guess = [0.1, 0.1, 0.1, 0.1, 0.1, 0.1]

    result = optimize(objective, initial_guess, BFGS())

    solution = Optim.minimizer(result)

    f_values = [0.0 for _ in 1:n]
    m_values = [0.0 for _ in 1:n]

    for t in 2:n
        f_values[t] = solution[1] * f_values[t-1] + solution[2] * m_values[t-1] + solution[3]
        m_values[t] = solution[4] * f_values[t-1] + solution[5] * m_values[t-1] + solution[6]
    end

    inflation = params_gov[1] - params_gov[5] * f_values[n] - params_gov[6] * m_values[n]
    output = params_gov[2] - params_gov[3] * f_values[n] - params_gov[4] * m_values[n]
    
    L_B_value = params_gov[12] * (params_gov[15] - inflation)^2 +
                (1 - params_gov[12]) * (params_gov[17] - output)^2 +
                λ * (m_values[n] - m_values[n-1])^2 +
                params_gov[10] * (params_gov[20] - params_gov[8] * m_values[n])^2

    L_G_value = params_gov[11] * (params_gov[14] - inflation)^2 +
                (1 - params_gov[11]) * (params_gov[16] - output)^2 +
                λ * (f_values[n] - f_values[n-1])^2 +
                params_gov[9] * (params_gov[19] - params_gov[7] * f_values[n])^2

    total_loss = L_B_value + L_G_value

    push!(results, (λ, f_values[n], m_values[n], inflation, output, L_B_value, L_G_value, total_loss))
end

# Update DataFrame
df = DataFrame(λ = [r[1] for r in results], 
               f_100 = [r[2] for r in results], 
               m_100 = [r[3] for r in results], 
               inflation_100 = [r[4] for r in results], 
               output_100 = [r[5] for r in results], 
               L_B_100 = [r[6] for r in results],
               L_G_100 = [r[7] for r in results],
               total_loss = [r[8] for r in results])


## Set up plots
# Define the folder path relative to the script's directory
script_dir = @__DIR__
folder_path = joinpath(script_dir, "plots")
mkpath(folder_path)  # Create the folder if it doesn't exist

# Generate and save plots
p1 = plot(df.λ, df.f_100, label="f", xlabel="λ", ylabel="Value", title="f vs λ")
savefig(p1, joinpath(folder_path, "f_vs_lambda.png"))

p2 = plot(df.λ, df.m_100, label="m", xlabel="λ", ylabel="Value", title="m vs λ")
savefig(p2, joinpath(folder_path, "m_vs_lambda.png"))

p3 = plot(df.λ, df.inflation_100, label="Inflation", xlabel="λ", ylabel="Value", title="inflation vs λ")
savefig(p3, joinpath(folder_path, "inflation_vs_lambda.png"))

p4 = plot(df.λ, df.output_100, label="Output", xlabel="λ", ylabel="Value", title="output vs λ")
savefig(p4, joinpath(folder_path, "output_vs_lambda.png"))

p5 = plot(df.λ, df.output_100, label="Output", xlabel="λ", ylabel="Value", title="inflation and output vs λ")
plot!(p5, df.λ, df.inflation_100, label="Inflation")
savefig(p5, joinpath(folder_path, "inflation_and_output_vs_lambda.png"))

# Comparing choices (add the legend none for the supply shocks)
p6 = plot(df.λ, df.m_100, xlabel="λ", ylabel="Value", title="f and m vs λ", label="m") # , legend=:none
plot!(p6, df.λ, df.f_100, label="f") # , legend=:none
annotate!(p6, df.λ[4], 0.5, text("f", :left, 10, :red))
annotate!(p6, df.λ[4], -0.5, text("m", :right, 10, :blue))
savefig(p6, joinpath(folder_path, "f_and_m_vs_lambda.png"))

p7 = plot(df.λ, df.L_B_100, label="Loss Bank", xlabel="λ", ylabel="Monetary Authority Loss", title="Loss of Monetary Authority vs λ")
savefig(p7, joinpath(folder_path, "monetary_loss_vs_lambda.png"))

p8 = plot(df.λ, df.L_B_100, label="Loss Bank", xlabel="λ", ylabel="Authority Loss", title="Loss of Monetary and Fiscal Authority vs λ")
plot!(p8, df.λ, df.L_G_100, label="Loss Fiscal")
savefig(p8, joinpath(folder_path, "loss_fiscal_and_monetary_vs_lambda.png"))

p9 = plot(df.λ, df.total_loss, label="Total Loss", xlabel="λ", ylabel="Total Loss", title="Total Loss vs λ")
savefig(p9, joinpath(folder_path, "total_loss_vs_lambda.png"))