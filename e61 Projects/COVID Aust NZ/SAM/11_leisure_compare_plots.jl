# compare_did_job_finding_rates.jl

using DataFrames, CSV, Plots

# 📥 Load data
df_0p = CSV.read("did_job_finding_rates_0p.csv", DataFrame)
df_10p = CSV.read("did_job_finding_rates_10p.csv", DataFrame)
df_20p = CSV.read("did_job_finding_rates_20p.csv", DataFrame)

# 📊 Full Shock plot
p1 = plot(
    df_0p.Month, df_0p.DiD_Full_Shock, label="0% Shock", lw=2, color=:blue,
    xlabel="Month", ylabel="Difference-in-Differences (p.p.)", 
    title="Full Shock: Job-Finding Rates"
)
plot!(p1, df_10p.Month, df_10p.DiD_Full_Shock, label="10% Shock", lw=2, ls=:dash, color=:green)
plot!(p1, df_20p.Month, df_20p.DiD_Full_Shock, label="20% Shock", lw=2, ls=:dot, color=:red)

# 📊 Benefit Only plot
p2 = plot(
    df_0p.Month, df_0p.DiD_Benefit_Only, label="0% Shock", lw=2, color=:blue,
    xlabel="Month", ylabel="Difference-in-Differences (p.p.)", 
    title="Benefit Only: Job-Finding Rates"
)
plot!(p2, df_10p.Month, df_10p.DiD_Benefit_Only, label="10% Shock", lw=2, ls=:dash, color=:green)
plot!(p2, df_20p.Month, df_20p.DiD_Benefit_Only, label="20% Shock", lw=2, ls=:dot, color=:red)

# 📦 Combine side by side
combined = plot(p1, p2, layout=(1,2), size=(1200, 600))

# 💾 Save to PNG
savefig(combined, "did_job_finding_leisurecomparison_side_by_side.png")
savefig(p1, "did_job_finding_leisurecomparison.png")
savefig(p2, "did_job_finding_leisurecomparison_implied_benefit.png")

# 👀 Display
display(combined)
display(p1)
display(p2)