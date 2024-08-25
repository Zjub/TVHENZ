function get_piH_new(par, x)
    # Ensure x is treated as a scalar
    piH = 1 / (1 + exp(x[1]))

    # Calculate the objective function value
    F = abs(piH * (1 - piH) - ((1 - par[:u]) / par[:u]) * (par[:s] / par[:f]) * par[:volsh])

    return F
end
