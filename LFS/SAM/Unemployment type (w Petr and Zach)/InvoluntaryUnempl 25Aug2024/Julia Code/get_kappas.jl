function get_kappas(par, x)
    # Unpack the input parameters
    kappaH = exp(x[1])
    kappaL = exp(x[2])

    # Compute the output
    F1 = kappaH - par[:theta] * (1 - par[:u]) / par[:u]
    F2 = kappaL - par[:theta] * (par[:u]) / par[:u]

    # Return a scalar value: sum of squared differences
    return F1^2 + F2^2
end
