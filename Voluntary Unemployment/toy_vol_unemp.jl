# Lifecycle employment choice model
# 30/03/2024
# Author: Matt Nolan

import Pkg
Pkg.add(["Distributions", "LinearAlgebra", "Interpolations", "Plots", "Optim"])

using LinearAlgebra
using Distributions
using Interpolations
using Plots
using Optim

## Basic setup
modelname = "mod_job"
figdir = "./../Figures/"