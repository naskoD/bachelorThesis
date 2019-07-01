
include("constrained_boosting.jl")

using CausalEffectMorph
using Distributions

# This generates some fake data for demonstration purposes, but you'll have to load in whatever real dataset you want to use here and use ObsData() to create the data object.
logistic(x) = 1./(1+exp(-x))
N,p = 100, 2
X1 = [rand(Uniform(-π,π)) for i in 1:N]
X2 = [0 for i in 1:N]
X = hcat(X1, X2)
W = [rand(Distributions.Binomial(1,logistic(logodds))) for logodds in 2*X1]
outcome_gen(X,W) = sin(X1) + [rand(Normal(0,0.15)) for i in 1:N]
Yobs = outcome_gen(X1,W)
data = CausalEffectMorph.ObsData(X,W,Yobs, trts=Dict(true=>1, false=>0));

# parameters for constrained boosting
τ = -2 # the synthetic effect you want to have
n_trees=100 # max number of trees to use for constrained boosting
Δ = 2 #  max tree depth
λ = 2 # regularization parameter
s = 10 # minimum number of samples per leaf in each tree

# Cross-validated over the parameters and generate the synthetic outcomes.
tr_err, te_err = CausalEffectMorph.cross_validate(data, CausalEffectMorph.Squared(), τ, n_trees,
                                    max_depth=Δ, λ=λ, min_samples_leaf=s,
                                    nfolds=4);
n_trees_min_cv = indmin(mean(hcat(te_err...),2)) - 1
F = CausalEffectMorph.constrained_boost(data, CausalEffectMorph.Squared(), τ, n_trees_min_cv,
                          max_depth=Δ, λ=λ, min_samples_leaf=s)

# Get the (mean) synthetic potential outcomes (no noise added yet)
mu1_synth = F[end].treated[true]
mu0_synth = F[end].treated[false]

ste=(sum(mu1_synth)-sum(mu0_synth))/N
println(ste)
