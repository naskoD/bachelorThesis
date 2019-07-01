# note that when using the binomial loss, the treatment effect must be between -1 and 1
# TO DO:
# - Get rid of learning rate mechanic- prove that it works in the continuous case but moves us off the constraint surface with binary
# - Initialize the step seach leaf values with those computed by the trees
# - Add in a regularizer to do the job of the learning rate

__precompile__()

module CausalEffectMorph

export ObsData,
       Squared, Binomial,
       constrained_boost, cross_validate

include_dependency("data_structures.jl")
include_dependency("losses.jl")
include_dependency("estimators.jl")

include("data_structures.jl")
include("losses.jl")
include("estimators.jl")

using .Estimators
using .Losses
using .ObsDataStructures

using DataStructures
using JuMP, Ipopt

const TF = [true, false]

function fit_const_pair(data::ObsData, loss::Squared, τ::Real)
    Ȳ = mean(data.Y.Y)
    return Dict(t => Leaf(Ȳ +(2*t-1) * τ * data.N_treated[t]/data.N) for t in TF)
end

function fit_const_pair(data::ObsData, loss::Binomial, τ::Real)
    m = Model(solver=IpoptSolver(print_level=0))
    @variable(m, 0 <= πt <= 1)
    @variable(m, 0 <= πf <= 1)
    @constraint(m, πt-πf == τ)
    @NLobjective(m, Max, data.N_treated[true]*log(1-πf) +
                         data.N_treated[false]*log(πt))
    status = solve(m)
    π = Dict(true=>getvalue(πt), false=>getvalue(πf))
    return Dict(t  => Leaf(log(π[t]/(1-π[t]))) for t in TF)
end

function predict_counterfactuals{T<:Estimators.Estimator}(estimator_pair::Dict{Bool,T}, data::ObsData)
    return Counterfactuals(predict(estimator_pair, data.X.X), data.W)
end

function obs_gradient{T<:Loss}(loss::T, Y::Outcome, F::Counterfactuals; idx=:)
    return Dict(t => neg_grad(loss, Γ(Y[idx],t=t), Γ(F[idx],o=true,t=t)) for t in TF)
end

function build_leaf_index{T<:Estimators.Estimator}(tree_pair::Dict{Bool,T}, X::Covariates)
    assignments = Dict(t=>assign_leaves(tree_pair[t], X.X) for t in TF)
    aa = vcat([ls for (t,ls) in assignments]...)
    leaves = Set(vcat([ls for (t,ls) in assignments]...))
    assignment_index_matrix = [(t,l,find(assignments[t].==l)) for l in leaves, t in TF]
    assignment_index = OrderedDict((t,l)=>ind for (t,l,ind) in vec(assignment_index_matrix) if length(ind)>0)
    return assignment_index
end

function filter_index(index, include_subjects)
    filtered_index = OrderedDict()
    for (k,v) in index
        v_in = v ∩ include_subjects
        length(v_in)>0 ? filtered_index[k] = v_in : nothing
    end
    return filtered_index
end


function step_search(loss::Squared, data::ObsData, F::Counterfactuals, idx::OrderedDict, λ, τ=nothing; tr=:)
    F1 = F.treated[true]
    F0 = F.treated[false]
    Y = data.Y.Y
    W = data.W

    idx1, idx0 = OrderedDict(), OrderedDict()
    for ((t,l),i) in idx
        t ? idx1[l] = i : idx0[l] = i
    end

    idx1obs = filter_index(idx1, findin(W,1) ∩ tr)
    idx0obs = filter_index(idx0, findin(W,0) ∩ tr)

    m = Model(solver=IpoptSolver(print_level=0))
    @variable(m, ν1[1:length(idx1)])
    @variable(m, ν0[1:length(idx0)])
    @constraint(m, sum(sum(ν1[i] for j in idx1[l]) for (i,l) in enumerate(keys(idx1))) -
                   sum(sum(ν0[i] for j in idx0[l]) for (i,l) in enumerate(keys(idx0))) == 0)
    @objective(m, Min,
                   sum(sum((Y[j] - F1[j] - ν1[i])^2 for j in idx1obs[l]) for (i,l) in enumerate(keys(idx1obs))) +
                   sum(sum((Y[j] - F0[j] - ν0[i])^2 for j in idx0obs[l]) for (i,l) in enumerate(keys(idx0obs))) +
                   sum(sum(λ*ν1[i]^2 for j in idx1[l]) for (i,l) in enumerate(keys(idx1))) +
                   sum(sum(λ*ν0[i]^2 for j in idx0[l]) for (i,l) in enumerate(keys(idx0))))
    status = solve(m)
    νopt1 = getvalue(ν1)
    νopt0 = getvalue(ν0) # this is fucked. even when I add the learning rate back it's still broken...
    return Dict(true => Dict(l=>νi for (l,νi) in zip(keys(idx1),νopt1)),
                false => Dict(l=>νi for (l,νi) in zip(keys(idx0),νopt0)))
end

function step_search(loss::Binomial, data::ObsData, F::Counterfactuals, idx::OrderedDict, λ, τ; tr=:)
    F1 = F.treated[true]
    F0 = F.treated[false]
    Y = data.Y.Y
    W = data.W

    idx1, idx0 = OrderedDict(), OrderedDict()
    for ((t,l),i) in idx
        t ? idx1[l] = i : idx0[l] = i
    end

    idx1obs = filter_index(idx1, findin(W,1) ∩ tr)
    idx0obs = filter_index(idx0, findin(W,0) ∩ tr)

    m = Model(solver=IpoptSolver(print_level=0))
    @variable(m, ν1[1:length(idx1)])
    @variable(m, ν0[1:length(idx0)])
    @NLconstraint(m, sum(sum((1+exp(-F1[j]-ν1[i]))^(-1) for j in idx1[l]) for (i,l) in enumerate(keys(idx1))) -
                     sum(sum((1+exp(-F0[j]-ν0[i]))^(-1) for j in idx0[l]) for (i,l) in enumerate(keys(idx0))) == data.N*τ)
    @NLobjective(m, Min,
                    sum(sum(log(1+exp(F1[j]+ν1[i])) - Y[j]*(F1[j]+ν1[i]) for j in idx1obs[l]) for (i,l) in enumerate(keys(idx1obs))) +
                    sum(sum(log(1+exp(F0[j]+ν0[i])) - Y[j]*(F0[j]+ν0[i]) for j in idx0obs[l]) for (i,l) in enumerate(keys(idx0obs))) +
                    sum(sum(λ*ν1[i]^2 for j in idx1[l]) for (i,l) in enumerate(keys(idx1))) +
                    sum(sum(λ*ν0[i]^2 for j in idx0[l]) for (i,l) in enumerate(keys(idx0))))
    status = solve(m)
    νopt1 = getvalue(ν1)
    νopt0 = getvalue(ν0)
    return Dict(true => Dict(l=>νi for (l,νi) in zip(keys(idx1),νopt1)),
                false => Dict(l=>νi for (l,νi) in zip(keys(idx0),νopt0)))
end

function predict_counterfactuals(tree_pair, X::Covariates, ν)
    return Counterfactuals(Dict(t=>[ν[t][li] for li in assign_leaves(tree_pair[t], X.X)] for t in TF), X.W)
end

function constrained_boost{T<:Real, T2<:Real, L<:Loss}(data::ObsData, loss::L, τ::T, n_trees::Int;
                           max_depth::Int=3, λ::T2=2, min_samples_leaf::Int=1,
                           tr=:)
    F = Vector{Counterfactuals}(0)

    const_pair = fit_const_pair(data[tr], loss, τ)
    predicted_counterfactuals = predict_counterfactuals(const_pair, data)
    push!(F, predicted_counterfactuals)
    residuals = obs_gradient(loss, data.Y, F[end], idx=tr)

    for i in 1:n_trees
        tree_pair = Dict(t=>fit_regression_tree(Γ(data.X[tr],t=t) , residuals[t],
                                                        min_samples_leaf=min_samples_leaf,
                                                        max_depth=max_depth) for t in TF);
        idx = build_leaf_index(tree_pair, data.X)
        ν = step_search(loss, data, F[end], idx, λ, τ, tr=tr)
        push!(F, F[end]+predict_counterfactuals(tree_pair, data.X, ν))
        residuals = obs_gradient(loss, data.Y, F[end], idx=tr)
    end
    return F
end

import MLBase.StratifiedKfold

function cross_validate{T<:Real, T2<:Real, L<:Squared}(data::ObsData, loss::L, τ::T, n_trees::Int;
                           max_depth::Int=3, λ::T2=2, min_samples_leaf::Int=1,
                           nfolds::Int=5)
    training_error, test_error = Vector{Vector{Float64}}(0), Vector{Vector{Float64}}(0)
    loss == Binomial() ? folds = StratifiedKfold(data.Y.Y + 2*data.W, nfolds) : folds = StratifiedKfold(data.W, nfolds)
    for tr in folds
        println("tr")
        println(tr)
        te = [i for i in 1:data.N if i ∉ tr]
        F = constrained_boost(data, loss, τ, n_trees,
                           max_depth=max_depth, λ=λ, min_samples_leaf=min_samples_leaf,
                           tr=tr)
        push!(training_error, [sum(evaluate(loss, data.Y.Y[tr], F[n].observed[true][tr]))/length(data.Y.Y[tr]) for n in 1:n_trees+1])
        push!(test_error, [sum(evaluate(loss, data.Y.Y[te], F[n].observed[true][te]))/length(data.Y.Y[te]) for n in 1:n_trees+1])
    end
    return training_error, test_error
end

end # module
