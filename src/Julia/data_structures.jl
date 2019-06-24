__precompile__()

module ObsDataStructures
import Base: getindex, +
export label_treatments,
       Counterfactuals, Outcome, Covariates, ObsData,
       getindex, Γ, +

function label_treatments(W::Vector)
    treatment_levels = sort!([w for w in Set(W)])
    if length(treatment_levels) != 2
        error("Only binary treatments")
    end
    return Dict(label=>t for (label, t) in zip([true, false], treatment_levels))
end

type Counterfactuals{T<:Real}
    treated::Dict{Bool,Vector{T}}
    observed::Dict{Bool,Vector{T}}
    W::Vector{Bool}
end

function Counterfactuals{T<:Real}(treated::Dict{Bool,Vector{T}}, W::Vector{Bool})
    observed = Dict(c=>Vector{T}(length(W)) for c in [true, false])
    observed[true][W] = treated[true][W]
    observed[true][!W] = treated[false][!W]
    observed[false][W] = treated[false][W]
    observed[false][!W] = treated[true][!W]
    return Counterfactuals(treated, observed, W)
end

function getindex(F::Counterfactuals, index)
    return Counterfactuals(Dict(true=>F.treated[true][index],
                                false=>F.treated[false][index]),
                           F.W[index])
end

function Γ(F::Counterfactuals; o::Union{Bool,Void}=nothing, t::Union{Bool,Void}=nothing)
    if (o==nothing) & (t==nothing)
        error("no conditions provided")
    elseif o==nothing
        return F.treated[t]
    elseif t==nothing
        return F.observed[o]
    else
        return t ? F.observed[o][F.W] : F.observed[o][!F.W]
    end
end

function +(F::Counterfactuals, f::Counterfactuals)
    treated = Dict(t=>F.treated[t]+f.treated[t] for t in (true, false))
    observed = Dict(t=>F.observed[t]+f.observed[t] for t in (true, false))
    return Counterfactuals(treated, observed, F.W)
end

immutable Outcome{T<:Real}
    Y::Vector{T}
    W::Vector{Bool}
end

function getindex(Y::Outcome, index)
    return Outcome(Y.Y[index], Y.W[index])
end

function Γ(Y::Outcome; t::Bool=true)
    t ? Y.Y[Y.W] : Y.Y[!Y.W]
end

function -(Y::Outcome, F::Counterfactuals)
    return Outcome(Y.Y - F.observed[true], Y.W)
end

immutable Covariates{T<:Real}
    X::Matrix{T}
    W::Vector{Bool}
end

function getindex(X::Covariates, index)
    return Covariates(X.X[index,:], X.W[index])
end

function Γ(X::Covariates; t::Bool=true)
    t ? X.X[X.W,:] : X.X[!X.W,:]
end

struct ObsData
    X::Covariates
    W::Vector{Bool}
    Y::Outcome
    trts::Dict{Bool,Any}
    N::Int
    N_treated::Dict{Bool,Int}
end

function ObsData(X::Matrix, W::Vector, Y::Vector; trts=nothing)
    trts == nothing ? trts = label_treatments(W) : nothing
    W_bool = Vector{Bool}(length(W))
    for (t,w) in trts
        W_bool[W.==w] = t
    end
    return ObsData(Covariates(X,W_bool), W_bool, Outcome(Y, W_bool), trts, length(W), Dict(true=>sum(W_bool), false=>sum(!W_bool)))
end

function getindex(data::ObsData, index)
    return ObsData(data.X[index], data.W[index], data.Y[index], data.trts,
                   length(data.W), Dict(true=>sum(data.W), false=>sum(!data.W)))
end

end #module
