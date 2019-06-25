# Adapted from https://github.com/bensadeghi/DecisionTree.jl
__precompile__()

module Estimators
import Base: length, convert, promote_rule, show, start, next, done
export Estimator, Leaf, Node,
       depth,
       fit_regression_tree,
       predict, assign_leaves

float(x) = map(Float64, x)
neg(arr) = map(!, arr) # `.!arr` is invalid in 0.5, and `!arr` triggers a warning in 0.6.
const NO_BEST=(0,0)

# converts a boolean vector into an integer by summing the appropriate powers of 2
boolvec2int(v::Vector{Bool}) = sum(b*v for (b,v) in zip(2.^(0:(length(v)-1)), v))

abstract type Estimator
end

immutable Leaf <: Estimator
    id::Integer
    value::Real
end
    length(leaf::Leaf) = 1
    depth(leaf::Leaf) = 0

Leaf(id::Vector{Bool}, value::Real) = Leaf(boolvec2int(id), value)
Leaf(value::Real) = Leaf(0,value)

immutable Node <: Estimator
    node_id::Integer
    featid::Integer
    featval::Any
    left::Union{Leaf,Node}
    right::Union{Leaf,Node}
end
    length(tree::Node) = length(tree.left) + length(tree.right)
    depth(tree::Node) = 1 + max(depth(tree.left), depth(tree.right))

function Node(id::Vector{Bool}, featid::Integer, featval::Any, left::Union{Leaf,Node}, right::Union{Leaf,Node})
    return Node(boolvec2int(id), featid, featval, left, right)
end

convert(::Type{Node}, x::Leaf) = Node(0, nothing, x, Leaf(nothing,[nothing])) #makes a node with that leaf as it's left node
promote_rule(::Type{Node}, ::Type{Leaf}) = Node # this tells julia to call convert(::node, leaf) on the leaf when this happens
promote_rule(::Type{Leaf}, ::Type{Node}) = Node

function fit_regression_tree{T<:Float64, U<:Real}(X::Matrix{U}, Y::Vector{T}; min_samples_leaf=5, max_depth=-1, node_id::Vector{Bool}=[true])
    if max_depth < -1
        error("Unexpected value for max_depth: $(max_depth) (expected: max_depth >= 0, or max_depth = -1 for infinite depth)")
    end
    if length(Y) <= min_samples_leaf || max_depth==0
        return Leaf(node_id, mean(Y))

    end
    S = _split_mse(X, Y)
    if S == NO_BEST
        return Leaf(node_id, mean(Y))
    end
    feat_id, thresh = S
    split = X[:,feat_id] .< thresh
    return Node(node_id, feat_id, thresh,
                fit_regression_tree(X[split,:], Y[split],
                    min_samples_leaf=min_samples_leaf, max_depth=max(max_depth-1, -1), node_id=vcat(true,node_id)),
                fit_regression_tree(X[neg(split),:], Y[neg(split)],
                    min_samples_leaf=min_samples_leaf, max_depth=max(max_depth-1, -1), node_id=vcat(false,node_id))
                )
end

function _split_mse{T<:Float64, U<:Real}(X::Matrix{U}, Y::Vector{T})
    N, p = size(X)

    best = NO_BEST
    best_val = -Inf

    for i in 1:p
        ord = sortperm(X[:,i])
        X_i = X[ord,i]
        Y_i = Y[ord]
        if N > 100
            if VERSION >= v"0.4.0-dev"
                domain_i = quantile(X_i, linspace(0.01, 0.99, 99);
                                    sorted=true)
            else  # sorted=true isn't supported on StatsBase's Julia 0.3 version
                domain_i = quantile(X_i, linspace(0.01, 0.99, 99))
            end
        else
            domain_i = X_i
        end
        value, thresh = _best_mse_loss(X_i, Y_i, domain_i)
        if value > best_val
            best_val = value
            best = (i, thresh)
        end
    end

    return best
end


function _best_mse_loss{T<:Float64, U<:Real}(X::Vector{U}, Y::Vector{T}, domain)
    # True, but costly assert. However, see
    # https://github.com/JuliaStats/StatsBase.jl/issues/164
    # @assert issorted(X) && issorted(domain)
    best_val = -Inf
    best_thresh = 0.0
    s_l = s2_l = zero(T)
    su = sum(Y)::T
    su2 = zero(T); for l in Y su2 += l*l end  # sum of squares
    nl = 0
    n = length(Y)
    i = 1
    # Because the `X` are sorted, below is an O(N) algorithm for finding
    # the optimal threshold amongst `domain`. We simply iterate through the
    # array and update s_l and s_r (= sum(Y) - s_l) as we go. - @cstjean
    @inbounds for thresh in domain
        while i <= length(Y) && X[i] < thresh
            l = Y[i]

            s_l += l
            s2_l += l*l
            nl += 1

            i += 1
        end
        s_r = su - s_l
        s2_r = su2 - s2_l
        nr = n - nl
        # This check is necessary I think because in theory all Y could
        # be the same, then either nl or nr would be 0. - @cstjean
        if nr > 0 && nl > 0
            loss = s2_l - s_l^2/nl + s2_r - s_r^2/nr
            if -loss > best_val
                best_val = -loss
                best_thresh = thresh
            end
        end
    end
    return best_val, best_thresh
end

predict(leaf::Leaf, feature::Vector) = leaf.value

function predict(tree::Node, X::Vector)
    if tree.featval == nothing
        return predict(tree.left, X)
    elseif X[tree.featid] < tree.featval
        return predict(tree.left, X)
    else
        return predict(tree.right, X)
    end
end

function predict(tree::Union{Leaf,Node}, X::Matrix)
    N = size(X,1)
    predictions = Array{Any}(N)
    for i in 1:N
        predictions[i] = predict(tree, X[i,:])
    end
    println("predictions ->")
    println(predictions)
    if typeof(predictions[1]) <: Float64
        return float(predictions)
    else
        return predictions
    end
end

function predict(estimator_dict::Dict, X::Matrix)
    return Dict(k=>predict(v,X) for (k,v) in estimator_dict)
end

assign_leaves(leaf::Leaf, feature::Vector) = leaf.id

function assign_leaves(tree::Node, X::Vector)
    if tree.featval == nothing
        return assign_leaves(tree.left, X)
    elseif X[tree.featid] < tree.featval
        return assign_leaves(tree.left, X)
    else
        return assign_leaves(tree.right, X)
    end
end

function assign_leaves(tree::Union{Leaf,Node}, X::Matrix)
    N = size(X,1)
    assignments = Array{Int}(N)
    for i in 1:N
        assignments[i] = assign_leaves(tree, X[i,:])
    end
    return assignments
end

function assign_leaves(estimator_dict::Dict, X::Matrix)
    return Dict(k=>assign_leaves(v,X) for (k,v) in estimator_dict)
end

end #module
