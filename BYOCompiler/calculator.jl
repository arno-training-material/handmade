using JSON

function parse_expr(s::String, idx::Int)
    idx = skip_space(s, idx)
    if idx > length(s)
        error("empty program")
    end
    if s[idx] == '('
        idx += 1
        l = []
        while true
            idx = skip_space(s, idx)
            if idx > length(s)
                error("parenthesis never closes")
            elseif s[idx] == ')'
                idx += 1
                break
            end
            idx, v = parse_expr(s, idx)
            push!(l, v)
        end
        return idx, l
    elseif s[idx] == ')'
        error("parenthesis closed without opening")
    else
        start = idx
        while idx <= length(s) && !isspace(s[idx]) && s[idx] ∉ "()"
            idx += 1
        end
        return idx, parse_atom(s[start:idx-1])
    end
end
function skip_space(s::String, idx::Int)
    while idx <= length(s) && isspace(s[idx])
        idx += 1
    end
    idx
end
function parse_atom(s::String)
    try
        # Attempt to parse as a JSON value
        return ("val", JSON.parse(s))
    catch
        # If parsing fails, treat it as a symbol/string
        return s
    end
end

function pl_parse(s::String)
    idx, node = parse_expr(s, 1)
    idx = skip_space(s, idx)
    if idx != length(s) + 1
        println(idx)
        error("trailing garbage")
    end
    return node
end

pl_parse("1")
pl_parse("a")
pl_parse("bb")
pl_parse(" bb ")
try
    pl_parse(" b b ")
catch e
    return e
end
try
    pl_parse(" ")
catch e
    return e
end
pl_parse("(+ 1 3)")
pl_parse("()")
try
    pl_parse("())")
catch e
    return e
end
try
    pl_parse(")")
catch e
    return e
end
try
    pl_parse("(+ 1 3) b")
catch e
    return e
end
try
    pl_parse("(+ 1 3")
catch e
    return e
end
try
    pl_parse("")
catch e
    return e
end
pl_parse("(? (lt 1 3) \"yes\" \"no\")")
pl_parse("(print 1 2 3)")
pl_parse("(a(lt 1 3))")
function pl_eval(node)
    if length(node) == 0
        error("empty list")
    end
    if length(node) == 2 && node[1] == "val"
        return node[2]
    end
    binops = Dict("+" => +, "-" => -, "*" => *, "/" => /, "eq" => ==, "ne" => !=, "ge" => >=, "le" => <=, "lt" => <, "and" => &, "or" => |)
    if length(node) == 3 && node[1] in keys(binops)
        op = binops[node[1]]
        return op(pl_eval(node[2]), pl_eval(node[3]))
    end
    unops = Dict("-" => -, "not" => !)
    if length(node) == 2 && node[1] in keys(unops)
        op = unops[node[1]]
        return op(pl_eval(node[2]))
    end
    if length(node) == 4 && node[1] == "?"
        _, cond, yes, no = node
        if pl_eval(cond)
            return pl_eval(yes)
        else
            return pl_eval(no)
        end
    end
    if node[1] == "print"
        for val in node[2:end]
        println((pl_eval(val)))
        end
        return nothing
    end
    return error("variables not implemented")
end

pl_eval(pl_parse("1"))
pl_eval(pl_parse("a"))
pl_eval(pl_parse("bb"))
pl_eval(pl_parse(" bb "))
pl_eval(pl_parse("(+ 1 3)"))
pl_eval(pl_parse("()"))
pl_eval(pl_parse("(? (lt 1 3) \"yes\" \"no\")"))
pl_eval(pl_parse("(print 1 2 3)"))
pl_eval(pl_parse("(a(lt 1 3))"))
