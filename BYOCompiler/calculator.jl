using JSON

struct LoopBreak
end
struct LoopContinue
end
struct FuncReturn
    val
end

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
    while true
        save = idx
        while idx <= length(s) && isspace(s[idx])
            idx += 1
        end
        if idx <= length(s) && s[idx] == ';'
            while idx <= length(s) && s[idx] != '\n'
                idx += 1
            end
        end
        if idx == save
            break
        end
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
pl_parse("(a(lt 1 3)); abc")
try
    pl_parse("(+ 1 2) \n (+ 1 2)")
catch e
    return e
end
pl_parse("(+ 1 \n 3)")
function pl_eval(env, node)
    isnothing(node) && return nothing
    #=     println(node)
        println(typeof(node)) =#
    if length(node) == 0
        error("empty list")
    end
    if length(node) == 2 && node[1] == "val"
        return node[2]
    end
    if node[1] == "do"
        new_env = (Dict(), env)
        for val in node[2:end-1]
            ret = pl_eval(new_env, val)
            if ret == LoopContinue() || ret == LoopBreak() || ret isa FuncReturn
                return ret
            end
        end
        return pl_eval(new_env, node[end])
    end
    binops = Dict("+" => +, "-" => -, "*" => *, "/" => /, "eq" => ==, "ne" => !=, "ge" => >=, "le" => <=, "lt" => <, "gt" => >, "and" => &, "or" => |)
    if length(node) == 3 && node[1] in keys(binops)
        op = binops[node[1]]
        return op(pl_eval(env, node[2]), pl_eval(env, node[3]))
    end
    unops = Dict("-" => -, "not" => !)
    if length(node) == 2 && node[1] in keys(unops)
        op = unops[node[1]]
        return op(pl_eval(env, node[2]))
    end
    if length(node) in (3, 4) && node[1] in ("if", "?")
        if length(node) == 3
            _, cond, yes = node
            no = nothing
        else
            _, cond, yes, no = node
        end
        new_env = (Dict(), env)
        if pl_eval(env, cond)
            return pl_eval(env, yes)
        else
            return pl_eval(env, no)
        end
    end
    if node[1] == "loop" && length(node) == 3
        _, cond, body = node
        ret = nothing
        while true
            new_env = (Dict(), env)
            if !pl_eval(new_env, cond)
                break
            end
            ret = pl_eval(new_env, body)
            if ret == LoopContinue()
                continue
            elseif ret == LoopBreak()
                break
            elseif ret isa FuncReturn
                break
            end
        end
        return ret
    end
    if node[1] == "def" && length(node) == 4
        _, name, args, body = node
        # sanity checks
        for arg_name in args
            if !(typeof(arg_name) == String)
                error("bad argument name")
            end
        end
        if length(args) != length(Set(args))
            error("duplicated arguments")
        end
        # add the function to the scope
        dct, _ = env
        key = (name, length(args))
        if key in keys(dct)
            error("duplicated function")
        end
        dct[key] = (args, body, env)
        return nothing
    end
    if node[1] == "call" && length(node) >= 2
        _, name, args... = node
        key = (name, length(args))
        fargs, fbody, fenv = name_loopup(env, key)[key]
        new_env = Dict()
        for (arg_name, arg_val) in zip(fargs, args)
            new_env[arg_name] = pl_eval(env, arg_val)
        end
        val = pl_eval((new_env, fenv), fbody)
        if val isa FuncReturn
            return val.val
        else
            return val
        end
    end
    if node[1] == "print"
        for val in node[2:end]
            println((pl_eval(env, val)))
        end
        return nothing
    end
    if node[1] == "var" && length(node) == 3
        _, name, val = node
        scope, _ = env
        if name in keys(scope)
            error("duplicated name")
        end
        val = pl_eval(env, val)
        scope[name] = val
        return val
    end
    if node[1] == "set" && length(node) == 3
        _, name, val = node
        scope = name_loopup(env, name)
        val = pl_eval(env, val)
        scope[name] = val
        return val
    end
    if node == "break"
        return LoopBreak()
    end
    if node == "continue"
        return LoopContinue()
    end
    if node == "return"
        return FuncReturn(nothing)
    end
    if node[1] == "return" && length(node) == 2
        _, val = node
        return FuncReturn(pl_eval(env, val))
    end
    return name_loopup(env, node)[node]
end
function name_loopup(env, key)
    while !isnothing(env)
        current, env = env
        if key in keys(current)
            return current
        end
    end
    error("undefined variable")
end

pl_eval(nothing, pl_parse("1"))
pl_eval(nothing, pl_parse("(+ 1 3)"))
pl_eval(nothing, pl_parse("(? (lt 1 3) \"yes\" \"no\")"))
pl_eval(nothing, pl_parse("(print 1 2 3)"))
pl_eval(nothing, pl_parse("(do 2 (+ 1 2) (+ 1 3))"))


function pl_parse_prog(s::String)
    return pl_parse("(do " * s * ")")
end
println("STARTER")
pl_parse_prog("""
2
(+ 1 2)
(+ 1 3)
""")
s = """
;; first scope
(var a 1)
(var b (+ a 1))
;; a=1, b=2
(do
    ;; new scope
    (var a (+ b 5))     ;; name collision
    (set b (+ a 10))
)
;; a=1, b=17
(* a b)
"""
@assert pl_eval(nothing, pl_parse_prog(s)) == 17


s = """
(if true 1 2)
"""
pl_eval(nothing, pl_parse_prog(s))
s = """
(if false 1 2)
"""
pl_eval(nothing, pl_parse_prog(s))
s = """
(? false 1 2)
"""
pl_eval(nothing, pl_parse_prog(s))
s = """
(if true 1)
"""
pl_eval(nothing, pl_parse_prog(s))
s = """
(if false 1)
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
(var n 10)
(loop (gt n 0) (do
                (print n)
                (set n (- n 1))
               )
)
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
(var n 10)
(var m 100)
(loop (gt n 0) (do
                (var m 1000)
                (set m (- m 1))
                (print n)
                (print m)
                (set n (- n 1))
               )
)
(print m)
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
(var n 10)
(loop (gt n 0) (do
                (print n)
                (if (eq n 5) break)
                (set n (- n 1))
               )
)
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
(var n 10)
(loop (gt n 0) (do
                (print n)
                (set n (- n 1))
                (if (eq n 5) continue)
                (set n (- n 1))
               )
)
"""
pl_eval(nothing, pl_parse_prog(s))


s = """
(def f (n) (- n 1))
(call f 10)
"""
pl_eval(nothing, pl_parse_prog(s))


s = """
(def f (n) (do (- n 2) (return (- n 1)) (- n 3)))
(call f 10)
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
(def f (n) (do (- n 2) (return n) (- n 3)))
(call f 10)
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
(def f (n) (do (- n 2) return (- n 3)))
(call f 10)
"""
pl_eval(nothing, pl_parse_prog(s))


s = """
(def f (n) (loop (gt n 0) (do
                            (print n)
                            (if (eq n 5) (return n))
                            (set n (- n 1))
                            )
            )
)
(+ (call f 10) 10)
"""
a = pl_eval(nothing, pl_parse_prog(s))

s = """
(var n 100)
(def f (n) (do (set n 10) (print n)))
(call f n)
(print n)
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
(var n 100)
(def f (m) (+ m n))
(call f 10)
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
(var n 100)
(def f (m) (+ m n))
(do (var n 1000) (call f 10))
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
(var n 100)
(def f (m) (+ m n))
(do (var n 1000) (def f (m) (+ m n)) (call f 10))
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
(var n 100)
(def f (m) (+ m n))
(set n 200)
(do (var n 1000) (call f 10))
"""
pl_eval(nothing, pl_parse_prog(s))

s = """
        (def fib (n)
            (if (le n 0)
                0
                (+ n (call fib (- n 1)))))
        (call fib 5)
"""
pl_eval(nothing, pl_parse_prog(s)) == 5 + 4 + 3 + 2 + 1

s = """
        (def fib (n) (do
            (var r 0)
            (loop (gt n 0) (do
                (set r (+ r n))
                (set n (- n 1))
            ))
            (return r)
        ))
        (call fib 5)
"""
pl_eval(nothing, pl_parse_prog(s)) == 5 + 4 + 3 + 2 + 1
