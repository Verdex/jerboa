

function to_eval_string(data)
    if type(data) == "string" then
        return string.format( [["%s"]], data )
    elseif type(data) == "number" then
        return string.format( [[%f]], data )
    elseif type(data) == "boolean" and data == true then
        return "true"
    elseif type(data) == "boolean" and data == false then
        return "false"
    elseif type(data) == "table" then
        local kvp = {}
        for k, v in pairs( data ) do
            if type(k) == "number" then
                kvp[#kvp+1] = to_eval_string( v )
            elseif type(k) == "string" then
                kvp[#kvp+1] = string.format( [[ ["%s"] = %s ]], k, to_eval_string( v ) )
            else
                error( "encountered non string key" )
            end
        end
        return "{ " .. table.concat( kvp, ", " ) .. " }"
    else 
        error( "encountered unsupported type: " .. type(data) )
    end
end

function lex(str)
    -- rules : [ { pattern, constructor } ]
    local match = string.match
    local sub = string.sub
    local rules = { { pattern = "%s+"
                    , constructor = function(m) return {t = 'data', k = 'atom', name = 'space'} end}
                  ; { pattern = "%d+"
                    , constructor = function(m) return { t = 'data'
                                                       , k = 'fun'
                                                       , name = 'number' 
                                                       , params = { { t = 'data', k = 'string', value = m } }
                                                       } end }
                  ; { pattern = "%w+"
                    , constructor = function(m) return { t = 'data' 
                                                       , k = 'fun' 
                                                       , name = 'symbol'
                                                       , params = { { t = 'data', k = 'string', value = m } }
                                                       } end }
                  }
    local output = {}
    local index = 1
    while index <= #str do
        for _, rule in ipairs( rules ) do
            local m = match( sub(str, index), "^" .. rule.pattern )
            if m then
                local o = rule.constructor(m)
                o.start_index = index
                index = index + #m 
                o.end_index = index
                output[#output+1] = o 
                goto ok 
            end
        end
        do
            return { success = false
                   , tokens = output
                   , unknown_token = sub(str, index, index)
                   , index = index
                   }
        end

        ::ok::
    end

    return {success = true, tokens = output}
end

function merge_env(e1, e2)
    local e3 = {}
    for k, v in pairs(e1) do
        e3[k] = v
    end
    for k, v in pairs(e2) do
        assert(not e3[k], 're-introducing variables not currently supported: ' .. k)
        e3[k] = v
    end
    return e3
end

function pattern_match(pattern, tokens, index, rules)
    local pi = 1
    local captures = {}
    local env = {}
    while index <= #tokens and pi <= #pattern  do
        assert(pattern[pi])
        assert(pattern[pi].t == 'pattern')
        assert(tokens[index])
        assert(tokens[index].t == 'data')
        if pattern[pi].k == 'atom' then
            if tokens[index].k ~= 'atom' then
                return false
            end
            captures[#captures+1] = tokens[index]
            pi = pi + 1
            index = index + 1
        elseif pattern[pi].k == 'fun' then
            if tokens[index].k ~= 'fun' then
                return false
            end
            if tokens[index].name ~= pattern[pi].name then
                return false
            end
            local success, _, new_env, _ = pattern_match(pattern[pi].params, tokens[index].params, 1, rules) 
            if not success then
                return false
            end
            captures[#captures+1] = tokens[index]
            env = merge_env(env, new_env)
            pi = pi + 1
            index = index + 1
        elseif pattern[pi].k == 'wild' then
            captures[#captures+1] = tokens[index]
            pi = pi + 1
            index = index + 1
        elseif pattern[pi].k == 'var' then
            assert(not env[pattern[pi].name], 're-introducing variables not currently supported: ' .. pattern[pi].name)
            env[pattern[pi].name] = tokens[index] 
            captures[#captures+1] = tokens[index]
            pi = pi + 1
            index = index + 1
        elseif pattern[pi].k == 'rule' then
            local o, next_index = parse_rule(tokens, index, rules, pattern[pi].name)
            if not o then
                return false
            end
            captures[#captures+1] = o
            pi = pi + 1
            index = next_index
        else
            error('Encountered unknown pattern type: ' .. pattern[pi].k) 
        end
    end

    if pi > #pattern then
        return true, captures, env, index
    else
        return false
    end
end

function parse_rule(tokens, index, rules, rule)
    local r = rules[rule]
    assert(r, 'Attempted to select rule: ' .. rule .. ' but found nothing' )
    
    for _, option in ipairs( r ) do
        local success, captures, env, next_index = pattern_match(option.pattern, tokens, index, rules)
        if success then
            local o = option.constructor(captures, env)
            o.start_index = captures[1].start_index 
            o.end_index = captures[#captures].end_index 
            return o, next_index
        end
    end

    return false, index
end

function parse(tokens)
    -- rules : dict<rule : string, [{ pattern, constructor}]>
    -- constructor : ([data], var_env) -> data
    local rules = { main = { { pattern = { { t = 'pattern'
                                           , k = 'fun'
                                           , name = 'symbol'
                                           , params = { { t = 'pattern', k = 'var', name = 'A' } }
                                           } 
                                           ; 
                                           { t = 'pattern'
                                           , k = 'atom'
                                           , name = 'space'
                                           } 
                                           ;
                                           { t = 'pattern'
                                           , k = 'fun'
                                           , name = 'number'
                                           , params = { { t = 'pattern', k = 'var', name = 'B' } }
                                           }
                                         }
                             , constructor = function(caps, env) return { t = 'data'
                                                                        , k = 'string'
                                                                        , value = env['A'].value .. ' ' .. env['B'].value

                                                                        } end
                             } 
                           }
                  }
   
    local output, index = parse_rule(tokens, 1, rules, 'main')  

    if not output then
        return false, index
    end

    return output 
end


--[[
data { t = 'data'
     ; k = 'atom' | 'fun' | 'string'
     ; name = string if atom or fun 
     ; params = [data] if fun
     ; value = string if string
     ; start_index = number
     ; end_index = number
     }

pattern { t = 'pattern'
        ; k = 'atom' | 'fun' | 'wild' | 'var' | 'rule'
        ; name = string if rule or var or atom or fun
        ; params = [pattern] if fun
        }

lex blah {
    "regex_string" => Data(_);
    ...
}

parse blah {
    rule: 
        atom fun(_) fun(B, Var) $rule fun2(Other, Var, atom2, fun3(A))
        => Blah($1, atom, fun(atom), Var, A) // $1 would refer to whatever is in position 1) 
        | blah => Blah();

}
--]]

