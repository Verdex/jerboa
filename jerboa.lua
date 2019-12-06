

function lua_data_to_string(data)
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
                kvp[#kvp+1] = lua_data_to_string( v )
            elseif type(k) == "string" then
                kvp[#kvp+1] = string.format( [[ ["%s"] = %s ]], k, lua_data_to_string( v ) )
            else
                error( "encountered non string key" )
            end
        end
        return "{ " .. table.concat( kvp, ", " ) .. " }"
    else 
        error( "encountered unsupported type: " .. type(data) )
    end
end

-- $1, Var, symbol, symbol(<<paramList>>), <<paramList>> = this
function parse_constructor(str, index)
    local top_level = not index
    local sub = string.sub 
    local match = string.match
    index = index or 1
    function clear_whitespace()
        while match( sub(str, index, index), "%s" ) do
            index = index + 1
        end
    end

    clear_whitespace()
    if sub(str, index, index) == "$" then
        local number = match(sub(str, index), "(%d+)")
        index = #number + index + 1
        clear_whitespace()
        if top_level and index <= #str then
            error( "found additional characters")
        elseif top_level then
            return { t = 'data', k = 'cap', number = tonumber(number) }
        end
        return { t = 'data', k = 'cap', number = tonumber(number) }, index
    elseif match( sub(str, index, index), "%u" ) then
        local name = match(sub(str, index), "(%w+)")
        index = #name + index
        clear_whitespace()
        if top_level and index <= #str then
            error( "found additional characters")
        elseif top_level then
            return { t = 'data', k = 'var', name = name }
        end
        return { t = 'data', k = 'var', name = name }, index
    elseif match( sub(str, index, index), "%l" ) then
        local name = match(sub(str, index), "(%w+)")
        index = #name + index 
        clear_whitespace()
        if index > #str then
            return { t = 'data', k = 'sym', name = name }
        elseif sub(str, index, index) == "(" then
            index = index + 1
            local params = {}
            while sub(str, index, index) ~= ")" do
                params[#params+1], index = parse_constructor(str, index)
                clear_whitespace()
                local n = sub(str, index, index)
                if n ~= ',' and n ~= ')' then
                    error( "expected comma in param list" )
                end
                index = index + 1 -- skip the , character
            end
            clear_whitespace()
            index = index + 1 -- skip the ) character
            if top_level and index <= #str then
                error( "found additional characters")
            elseif top_level then
                return { t = 'data', k = 'fun', name = name, params = params }
            end
            return { t = 'data', k = 'fun', name = name, params = params }, index
        elseif not top_level then
            return { t = 'data', k = 'sym', name = name }, index
        end
        error( "found additional characters : " .. sub(str, index, index) .. " : " .. index)
    else
        error( "Encountered unknown data element: " .. sub( str, index, index ) .. " : " .. index )
    end
end
--[[
lex blah {
    "regex_string" => Data($1, $2);
    ...
}

--]]

function test_parse_constructor()
    local solo_var, index = parse_constructor("Variable") 
    assert(not index)
    assert(solo_var)
    assert(solo_var.k == 'var')
    assert(solo_var.name == 'Variable')

    local solo_cap, index = parse_constructor("$123")
    assert(not index)
    assert(solo_cap)
    assert(solo_cap.k == 'cap')
    assert(solo_cap.number == 123)

    local solo_sym, index = parse_constructor("symbol")
    assert(not index)
    assert(solo_sym)
    assert(solo_sym.k == 'sym')
    assert(solo_sym.name == 'symbol')

    local solo_fun, index = parse_constructor("fun()")
    assert(not index)
    assert(solo_fun)
    assert(solo_fun.k == 'fun')
    assert(solo_fun.name == 'fun')
    assert(solo_fun.params)
    assert(#solo_fun.params == 0)

    local complex, index = parse_constructor("fun ( $1 , Var , sym , fun2 ( sym ) )")
    assert(not index)
    assert(complex)
    assert(complex.k == 'fun')
    assert(complex.name == 'fun')
    assert(complex.params)
    assert(#complex.params == 4)
    assert(complex.params[1].k == 'cap')
    assert(complex.params[1].number == 1)
    assert(complex.params[2].k == 'var')
    assert(complex.params[2].name == 'Var')
    assert(complex.params[3].k == 'sym')
    assert(complex.params[3].name == 'sym')
    assert(complex.params[4].k == 'fun')
    assert(complex.params[4].name == 'fun2')
    assert(complex.params[4].params)
    assert(#complex.params[4].params == 1)
    assert(complex.params[4].params[1].k == 'sym')
    assert(complex.params[4].params[1].name == 'sym')
end
