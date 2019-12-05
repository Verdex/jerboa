

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
        local number, end_pos = match(sub(str, index), "(%d+)()")
        index = end_pos 
        clear_whitespace()
        if top_level and index <= #str then
            error( "found additional characters")
        elseif top_level then
            return { t = 'data', k = 'cap', number = tonumber(number) }
        end
        return { t = 'data', k = 'cap', number = tonumber(number) }, index
    elseif match( sub(str, index, index), "%u" ) then
        local name, end_pos = match(sub(str, index), "(%w+)()")
        index = end_pos 
        clear_whitespace()
        if top_level and index <= #str then
            error( "found additional characters")
        elseif top_level then
            return { t = 'data', k = 'var', name = name }
        end
        return { t = 'data', k = 'var', name = name }, index
    elseif match( sub(str, index, index), "%l" ) then
        local name, end_pos = match(sub(str, index), "(%w+)()")
        index = end_pos 
        clear_whitespace()
        if index > #str then
            return { t = 'data', k = 'sym', name = name }, index
        end
        if sub(str, index, index) == "(" then
            local params = {}
            while sub(str, index, index) ~= ")" do
                params[#params+1], index = parse_constructor(str, index + 1)
                local n = sub(str, index, index)
                if n ~= ',' and n ~= ')' then
                    error( "expected comma in param list" )
                end
            end
            clear_whitespace()
            if top_level and index <= #str then
                error( "found additional characters")
            elseif top_level then
                return { t = 'data', k = 'fun', name = name, params = params }
            end
            return { t = 'data', k = 'fun', name = name, params = params }, index
        end 
        error( "found additional characters")
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


