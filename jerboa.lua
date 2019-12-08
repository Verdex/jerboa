

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

function lex(str)
    -- rules : [ { pattern, constructor } ]
    local match = string.match
    local sub = string.sub
    local rules = { {pattern = "%s+", constructor = function(m) return "space" end}
                  ; {pattern = "%d+", constructor = function(m) return "number " .. m end}
                  } 
    local output = {}
    local index = 1
    while index <= #str do
        for _, rule in ipairs( rules ) do
            local m = match( sub(str, index), "^" .. rule.pattern )
            if m then
                output[#output+1] = rule.constructor( m ) 
                print( output[#output+1], #m )
                index = index + #m 
                goto done 
            end
        end
        error("encountered unknown token : " .. sub(str, index, index) .. " at: " .. index)
        ::done::
    end
    return output
end

--[[
lex blah {
    "regex_string" => Data($1, $2);
    ...
}

--]]

