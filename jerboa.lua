

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

--[[
lex blah {
    "regex_string" => Data($1, $2);
    ...
}

--]]

