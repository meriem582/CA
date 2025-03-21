function varargFunc(...)
    local args = {...}  -- VARARG
    return args[1], args[2]
end
print(varargFunc(100, 200))
