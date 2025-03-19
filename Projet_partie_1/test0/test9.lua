Person = {}
Person.__index = Person

function Person:new(name, age)
    local self = setmetatable({}, Person)
    self.name = name
    self.age = age
    return self
end

function Person:display()
    print("Nom:", self.name, "Âge:", self.age)
end

local p = Person:new("Bob", 30)
p:display()
