a = 0 -- c0
a = 1 -- c1_0
-- c1_1
-- c1_2
a = 2

-- c2
a = 3

--[[ c3_0 -- c3_1 ]]--
a = 4

b = {"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"} -- t0
b = {
    0, -- t1
}
b = {
    0, -- t2_0
    1, -- t2_1
}
b = {
    -- t3_0
    0,
    -- t3_1
    1,
}
b = {
    --[[ t4_0 ]]--
    0, --[[ t4_1 ]]--
}

function f()
    -- f0_0
    a = 0
    -- f0_1
    a = 1
    -- f0_2
end

function f()
    -- f1
end

function f() -- f2
end

function f() --[[ f3_0 ]]-- a = 0 --[[ f3_1 ]]-- end
