a = ret_by_value.get_test();
if (a.myInt ~= 100)
    error
end

if (a.myShort ~= 200)
    error
end
