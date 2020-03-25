function merge_four_layer(dict1,dict2)
    key1 = collect(keys(dict2))[1]
    key2 = collect(keys(dict2[key1]))[1]
    key3 = collect(keys(dict2[key1][key2]))[1]
    key4 = collect(keys(dict2[key1][key2][key3]))[1]
    print("hi")
    if haskey(dict1,key1)
        if haskey(dict1[key1], key2)
            if haskey(dict1[key1][key2],key3)
                dict1[key1][key2][key3][key4] = dict2[key1][key2][key3][key4]
            else
                dict1[key1][key2][key3] = dict2[key1][key2][key3]
            end
        else
            dict1[key1][key2] = dict2[key1][key2]
        end
    else
        merge!(dict1,dict2)
    end

    return(dict1)

end
