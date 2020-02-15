function IBM2_Tranz(Fre, tDict, aDict)
    # Purpose:  Apply the IBM2 model to a french sentence to produce a translation
    # Inputs :  Fre - the French sentence needing translation
    #           tDict - the translation dictionary
    #           aDict - the dictionary of alignment probabilities
    # Outputs:  the translated sentence in English

    # Split the French sentence into words
    fr = split(Fre, " ")
    fr = convert(Array,fr)
    
    # add in a null token
    append!(fr,["null"])
    fr_len = length(fr)

    translation_info = []

    # Determine the English sentence with maximum probability
    for e in 1:length(fr)
        # find the most probable alignment
        align_key = hcat(string(fr_len),string(fr_len))
        alignment = argmax(aDict[align_key][e,:])
        ind = argmax(collect(values(tDict[fr[alignment]])))

        # find the most probable English word for the French word
        word = collect(keys(tDict[fr[alignment]]))[ind]
        push!(translation_info, word)
    end
    return(translation_info)
end
