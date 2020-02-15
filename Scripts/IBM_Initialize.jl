function InitT(y, x)
    # Purpose:  Compiles initial counts for a pair of sentences
    # Inputs :  y - the English sentence
    #           x - the French sentence
    # Outputs:  a dictionary containing the needed counts

    # split the english sentence into words
    sent = Sent_Split(y, x)
    e = sent[1]
    f = sent[2]

    # Initialise a dictionary of zeros for storing counts
    s_tot = Dict(f.=> fill(Dict(e.=>zeros(size(e)[1])),size(f)[1]))

    # For each time a word pair appears, add to the translation count
    for i in 1:size(f)[1]
        for j in 1:size(e)[1]
            s_tot[f[i]][e[j]] += 1
        end
    end

    # return the count of translation counts
    return(s_tot)
end

# Iterate through a list of training sentences and initalise the translation dictionary
function Initialize(Eng, Fre)
    # Purpose:  Run the fist iteration of the IBM1 and construct the translation dictionary
    # Inputs :  Eng - ordered array containing all english sentences
    #           Fre - ordered array containing all French sentences
    # Outputs:  A dictionary containing the translation probabilites output
    #           resulting from the first IBM1 iteration
    Translation_Dict = Dict()

    # For each sentence calculate the sum of word by word translation probabilites
    for i in 1:size(Eng)[1]

        # call the InitT function
        SentCount = InitT(Eng[i], Fre[i])

        # merge the new counts into the dictionary of counts
        merge!(merger_plus,Translation_Dict, SentCount)
    end

    newProbs = Translation_Dict

    # For each french word in the dictionary normalise its probability of being
    # translated as each English word
    for f in keys(newProbs)
        f_total = 0

        # Each of word f take its reciprical and
        # sum all of the recipricals
        for e in keys(newProbs[f])
            new = Translation_Dict[f][e]
            newProbs[f][e] = new
            f_total += new
        end

        # normalise the translation probabilites
        vals = values(newProbs[f])./f_total

        # recast the array as a dictionary
        newProbs[f] = Dict(keys(newProbs[f]).=> vals)

    end

    # return the latest iteration of translation probabilities
    return(newProbs)
end
