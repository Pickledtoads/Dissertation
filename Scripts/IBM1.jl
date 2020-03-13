# After initialisation sum translation probabilities
function CountP(y, x, tDict)
    # Purpose:  Compile the probability counts for a sentence pair
    # Inputs :  y - the English sentence
    #           x - the French sentence
    #           tDict - a dictionary containing the current translation probabilites
    # Outputs:  a dictionary containing the needed counts

    # Split the English sentence into words and add a null token
    sent = Sent_Split(y, x)
    e = sent[1]
    f = sent[2]

    s_tot = Dict(e .=> [0.0]*length(e))

    # for each english/french pair that appears record the current translation
    # probabilty in a tDictionary
    for i in 1:size(f)[1]
        for j in 1:size(e)[1]
            s_tot[e[j]] += tDict[f[i]][e[j]]
        end
    end
    return(s_tot)
end


# function that carries out each iteration of the IBM1 Model
function IBM1(Eng, Fre, iter, init)
    # Purpose:  Apply CountP to each sentence in the corpus then recalculate
    #           the translation probabilities. We do this until convergence.
    # Inputs :  Eng - Ordered array of all the English sentences
    #           Fre - Ordered array of all the French sentences
    #           iter - how many iterations of the model to use
    #           init - a dictionary containing the results of intializing the
    #           lexical distribution
    # Outputs:  A dictionary with the latest estimate for the lexical distribution

    new_tDict = copy(init)

    # run the algorithm iter times

    for j in 1:iter

        # create a zeroed copy of the translation dictionary
        tDict = zero_dict(copy(init))

        # for each sentence record the sum of translation probabilites
        # for each pair of words
        total = Dict(collect(keys(new_tDict)) .=> [0.0]*length(keys(new_tDict)))
        for i in 1:length(Eng)
            
            # call CountP to produce counts for sentence i
            SentCount = CountP(Eng[i], Fre[i], new_tDict)

            # break up the strings into arrays
            sent = Sent_Split(Eng[i], Fre[i])
            en = sent[1]
            fr = sent[2]

            # Iterate through each word in the French sentence
            for f in 1:length(fr)

                # Iterate through each word in the English sentence
                for e in 1:length(en)
                    french = string(fr[f])
                    english = string(en[e])
                    # for the selected French-English pair work out a
                    # weighted probability of translation
                    newval = new_tDict[french][english]/SentCount[english]

                    # add newval to the relevant location of the lexical distribution
                    tDict[french][english] += newval
                    total[french] += newval
                end
            end
        end

        # iterate through each entry in the dictionary to normalise them
        for f in collect(keys(tDict))
                new = collect(values(tDict[f]))./total[f]
                new = Dict(collect(keys(tDict[f])) .=> new)
                tDict[f] = new
        end
        new_tDict = tDict
    end

    return(new_tDict)
end
