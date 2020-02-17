function hillclimbing(eng, fre, a, jp, dict, align, fert, null)
    # Purpose:  Search the local area for the most probable alignment
    # Inputs :  eng - the english sentence
    #           fre - the french sentence
    #           a - the starting point we use the best IBM2 aligment for this
    #           jp - the pegged alignment point
    #           dict - dictionary of translation probabilities
    #           align - dictionary of alignment probabilites
    #           fert - dictionary of fertility probabilites
    #           null - the probability of null insertion
    # Outputs:  a - the most probable alignment in the locality of our seed alignment
    lens = [length(eng), length(fre)]

    # boolean to determine when we converge
    go = true

    # loop until convergence
    while go == true
        a_old = copy(a)
        neighbors = neighboring(a, jp, lens)
        for n in neighbors

            # Check if the latest option has a greater probability
            if prob(eng,fre,n,dict,align,fert, null) > prob(eng,fre,a,dict,align,fert, null)
                a = n
            end
        end

        # if we've looped through with no change end loop
        if a == a_old
           go = false
        end
   end

   return(a)
end


function neighboring(a, jp, lens)
    # Purpose:  Generate the alignments neighboring the input alignment
    # Inputs :  a - the most probable alignment under the IBM 2 model
    #           jp - the pegged alignment point
    #           lens - the length of the english and french sentences
    # Outputs:  N - a set of all the neighbours of a

    N = []
    push!(N,a)
    notpegged = setdiff(1:lens[1], jp)
    for j in notpegged
        for i in 1:lens[2]
            #println([j,i])
            a_new = copy(a)
            a_new[j] = i
            push!(N,a_new)
        end
    end
    for j1 in notpegged
        for j2 in setdiff(notpegged, j1)
            a_new = copy(a)
            a_new[j1] = a[j2]
            a_new[j2] = a[j1]
            push!(N, a_new)
        end
    end
    #println(N)
    return(N)
end


function sample(eng, fre, dict, align,fert,null)
    # Purpose:  Produces a sample from the space of alignment functions
    # Inputs :  eng - English sentence broken into tokens
    #           fre - French sentence broken into tokens
    #           dict - lexical probability distribution
    #           align - aligment distribution
    #           fert - fertility distribution
    #           null - null insertion distribution
    # Outputs:  a array containing a sample of the highest probability density
    #           alignment in the alignment space

    # select the appropriate alignment distribution
    a = Dict()
    align_key = hcat(string(length(eng)),string(length(fre)))
    align_dist = align[align_key]
    A = []

    # we peg each english word in turn
    for j in 1:length(eng)
        for i in 1:length(fre)
            a[j] = i
            # Now we find the optimal alignment - using the IBM2 aligment probs
            for j2 in setdiff(1:length(eng),j)
                align_vec = align_dist[j2, :]
                probs = []
                for i2 in 1:length(fre)
                    push!(probs, dict[fre[i2]][eng[j2]])
                end
                probs = probs.*align_vec

                a[j2] = argmax(probs)
            end
            # Now we find the max alignment using hillclimbing
            lens = [length(eng),length(fre)]
            a = hillclimbing(eng,fre,a, j,dict,align,fert,null)
            # finally add the optimal aligment and it's neighbors to the sample
            append!(A, neighboring(a,j,lens))
        end
    end
    return(A)
end

function prob(eng,fre,a,dict, align, fert, null)
    # Purpose:  find the probability of a translation using the current distributions
    # Inputs :  eng - English sentence broken into tokens
    #           fre - French sentence broken into tokens
    #           dict - lexical probability distribution
    #           align - aligment distribution
    #           fert - fertility distribution
    #           null - null insertion distribution
    # Outputs:  The probability of translation - given the input distributions
    # initialise the variables to sum on
    fertility = 0
    lex_align = 0

    # choose the right alignment table
    align_key = hcat(string(length(eng)), string(length(fre)))
    align = align[align_key]
   # println("Wagger")
    # factor in fertility only if we have estimated the
    if fert != false
        #println("hello")
    # find the probability associated with fertility

        for i in 1:length(fre)
            f = length([k for (k,v) in a if v==i])
            if f in keys(fert[fre[i]])
                fertility+=log(factorial(f)*fert[fre[i]][f])
                #println(fertility)
            end

            phi = length([k for (k,v) in a if v==length(fre)])
            if length(eng)>(2*phi)
                nulls = (factorial(length(eng)-phi)/(factorial(phi)*factorial(length(eng)-2*phi)))*null[1]^phi*null[2]^(length(eng)-phi)
                fertility += log(nulls)
            else
                nulls = (factorial(phi+1)/(factorial(2*phi+1-length(eng))*factorial(length(eng)-phi)))*null[1]^phi*null[2]^(length(eng)-phi)
                fertility += log(nulls)
            end
        end

    else
        fertility = 0
    end
    #println("patter")
    # find the alignment and translation probabilities
    for j in 1:length(eng)
        #println(lex_align)
        lex = log(dict[fre[a[j]]][eng[j]]*align[j,a[j]])
        if !isinf(lex) & !isnan(lex)
            lex_align += lex
        end
    end

    # Return the probability
    prob = MathConstants.e^(lex_align+fertility)
    if prob < 0
        println(prob)
    end

    return(prob)

end
