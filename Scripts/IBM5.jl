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
    return(N)
end


# I chose to base the initial alignment on the IBM 3 alignment function
# Finding the best starting point based on the IBM 4 is too computationally intensive.
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

function prob_IBM5(eng,fre,a,dict, align, fert, null)
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
    lex = 0
    alignment = 0

    # factor in fertility only if we have estimated the
    if fert != false

    # find the probability associated with fertility
        for i in 1:length(fre)
            f = length([k for (k,v) in a if v==i])

            # Calculate the fertility probabilities
            if f in keys(fert[fre[i]])
                fertility+=MathConstants.e^(log(factorial(f)*fert[fre[i]][f]))
            end

            phi = length([k for (k,v) in a if v==length(fre)])
            # if there are lots of null tokens the "choose" term needs
            # to be found differently

            if length(eng)>(2*phi)
                N = length(eng)-phi
                x = phi
                N_x = N-x

                numerator = 0.5*log(2*MathConstants.pi*N)+N*log(N/MathConstants.e)

                if x != 0
                    denominator = 0.5*log(2*MathConstants.pi*x)+0.5*log(2*MathConstants.pi*N_x)+x*log(x/MathConstants.e)+N_x*log(N_x/MathConstants.e)
                elseif x == 0
                    denominator = 0.5*log(2*MathConstants.pi*N_x)+N_x*log(N_x/MathConstants.e)
                end


                nulls = (numerator-denominator)+phi*log(null[1])+N_x*log(null[2])
                fertility += MathConstants.e^nulls
            else
                N = phi+1
                x = 2*phi+1-length(eng)
                N_x = N-x

                numerator = 0.5*log(N)+N*log(N/MathConstants.e)
                denominator = 0.5*log(2*MathConstants.pi*x*N_x)+x*log(x/MathConstants.e)+N_x*log(N_x/MathConstants.e)

                nulls = (numerator-denominator)+phi*log(null[1])+N_x*log(null[2])
                fertility += MathConstants.e^nulls
            end
        end

    # If we have not yet found fertility probabilities
    else
        fertility = 0
    end

    # find the distortion/alignment probabilites
    last_cept = 0
    Filled = repeat([""], length(eng))
    for i in 1:length(fre)
        maps_to = sort([k for (k,v) in a if v==i])
        fert = length(maps_to)
        vacmax = length(Filled)
        if fert>0
            for words in maps_to
                no_vac_to_cept = length(findall(x->x=="", Filled[1:last_cept]))
                no_vac_to_pos = length(findall(x->x=="", Filled[1:words]))
                if maps_to[1]!=words
                    no_vac_to_pos = no_vac_to_pos-length(Filled[findall(x->x=="",Filled[1:maps_to[1]])])
                end
                try
                    new = sum(log.(align[fert][vacmax][no_vac_to_cept][no_vac_to_pos]))
                    alignment += MathConstants.e^new
                catch

                end
                Filled[words] = "word"
                vacmax -= 1
            end

            last_cept = convert(Integer,ceil(mean(maps_to)))
        end


    end

    # find the translation probabilities
    for j in 1:length(eng)
        lexic = log(dict[fre[a[j]]][eng[j]])
        if !isinf(lex) & !isnan(lex)
            lex += MathConstants.e ^ lexic
        end
    end

    # Return the probability
    prob = MathConstants.e^(log(lex)+log(fertility))
    if prob < 0
        println(prob)
    end

    return(prob)

end


using Base.Threads


function IBM5(Eng, Fre, iter, init, samp_align)
    # Purpose:  Apply the IBM3 model to the training data
    # Inputs :  Eng - ordered array of all the English sentences
    #           Fre - ordered array of all the French sentences
    #           iter - number of iterations we want to run for
    #           init - the dictionary containing the output from IBM2
    # Outputs:  A dictionary containing translation, alignment, fertility
    #           and null insertion probabilities

    init = Dict("trans"=>copy(init["trans"]),"align"=>copy(init["align"]), "fert"=>copy(init["fert"]), "null"=>copy(init["null"]))

    for it in 1:iter
        # initialise the count variables
        count_t = zero_dict(init["trans"]) # translation count
        total_t = Dict(keys(init["trans"]) .=> [0.0]*length(count_t))

        count_d = Dict()# distortion counts
        total_d = Dict()

        count_p1 = 0 # null insertio counts
        count_p0 = 0
        count_f = Dict()
        for k in keys(init["trans"])
            count_f[k] =  Dict()# fertility counts
        end

        @threads for s in 1:length(Eng)
            # split up our words
            if s%100 ==0
                println(s)
            end

            sent = Sent_Split(Eng[s],Fre[s])
            eng = sent[1]
            fre = sent[2]

            A = sample(eng, fre,init["trans"], samp_align, init["fert"], init["null"])
            c_tot = 0

            # Need to calculate this differently after the first iteration
            if it == 1
                for a in A
                    c_tot += prob(eng,fre,a,init["trans"],samp_align, init["fert"], init["null"])
                end
            else
                for a in A
                    c_tot += prob_IBM5(eng,fre,a,init["trans"],init["align"], init["fert"], init["null"])
                end
            end

            for a in A
                null = 0
                # need to use a different prob expression after the first iteration
                if it == 1
                    c = prob_IBM4(eng, fre, a, init["trans"],init["align"], init["fert"], init["null"])/c_tot
                else
                    c = prob_IBM5(eng,fre,a,init["trans"],init["align"], init["fert"], init["null"])/c_tot
                end

    
                for e in 1:length(eng)
                    count_t[fre[a[e]]][eng[e]] += c
                    total_t[fre[a[e]]] += c

                    if a[e] == (length(fre))
                        null += 1
                    end
                end
                    # we now add the count to the rel_distortion counts
                    last_cept = 0
                    Filled = repeat([""], length(eng))
                    for i in 1:length(fre)
                        maps_to = sort([k for (k,v) in a if v==i])
                        fert = length(maps_to)
                        vacmax = length(Filled)
                        if fert>0
                            for words in maps_to
                                no_vac_to_cept = length(findall(x->x=="", Filled[1:last_cept]))
                                no_vac_to_pos = length(findall(x->x=="", Filled[1:words]))
                                if maps_to[1]!=words

                                    no_vac_to_pos = no_vac_to_pos-length(findall(x->x=="", Filled[1:maps_to[1]]))
                                end
                                new = Dict(fert=>Dict(vacmax=>Dict(no_vac_to_cept=>Dict(no_vac_to_pos=>c))))
                                try
                                    count_d[fert][vacmax][no_vac_to_cept][no_vac_to_pos] += c
                                    total_d[fert][vacmax][no_vac_to_cept] += c
                                catch
                                    new = Dict(no_vac_to_pos=>c)
                                    if fert in keys(count_d)


                                        if vacmax in keys(count_d[fert])
                                            count_d[fert][vacmax] = merge(merger_plus, count_d[fert][vacmax],Dict(no_vac_to_cept=>new))
                                            total_d[fert][vacmax] = merge(+,total_d[fert][vacmax],Dict(no_vac_to_cept=>c))
                                        else
                                            new_d = Dict(vacmax=>Dict(no_vac_to_cept=>Dict(no_vac_to_pos=>c)))
                                            count_d[fert]= merge(count_d[fert],new_d)
                                            new_dt = Dict(vacmax=>Dict(no_vac_to_cept=>c))
                                            total_d[fert]=merge(total_d[fert],new_dt)
                                        end

                                    else
                                        new_d = Dict(fert=>Dict(vacmax=>Dict(no_vac_to_cept=>Dict(no_vac_to_pos=>c))))
                                        merge!(count_d,new_d)
                                        new_dt = Dict(fert=>Dict(vacmax=>Dict(no_vac_to_cept=>c)))
                                        merge!(total_d,new_dt)
                                    end
                                end

                                Filled[words] = "word;)"
                                vacmax -= 1
                            end

                            last_cept = convert(Integer,ceil(mean(maps_to)))
                        end
                    end

                if !isnan(null*c) & !isnan((length(eng)-2*null)*c)
                    count_p1 += null*c
                    count_p0 += abs(length(eng)-2*null)*c
                end
                for e in 1:length(eng)
                    fertility = 0
                    for j in 1:length(fre)
                        if j == a[e]
                            fertility += 1
                        end
                        temp = Dict(fertility => c)
                        count_f[fre[j]]= merge(+,count_f[fre[j]], temp)
                    end
                end
            end
        end
        # initialise the new distributions
        Translation_Dict = zero_dict(init["trans"])

        alignments = Dict()

        fertilities = copy(count_f)
        # recalculate the translation distribution
        for i in 1:length(Translation_Dict)
            fre = collect(keys(Translation_Dict))[i]
            @threads for j in 1:length(Translation_Dict[fre])
                eng = collect(keys(Translation_Dict[fre]))[j]
                Translation_Dict[fre][eng] = count_t[fre][eng]/ total_t[fre]
            end
        end
        # Recalculate the alignment distribution
        for k1 in keys(count_d)
            for k2 in keys(count_d[k1])
                for k3 in keys(count_d[k1][k2])
                    merge!(alignments, Dict(k1=>Dict(k2=>Dict(k3=>Dict()))))
                    alignments[k1][k2][k3] = Dict(keys(count_d[k1][k2][k3]).=>values(count_d[k1][k2][k3])./total_d[k1][k2][k3])
                end
            end
        end
        # Recalculate the fertility distribution
        for f in collect(keys(fertilities))
            normed_vals = values(fertilities[f])./ sum(values(fertilities[f]))
            fertilities[f] = Dict(keys(fertilities[f]).=> normed_vals)
        end
        p1 = count_p1/(count_p1+count_p0)
        p0 = 1 - p1
        init = Dict("trans"=> copy(Translation_Dict),"align"=>copy(alignments),"fert"=>copy(fertilities), "null" => [p1,p0])
    end
    return(init)
end
