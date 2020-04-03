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

            # if there are a *small* number of null tokens
            if length(eng)>(2*phi)
                N = length(eng)-phi
                x = phi
                N_x = N-x

                # Use stirling's approximation for numerical efficiency
                numerator = 0.5*log(2*MathConstants.pi*N)+N*log(N/MathConstants.e)

                # case where fertility is zero and where it isn't
                if x != 0
                    denominator = 0.5*log(2*MathConstants.pi*x)+0.5*log(2*MathConstants.pi*N_x)+x*log(x/MathConstants.e)+N_x*log(N_x/MathConstants.e)
                elseif x == 0
                    denominator = 0.5*log(2*MathConstants.pi*N_x)+N_x*log(N_x/MathConstants.e)
                end


                nulls = (numerator-denominator)+phi*log(null[1])+N_x*log(null[2])
                fertility += MathConstants.e^nulls
            # if there are lots of null tokens the "choose" term needs
            # to be found differently
            else

                N = phi+1
                x = 2*phi+1-length(eng)
                N_x = N-x

                # Use stirling's approximation for numerical efficiency
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
        # find all enlgish words that map to the ith french word
        maps_to = sort([k for (k,v) in a if v==i])
        fert = length(maps_to)
        # maximum number of vacencies in the translated sentence
        vacmax = length(Filled)
        if fert>0
            for words in maps_to

                # count number of vacencies to centre of last cept & current eng word position
                no_vac_to_cept = length(findall(x->x=="", Filled[1:last_cept]))
                no_vac_to_pos = length(findall(x->x=="", Filled[1:words]))

                # if this is not the first word in a cept consider only vacencies after the previous word
                # in cept
                vacmax2 = vacmax
                if maps_to[1]!=words                                                                        #
                    vacmax2 = length(Filled[findall(x->x=="",Filled[last:length(Filled)])])
                    no_vac_to_pos = no_vac_to_pos-length(Filled[findall(x->x=="",Filled[1:maps_to[1]])])
                end

                # if possible increment the count
                try
                    new = sum(log.(align[fert][vacmax2][no_vac_to_cept][no_vac_to_pos]))
                    alignment += MathConstants.e^new
                catch

                end
                #remove the vacency we've just filled and keep track of the last position filled
                last = words
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

    init = Dict("trans"=>copy(init["trans"]),"align"=>copy(init["align"]),
                "fert"=>copy(init["fert"]), "null"=>copy(init["null"]))

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
            sent = Sent_Split(Eng[s],Fre[s])
            eng = sent[1]
            fre = sent[2]

            # generate a large sample of highest density alignments
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

            # iterate through each of the generated alignments
            for a in A
                null = 0

                # need to use a different prob expression after the first iteration
                if it == 1
                    # determine the increment for this alignment
                    c = prob_IBM4(eng, fre, a, init["trans"],init["align"], init["fert"], init["null"])/c_tot
                else
                    # determine the increment for this alignment
                    c = prob_IBM5(eng,fre,a,init["trans"],init["align"], init["fert"], init["null"])/c_tot
                end

                # determine the lexical counts
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

                    # total number of vacencies
                    vacmax = length(Filled)
                    for f in 1:length(fre)
                        # find the locations of the eng word that map to the ith fre word
                        maps_to = sort([k for (k,v) in a if v==f])
                        fert = length(maps_to)
                        if fert > 0
                            for e in 1:fert

                                # if this is the first word in the cept
                                if e == 1

                                    # find the number of vacencies up to the last cept's centre
                                    if last_cept == 0
                                        vac_cept = 0
                                    else
                                        vac_cept = length(findall(x->x=="", Filled[1:last_cept]))
                                    end

                                    # Number of vacencies up to the current word
                                    vac_current = length(findall(x->x=="", Filled[1:maps_to[e]]))
                                    Filled[maps_to[e]] = "flibber"

                                    new = Dict(fert=>Dict(vacmax=>Dict(vac_cept=>Dict(vac_current=>c))))
                                    count_d = merge_four_layer(count_d, new)

                                # if this is a word after the first in a cept
                                else
                                    # where did the preceeding word map to
                                    current = maps_to[e-1]

                                    # how many vacencies after the last word in cept
                                    vacmax_adj = convert(Int,length(findall(x->x=="",
                                                         Filled[current:length(Filled)])))

                                    vac_cept = length(findall(x->x=="",
                                                      Filled[1:last_cept]))

                                    vac_current = length(findall(x->x=="",
                                                         Filled[current:maps_to[e]]))

                                    # fill the correct vacency and iterate the correct count
                                    Filled[maps_to[e]] = "flibber"
                                    new = Dict(fert=>Dict(vacmax_adj=>Dict(vac_cept=>Dict(vac_current=>c))))
                                    count_d = merge_four_layer(count_d, new)
                                end

                                # find the centre of this cept & deincrement vacmax
                                last_cept = convert(Int,ceil(mean(maps_to)))
                                vacmax = vacmax-1
                            end
                        end

                    end

                # increment the null insertion probabilities
                if !isnan(null*c) & !isnan((length(eng)-2*null)*c)
                    count_p1 += null*c
                    count_p0 += abs(length(eng)-2*null)*c
                end

                # calculate the fertlity counts
                for f in 1:length(fre)
                    fertility = 0

                    # find the number of english words that map to each french word
                    for e in 1:length(eng)
                        if f == a[e]
                                fertility += 1
                        end
                    end
                    temp = Dict(fertility => c)
                    count_f[fre[f]]= merge(+,count_f[fre[f]], temp)
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

        # Normalise the alignment distribution
        for k1 in keys(count_d)
            for k2 in keys(count_d[k1])
                for k3 in keys(count_d[k1][k2])
                    for k4 in keys(count_d[k1][k2][k3])
                        new = count_d[k1][k2][k3][k4]/sum(values(count_d[k1][k2][k3]))
                        new = Dict(k1=>Dict(k2=>Dict(k3=>Dict(k4=>new))))
                        alignments = merge_four_layer(alignments,new)
                    end
                end
            end
        end
        # Normalise the fertility distribution
        for f in collect(keys(fertilities))
            normed_vals = values(fertilities[f])./ sum(values(fertilities[f]))
            fertilities[f] = Dict(keys(fertilities[f]).=> normed_vals)
        end

        # Recalculate the null insertion probabilities
        p1 = count_p1/(count_p1+count_p0)
        p0 = 1 - p1

        # redefine the lastest state of the model
        init = Dict("trans"=> copy(Translation_Dict),"align"=>copy(alignments),
                    "fert"=>copy(fertilities), "null" => [p1,p0])

    end
    return(init)
end
