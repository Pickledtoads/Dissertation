
function prob_IBM4(eng,fre,a,dict, align, fert, null)
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

    # find the probability associated with fertility
    for i in 1:length(fre)
        f = length([k for (k,v) in a if v==i])

        # Calculate the fertility probabilities
        if f in keys(fert[fre[i]])
            ferti = log(factorial(f)*fert[fre[i]][f])
            if !isnan(ferti) & !isinf(ferti)
                fertility+=MathConstants.e^ferti
            end
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
            if !isnan(nulls) & !isinf(nulls)
                fertility += MathConstants.e^nulls
            end

        # if there are lots of null tokens the "choose" term needs
        # to be found differently
        else
            N = phi+1
            x = 2*phi+1-length(eng)
            N_x = N-x

            numerator = 0.5*log(N)+N*log(N/MathConstants.e)
            denominator = 0.5*log(2*MathConstants.pi*x*N_x)+x*log(x/MathConstants.e)+N_x*log(N_x/MathConstants.e)

            nulls = (numerator-denominator)+phi*log(null[1])+N_x*log(null[2])
            if !isnan(nulls) & !isinf(nulls)
                fertility += MathConstants.e^nulls
            end
        end
    end



    # find the distortion/alignment probabilites
    last_cept = 0


    for i in 1:length(fre)

        # determine the which and how many english words map to the ith french word
        maps_to = sort([k for (k,v) in a if v==i])
        fert = length(maps_to)

        # for fertility 1 words
        if fert == 1

            # find the relative distortion
            rel_dist = i-last_cept
            try
                new = align[fert][rel_dist]
            catch
                new = 0
            end

            # increment the distortion dist
            alignment += new
            last_cept = convert(Integer,ceil(mean(maps_to)))

        # For words with fertility more than 1
        elseif fert>1

            # find the relative distortions
            rel_dist = vcat(maps_to[1]-last_cept, maps_to[2:fert]-maps_to[1:(fert-1)])
            new = []
            for e in rel_dist
                try
                    push!(new, align[convert(Integer,fert)][convert(Integer,e)])
                catch
                    #print([fert,e])
                end

            end

            # increment the distortion counts
            new = sum(log.(new))
            if !isnan(new) & !isinf(new)
                alignment += MathConstants.e^new
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
    prob = MathConstants.e^(log(lex)+log(fertility)+log(alignment))


    return(prob)
end


function IBM4(Eng, Fre, iter, init, samp_align)
    # Purpose:  Apply the IBM3 model to the training data
    # Inputs :  Eng - ordered array of all the English sentences
    #           Fre - ordered array of all the French sentences
    #           iter - number of iterations we want to run for
    #           init - the dictionary containing the output from IBM2
    # Outputs:  A dictionary containing translation, alignment, fertility
    #           and null insertion probabilities

    init = Dict("trans"=>copy(init["trans"]),"align"=>copy(init["align"]), "fert"=>false, "null"=>false)

    for it in 1:iter

        # initialise the count variables
        count_t = zero_dict(init["trans"]) # translation count
        total_t = Dict(keys(init["trans"]) .=> [0.0]*length(count_t))

        # distortion counts
        count_d = Dict()
        total_d = Dict()

        # null insertion counts
        count_p1 = 0
        count_p0 = 0
        count_f = Dict()

        # fertility counts
        for k in keys(init["trans"])
            count_f[k] =  Dict()
        end

        @threads for s in 1:length(Eng)

            # split up our words
            sent = Sent_Split(Eng[s],Fre[s])
            eng = sent[1]
            fre = sent[2]
            A = sample(eng,fre,init["trans"], samp_align, init["fert"], init["null"])
            c_tot = 0

            # Need to calculate this differently after the first iteration
            if it == 1
                for a in A
                    c_tot += prob(eng,fre,a,init["trans"],init["align"], init["fert"], init["null"])
                end
            else
                for a in A
                    c_tot += prob_IBM4(eng,fre,a,init["trans"],init["align"], init["fert"], init["null"])
                end
            end
            for a in A
                null = 0

                # need to use a different prob expression after the first iteration
                if it == 1
                    # determine the increment for this alignment
                    c = prob(eng,fre,a,init["trans"],init["align"], init["fert"], init["null"])/c_tot
                else
                    # determine the increment for this alignment
                    c = prob_IBM4(eng,fre,a,init["trans"],init["align"], init["fert"], init["null"])/c_tot
                end



                # determine the lexical counts
                for e in 1:length(eng)
                    count_t[fre[a[e]]][eng[e]] += c
                    total_t[fre[a[e]]] += c


                    if a[e] == (length(fre))
                        null += 1
                    end

                end


                cept_centre = 0
                for f in 1:length(fre)
                    # we now add the count to the rel_distortion counts
                    maps_to = sort([k for (k,v) in a if v==f])
                    fert = length(maps_to)

                    if fert >0

                        # initialise a new dictionary
                        new_d = Dict()
                        for j in 1:length(maps_to)

                            # if this is not the first word in a cept
                            if j>1

                                # calculate the relative distortion for the
                                # case where the word in not first in the cept
                                rel_dist = maps_to[j]-maps_to[findall(x->x==maps_to[j], maps_to)[1] - 1]
                                new_d = Dict(fert=>Dict(rel_dist => c))

                            # for the first word in a cept
                            else
                                rel_dist = maps_to[j] - cept_centre
                                new_d = Dict(fert=>Dict(rel_dist => c))
                            end

                            # increment the distortion distribution counts
                            count_d = merge(merger_plus, count_d, new_d)
                            merge!(+,total_d,Dict(fert=>c))
                        end

                        # calculate the centre of the cept
                        cept_centre = convert(Integer,ceil(mean(maps_to)))
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

        # Normalise the translation distribution
        for i in 1:length(Translation_Dict)
            fre = collect(keys(Translation_Dict))[i]
            @threads for j in 1:length(Translation_Dict[fre])
                eng = collect(keys(Translation_Dict[fre]))[j]
                Translation_Dict[fre][eng] = count_t[fre][eng]/ total_t[fre]
            end
        end

        # Normalise the alignment distribution
        for k in keys(count_d)
            alignments[k] = Dict(keys(count_d[k]).=>values(count_d[k])./total_d[k])
        end

        # Normalise the fertility distribution
        for f in collect(keys(fertilities))
            normed_vals = values(fertilities[f])./ sum(values(fertilities[f]))
            fertilities[f] = Dict(keys(fertilities[f]).=> normed_vals)
        end

        # Recalculate the null-insertion probabilities
        p1 = count_p1/(count_p1+count_p0)
        p0 = 1 - p1

        # Redefine the current model state
        init = Dict("trans"=> copy(Translation_Dict),"align"=>copy(alignments),
                    "fert"=>copy(fertilities), "null" => [p1,p0])

    end
    return(init)
end
