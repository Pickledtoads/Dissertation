function IBM2(Eng, Fre, iter, init)
    # Purpose: To implement the EM algorithm to train a IBM 2 model for translation
    # Inputs:  Eng - ordered array containing the english sentences for training on
    #          Fre - matching ordered array of french sentences for training the model
    #          iter - how many iterations to train for
    #          init - initial translation dictionary (output of the IBM 1 model training)
    # Outputs: Trans_Dict - a dictionary of translation probabilities for the IBM 2 model
    #          align - a dictionary of alignment probabilites


    # Initialise each of  the data-structures
    # we need for this algorithm

    # this holds current translation probs
    Trans_Dict = copy(init)

    # this holds the current alignment probabilites
    align = init_Align(Eng, Fre)

    # needed later when gathering counts

    count_a = zero_align(copy(align))
    # built the variable to hold total alignment
    total_a = copy(count_a)
    for k in keys(count_a)
        total_a[k] = sum(count_a[k], dims =2)
    end

    count_a = zero_align(align)
    alignment = copy(count_a)

    # Run iter times
    for x in 1:iter
        # Initialize all variables to hold counts
        s_tot = Dict()
        count_s = zero_dict(copy(init))
        total_s = Dict(keys(Trans_Dict) .=> [0.0]*length(count_s))
        for s in 1:length(Fre)

            # Split the sentence up into words
            sent = Sent_Split(Eng[s], Fre[s])
            e = sent[1]
            f = sent[2]
            # The dictionary key pointing to the right alignment table
            align_key = hcat(string(length(e)), string(length(f)))
            # Put together the counts
            for j in 1:length(e)
                for i in 1:length(f)

                    # find the log probability of translation
                    latest = log(Trans_Dict[f[i]][e[j]])+log(align[align_key][j,i])

                    # Ignore the entries with a nan or infinite value
                    if isnan(latest)
                        latest = 0
                    end

                    # rescale the probability off the log scale
                    latest = MathConstants.e ^ latest
                    try
                        s_tot[e[j]] += latest
                    catch
                        s_tot[e[j]] = latest
                    end
                end
            end

            for j in 1:length(e)
                for i in 1:length(f)

                    #   find the normalised translation probabilty for each
                    #   pair in the sentence
                    c = convert(Float64,log(Trans_Dict[f[i]][e[j]])+log(align[align_key][j,i])-log(s_tot[e[j]]))
                    c = MathConstants.e ^ c
                    c = convert(Float64, c)

                    # if we get nan or infinte values set them to zero
                    if isnan(c) | isinf(c)
                        c = 0
                    end

                    # add c onto the appropriate count storage variables
                    count_s[f[i]][e[j]] += c

                    total_s[f[i]] += c
                    count_a[align_key][j,i] += c
                    total_a[align_key][j] +=c
                end
            end

            #   this expression finds the total count for each column of the
            #   alignment probability distribution
        end

        # We now refresh our estimates for the probability distributions
        for fr in keys(count_s)
            for en in keys(count_s[fr])
                # divide each English word's probability by the sum of all
                # potential translations of the French source word
                new = log(count_s[fr][en])- log(total_s[fr])
                count_s[fr][en] = MathConstants.e ^ new
            end
        end

        alignment = Dict()
        for align_key in keys(count_a)
            f_l = size(count_a[align_key])[2]
            e_l = size(count_a[align_key])[1]
            arr = Matrix(undef,e_l,f_l)
            for j in 1:size(count_a[align_key])[1]
                for i in 1:size(count_a[align_key])[2]
                    # divide each entry by the sum of the row it is in
                    new = log(count_a[align_key][j,i])-log(total_a[align_key][j])
                    arr[j,i] = MathConstants.e^new
                end
            end
        for align_key in keys(alignment)
            for j in 1:size(alignment[align_key])[1]
                for i in 1:size(alignment[align_key])[2]
                    # divide each entry by the sum of the row it is in
                    new = log(count_a[align_key][j,i]) - log(sum(count_a[align_key][j,i], dims=2)[j])
                    alignment[align_key][j,i] = MathConstants.e ^ new

                end
            end
            alignment[align_key]=arr
        end

        # reset the intial values for the next iteration
        align = alignment
        Trans_Dict = count_s
    end
    # output the updated estimates
    return(Dict("trans"=>Trans_Dict, "align" => alignment, "align2"=>count_a, "align3"=>total_a))
    end
end
