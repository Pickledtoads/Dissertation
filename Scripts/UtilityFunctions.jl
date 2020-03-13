function merger_plus(a,b)
    # Purpose:  To correctly sum probabilites within dictionaries of dictionaries
    #           we produce a dictionary for each french word showing the prob of
    #           each english translation. merger_plus allows us to correctly merge
    #           a new dictionary of prob counts with this overarching dictionary
    # Inputs :  a - a dictionary of dictionaries
    #           b - a dictionary
    # Outputs:  the result of merging the
    return(merge(+,a,b))
end

function Sent_Split(y, x)
    # Purpose: Divide the French and English sentences up into words and add the null token
    # Inputs : y - a string containing the space-separated English sentence
    #          x - a string containing the space-separated French sentence
    # Outputs: two arrays containing the separated words of each input sentence

    # split the english sentence into words
    e = split(y, " ")
    e = convert(Array,e)
    # split the french sentence into words
    f = split(x, " ")
    f = convert(Array,f)
    #add in a null token
    append!(f,["null"])
    return([e,f])
end

function init_Align(Eng, Fre)
    # Purpose: To initialise the alignment distribution uniformly
    # Inputs : Eng - array containing all English sentences
    #          Fre - array containing all French sentences
    # Outputs: A dictionary containing uniform alignment distributions
    #          for each pair of sentences
    align = Dict()
    for i in 1:length(Fre)

            # call Sent_Split to split the sentences into words
            sent = Sent_Split(Eng[i],Fre[i])
            e = sent[1]
            f = sent[2]
            # generate an alignment table key
            align_key = hcat(string(length(e)),string(length(f)))

            # if this alignment table has be filled already skip it
            if setdiff(align_key,keys(align)) != nothing
                # build an array for sentence i where each entry
                # is 1/(lf+1)
                new = zeros(Float64,length(e),length(f)) .+ 1
                new = new/(size(f)[1]+1)

                # associate the new alignment table with its key in the dictionary
                align[align_key] = new
            end
    end
    return(align)
end

function zero_align(align)
    # Purpose:  to create a copy of an alignment distribution with all probabilities
    #           reset to zero
    # Inputs :  align - an alignment distribution
    # Outpurs   An alignment distribution with all-zero alignment tables

    new_align = copy(align)

    # for each key create a 2D array of zeros and replace the current table
    for i in keys(align)
        row = size(align[i])[1]
        col = size(align[i])[2]
        new_align[i] = zeros(row, col)
    end
    return(new_align)
end

function zero_dict(dict)
    # Purpose: Return a copy of a dictionary with each entry set to zero
    # Inputs:  dict - a dictionary of dictionaries
    # Outputs: newdict - a dictionary with the same structure as dict
    #                    but with each second-level entry set to zero

    newdict = copy(dict)

    # for each of the main key values
    for i in keys(dict)

        # set the dictionary at each main key to one with all zero entries
        new = Dict(keys(dict[i]) .=> [0.0]*length(values(dict[i])))
        newdict[i] = new
    end

    # return the newly zeroed dictionary
    return(newdict)
end
