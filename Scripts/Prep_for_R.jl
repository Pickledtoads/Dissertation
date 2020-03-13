function IBM1_R_Prep(IBM1_save,n)
    include(joinpath(@__DIR__, "Scripts\\IBM1.jl"))
    Name = string(joinpath(@__DIR__, "Trained\\"),"IBM1_save_", n, ".jld")
    lex = IBM1_save
    french = []
    english = []
    probs = []
    for f in keys(lex)
        for e in keys(lex[f])
            push!(french,f)
            push!(english,e)
            push!(probs, lex[f][e])
        end
    end

    df_trans = DataFrame(fre=french, eng=english, prob=probs)
    Name_trans = string(joinpath(@__DIR__, "Trained\\"),"IBM1_trans_", n, ".csv")
    CSV.write(Name_trans, df_trans)


end


function IBM2_R_Prep(IBM2_save,n)
    include(joinpath(@__DIR__, "Scripts\\IBM2.jl"))
    Name = string(joinpath(@__DIR__, "Trained\\"),"IBM2_save_", n, ".jld")
    lex = IBM2_save["trans"]
    french = []
    english = []
    probs = []
    for f in keys(lex)
        for e in keys(lex[f])
            push!(french,f)
            push!(english,e)
            push!(probs, lex[f][e])
        end
    end

    df_trans = DataFrame(fre=french, eng=english, prob=probs)
    Name_trans = string(joinpath(@__DIR__, "Trained\\"),"IBM2_trans_", n, ".csv")
    CSV.write(Name_trans, df_trans)


    align = IBM2_save["align"]
    french_len = []
    english_len = []
    fre_ind = []
    eng_ind= []
    probs = []
    for k in keys(align)
        for i in 1:size(align[k])[2]
            for j in 1:size(align[k])[1]
                push!(french_len,k[2])
                push!(english_len,k[1])
                push!(fre_ind,i)
                push!(eng_ind,j)
                push!(probs,align[k][j,i])
            end
        end

    end

    df_align = DataFrame(fre_len=french_len, eng_len=english_len, f_ind = fre_ind,e_ind = eng_ind, prob=probs)

    Name_align = string(joinpath(@__DIR__, "Trained\\"),"IBM2_align_", n, ".csv")
    CSV.write(Name_align, df_align)
end




function IBM3_R_Prep(IBM3_save,n)
    include(joinpath(@__DIR__, "Scripts\\IBM3.jl"))
    Name = string(joinpath(@__DIR__, "Trained\\"),"IBM3_save_", n, ".jld")
    lex = IBM3_save["trans"]
    french = []
    english = []
    probs = []
    for f in keys(lex)
        for e in keys(lex[f])
            push!(french,f)
            push!(english,e)
            push!(probs, lex[f][e])
        end
    end

    df_trans = DataFrame(fre=french, eng=english, prob=probs)
    Name_trans = string(joinpath(@__DIR__, "Trained\\"),"IBM3_trans_", n, ".csv")
    CSV.write(Name_trans, df_trans)


    align = IBM3_save["align"]
    french_len = []
    english_len = []
    fre_ind = []
    eng_ind= []
    probs = []
    for k in keys(align)
        for i in 1:size(align[k])[2]
            for j in 1:size(align[k])[1]
                push!(french_len,k[2])
                push!(english_len,k[1])
                push!(fre_ind,i)
                push!(eng_ind,j)
                push!(probs,align[k][j,i])
            end
        end

    end

    df_align = DataFrame(fre_len=french_len, eng_len=english_len, f_ind = fre_ind,e_ind = eng_ind, prob=probs)

    Name_align = string(joinpath(@__DIR__, "Trained\\"),"IBM3_align_", n, ".csv")
    CSV.write(Name_align, df_align)


    fert = IBM3_save["fert"]
    french = []
    fertility = []
    probs = []
    for f in keys(fert)
        for i in keys(fert[f])
            push!(french,f)
            push!(fertility,i)
            push!(probs,fert[f][i])
        end
    end

    df_fert = DataFrame(french=french, fertility=fertility, prob=probs)
    Name_fert = string(joinpath(@__DIR__, "Trained\\"),"IBM3_fert_", n, ".csv")
    CSV.write(Name_fert, df_fert)


    Name_null = string(joinpath(@__DIR__, "Trained\\"),"IBM3_null_", n, ".csv")

    null = IBM3_save["null"]

    df_null = DataFrame(p1=null[1], p2 = null[2])
    Name_null = string(joinpath(@__DIR__, "Trained\\"),"IBM3_null_", n, ".csv")
    CSV.write(Name_null, df_null)
end
