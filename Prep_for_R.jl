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


function IBM4_R_Prep(IBM4_save,n)
    include(joinpath(@__DIR__, "Scripts\\IBM4.jl"))
    Name = string(joinpath(@__DIR__, "Trained\\"),"IBM4_save_", n, ".jld")
    lex = IBM4_save["trans"]
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
    Name_trans = string(joinpath(@__DIR__, "Trained\\"),"IBM4_trans_", n, ".csv")
    CSV.write(Name_trans, df_trans)


    align = IBM4_save["align"]
    fert = []
    rel_dist = []
    probs = []

    for k1 in keys(align)
        ky = keys(align[k])
        for k2 in ky
            push!(fert,k1)
            push!(rel_dist,k2)
            push!(probs,align[k][k2])
        end
    end



    df_align = DataFrame(fert=fert, rel_dist=rel_dist, prob=probs)

    Name_align = string(joinpath(@__DIR__, "Trained\\"),"IBM4_align_", n, ".csv")
    CSV.write(Name_align, df_align)


    fert = IBM4_save["fert"]
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
    Name_fert = string(joinpath(@__DIR__, "Trained\\"),"IBM4_fert_", n, ".csv")
    CSV.write(Name_fert, df_fert)


    Name_null = string(joinpath(@__DIR__, "Trained\\"),"IBM4_null_", n, ".csv")

    null = IBM4_save["null"]

    df_null = DataFrame(p1=null[1], p2 = null[2])
    Name_null = string(joinpath(@__DIR__, "Trained\\"),"IBM4_null_", n, ".csv")
    CSV.write(Name_null, df_null)
end


function IBM5_R_Prep(IBM5_save,n)
    include(joinpath(@__DIR__, "Scripts\\IBM5.jl"))
    Name = string(joinpath(@__DIR__, "Trained\\"),"IBM5_save_", n, ".jld")
    lex = IBM5_save["trans"]
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
    Name_trans = string(joinpath(@__DIR__, "Trained\\"),"IBM5_trans_", n, ".csv")
    CSV.write(Name_trans, df_trans)


    align = IBM5_save["align"]
    fert = []
    max_vac = []
    last_cept = []
    rel_dist = []
    probs = []

    for k1 in keys(align)
        for k2 in keys(align[k1])
            for k3 in keys(align[k1][k2])
                for k4 in keys(align[k1][k2][k3])
                    push!(fert,k1)
                    push!(max_vac,k2)
                    push!(last_cept, k3)
                    push!(rel_dist, k4)
                    push!(probs,align[k1][k2][k3][k4])
                end
            end
        end
    end



    df_align = DataFrame(fert=fert,max_vac=max_vac,last_cept=last_cept ,rel_dist=rel_dist, prob=probs)

    Name_align = string(joinpath(@__DIR__, "Trained\\"),"IBM5_align_", n, ".csv")
    CSV.write(Name_align, df_align)


    fert = IBM5_save["fert"]
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
    Name_fert = string(joinpath(@__DIR__, "Trained\\"),"IBM5_fert_", n, ".csv")
    CSV.write(Name_fert, df_fert)


    Name_null = string(joinpath(@__DIR__, "Trained\\"),"IBM5_null_", n, ".csv")

    null = IBM5_save["null"]

    df_null = DataFrame(p1=null[1], p2 = null[2])
    Name_null = string(joinpath(@__DIR__, "Trained\\"),"IBM5_null_", n, ".csv")
    CSV.write(Name_null, df_null)
end
