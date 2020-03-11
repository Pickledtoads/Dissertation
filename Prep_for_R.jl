using HDF5, JLD, Base.Threads, Statistics, DataFrames,CSV
n=100
include(joinpath(@__DIR__, "Scripts\\IBM3.jl"))
Name4 = string(joinpath(@__DIR__, "Trained\\"),"IBM3_save_", n, ".jld")
IBM3_save = load(Name4)["IBM3_save"]

println(IBM3_save["fert"])

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
Name4_trans = string(joinpath(@__DIR__, "Trained\\"),"IBM3_trans_", n, ".csv")
CSV.write(Name4_trans, df_trans)


align = IBM3_save["align"]
french_len = []
english_len = []
fre_ind = []
eng_ind= []
probs = []
for k in keys(align)
    for i in 1:size(align[k])[2]
        for j in 1:size(align[k])[1]
            push!(french_len,k[1])
            push!(english_len,k[2])
            push!(fre_ind,i)
            push!(eng_ind,j)
            push!(probs,align[k][j,i])
        end
    end

end

df_align = DataFrame(fre_len=french_len, eng_len=english_len, f_ind = fre_ind,e_ind = eng_ind, prob=probs)

Name4_align = string(joinpath(@__DIR__, "Trained\\"),"IBM3_align_", n, ".csv")
CSV.write(Name4_align, df_align)


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
Name4_fert = string(joinpath(@__DIR__, "Trained\\"),"IBM3_fert_", n, ".csv")
CSV.write(Name4_fert, df_fert)


Name4_null = string(joinpath(@__DIR__, "Trained\\"),"IBM3_null_", n, ".csv")

null = IBM3_save["null"]

df_null = DataFrame(p1=null[1], p2 = null[2])
Name4_null = string(joinpath(@__DIR__, "Trained\\"),"IBM3_null_", n, ".csv")
CSV.write(Name4_null, df_null)
