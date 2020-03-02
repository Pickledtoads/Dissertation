
# Import all required packages
using HDF5, JLD, Base.Threads, Statistics

French = readlines(joinpath(@__DIR__,"CleanedShortFrench.txt"))
English = readlines(joinpath(@__DIR__,"CleanedShortEnglish.txt"))
n = 10




include(joinpath(@__DIR__, "Scripts\\UtilityFunctions.jl"))
include(joinpath(@__DIR__, "Scripts\\IBM_Initialize.jl"))

# Run the model that initializes the IBM models
Name1 = string(joinpath(@__DIR__, "Trained\\"),"IBM_init_", n, ".jld")
# if this model has already been trained load in the data
if isfile(Name1)
    IBM_init = load(Name1)["IBM_init"]
    println("loaded - IBM_init")
else
    IBM_init = Initialize(English[1:n], French[1:n])
    save(Name1, "IBM_init", IBM_init)
end

# Run the first IBM Model
include(joinpath(@__DIR__, "Scripts\\IBM1.jl"))
Name2 = string(joinpath(@__DIR__, "Trained\\"),"IBM1_save_", n, ".jld")

if isfile(Name2)
    IBM1_save = load(Name2)["IBM1_save"]
    println("loaded - IBM1_save" )
else
    IBM1_save = IBM1(English[1:n], French[1:n], 25, IBM_init)
    save(Name2, "IBM1_save", IBM1_save)
end

# Run the second IBM Model
include(joinpath(@__DIR__, "Scripts\\IBM2.jl"))
Name3 = string(joinpath(@__DIR__, "Trained\\"),"IBM2_save_", n, ".jld")

if isfile(Name3)
    IBM2_save = load(Name3)["IBM2_save"]
    println("loaded - IBM2_save" )
else
    IBM2_save = IBM2(English[1:n], French[1:n], 25, IBM1_save)
    save(Name3, "IBM2_save", IBM2_save)
end



# Now do IBM3
include(joinpath(@__DIR__, "Scripts\\IBM3.jl"))
Name4 = string(joinpath(@__DIR__, "Trained\\"),"IBM3_save_", n, ".jld")

if isfile(Name4)
    IBM3_save = load(Name4)["IBM3_save"]
    println("loaded - IBM3_save" )

else
    IBM3_save = IBM3(English[1:n], French[1:n], 1, IBM2_save)
    save(Name4, "IBM3_save", IBM3_save)
end

# Aaaaaand IBM4 😈
include(joinpath(@__DIR__, "Scripts\\IBM4.jl"))
Name5 = string(joinpath(@__DIR__, "Trained\\"),"IBM4_save_", n, ".jld")
IBM4_save = IBM4(English[1:n], French[1:n], 10, IBM3_save, IBM3_save["align"])
println(IBM4_save["align"])



if isfile(Name5)
    IBM4_save_ = load(Name5)["IBM4_save"]
    println("loaded - IBM4_save" )

else
    IBM4_save = IBM4(English[1:n], French[1:n], 5, IBM3_save, IBM3_save["align"])
    save(Name5, "IBM4_save", IBM4_save)
end


# Finally IBM5 🍰
include(joinpath(@__DIR__, "Scripts\\IBM5.jl"))
IBM5_save = IBM5(English[1:n], French[1:n], 5, IBM4_save, IBM3_save["align"])

Name6 = string(joinpath(@__DIR__, "Trained\\"),"IBM5_save_", n, ".jld")

if isfile(Name6)
    IBM5_save_ = load(Name6)["IBM5_save"]
    println("loaded - IBM5_save" )

else
    IBM5_save = IBM5(English[1:n], French[1:n], 5, IBM4_save, IBM3_save["align"])
    save(Name6, "IBM5_save", IBM5_save)
end
