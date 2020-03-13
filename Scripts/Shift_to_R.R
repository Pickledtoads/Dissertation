require(rhdf5)

File_Root <- "C:/Users/julul/github/Dissertation/Trained/"
IBM3_Names <- c("IBM3_trans_50.csv","IBM3_align_50.csv","IBM3_fert_50.csv","IBM3_null_50.csv")

IBM3_trans <- read.csv(paste(File_Root,IBM3_Names[1], sep=""),encoding ="UTF-8")
IBM3_align <- read.csv(paste(File_Root,IBM3_Names[2], sep=""),encoding ="UTF-8")
IBM3_fert <- read.csv(paste(File_Root,IBM3_Names[3], sep=""),encoding ="UTF-8")
IBM3_null <- read.csv(paste(File_Root,IBM3_Names[4], sep=""),encoding ="UTF-8")

french = unlist(strsplit("nous ne savons pas ce qui se passe", split =" "))
IBM3_align[IBM3_align[,"fre_len"]==10,]
IBM3_align[IBM3_align[,"fre_len"]==7,]


IBM3_Translate <- function(french, IBM3_trans,IBM3_align,IBM3_fert,IBM3_null)
{
  # Create a vector to contain the translation
  words <- c()
  ferts <- c()

  for (i in 1:length(french)){
    # Select the word that we are translating
    fre <- french[i]

    # Carry out the fertility step
    Fre_ferts<- IBM3_fert[IBM3_fert["french"]==fre,]
    ind <- which.max(unlist(Fre_ferts[,"prob"]))
    fert <- Fre_ferts[,"fertility"][ind]
    
    if (fert != 0) {
      ferts <- append(ferts,fert)
      words <- append(words, rep(fre,fert))
    }
  }


    # Carry out the null insertion step ;)
    null_insert <- runif(length(words))< IBM3_null[1]
    ind_null <- which(null_insert==T)
    ind_null <- ind_null + 1:length(ind_null)
    null_insert <- rep("",length(words)+length(ind_null))
    null_insert[ind_null] <- "null"
    null_insert[null_insert != "null"] <- words
    # Now we align the words
    n_fre = length(french)
    n_eng = length(null_insert)
    align = IBM3_align[IBM3_align[,"fre_len"]==(n_fre) & IBM3_align[,"eng_len"]==n_eng,]
    print(c(n_fre,n_eng))   
    aligned = rep("", length(null_insert))
    fert_index = 1
    fert_current = 1
    print(null_insert)
    # The next step is to deal with alignment
    for (f in 1:length(null_insert)){

      if (null_insert[f] == "null"){
        aligned[ceiling(runif(1,length(null_insert) + 1))] = "null"
      }
      else{
        if (fert_current == 1){
          alignment <- align[align[,"f_ind"] == f,]
        }
        else {
          alignment <- alignment[-index,]
        }
        print(alignment)
        max_index <- which.max(alignment[,"prob"])[1]
        index <- alignment[,"e_ind"][max_index]
        print(index)
        aligned[index]<- null_insert[f]
        if (fert_current==ferts[fert_index]){
          fert_index = fert_index + 1
          fert_current = 1

        }
        else{
          fert_current =fert_current + 1
        }
      }


    }
    
    translation <- rep("", length(aligned))

    for (f in 1:length(aligned)){
      lex <- IBM3_trans[IBM3_trans[,"french"]==aligned[f],]
      return(lex)
      max_trans <- which.max(lex[,"prob"])[1]
      eng <- lex[,"english"][max_trans]
      translation[f] <- eng
    }
    print(translation)
}

IBM3_Translate(french, IBM3_trans,IBM3_align,IBM3_fert,IBM3_null)

