require(rhdf5)

File_Root <- "C:/Users/julul/github/Dissertation/Trained/"
french = unlist(strsplit("je vous invite ? vous lever pour cette minute de silence", split =" "))
IBM1_Translate <-function(french,n)
{
  # Load in the IBM1 data
  IBM1_Names <- c(paste("IBM1_trans_",n,".csv", sep=""))
  IBM1_trans <- read.csv(paste(File_Root,IBM1_Names[1], sep=""),encoding ="UTF-8")
  
  french <- append(french, "null")
  eng <- c()
  for (f in 1:length(french)){
    # Find the most likely translation for the source word aligned with by the fth target word
    word_index <- unname(which.max(unlist(IBM1_trans[IBM1_trans["fre"]==french[f],]["prob"]))[1])
    # Build the output vector of translated words
    engl <- as.character(IBM1_trans[IBM1_trans["fre"]==french[f],][,"eng"][word_index])
    eng <- append(eng,engl)
  }
  return(eng)
  
}


IBM2_Translate <- function(french, n)
{
  # Load in the IBM2 data
  IBM2_Names <- c(paste("IBM2_trans_",n,".csv"),paste("IBM2_align_",n,".csv"))
  IBM2_trans <- read.csv(paste(File_Root,IBM2_Names[1], sep=""),encoding ="UTF-8")
  IBM2_align <- read.csv(paste(File_Root,IBM2_Names[2], sep=""),encoding ="UTF-8")
  
  # Add in the null word & save down the length of the sentence
  french <- append(french,"null")
  l_fre <- length(french)
  eng <- c()
  for (f in 1:l_fre){
    # Select the correct alignment table
    align <- IBM2_align[IBM2_align["fre_len"] == l_fre & IBM2_align["eng_len"] == l_fre, ] 
    # Find the most likely alignment point for the fth target word
    index <- unname(which.max(unlist(align[align['e_ind']==f,]["prob"]))[1])
    # Find the most likely translation for the source word aligned with by the fth target word
    word_index = unname(which.max(unlist(IBM2_trans[IBM2_trans["fre"]==french[f],]["prob"]))[1])
    # Build the output vector of translated words
    engl <- as.character(IBM2_trans[IBM2_trans["fre"]==french[f],][,"eng"][word_index])
    eng <- append(eng,engl)
  }
  
  return(eng)
}



IBM3_Translate <- function(french, n)
{
  # Load in the IBM3 data
  IBM3_Names <- c(paste("IBM3_trans_",n,".csv"),paste("IBM3_align_",n,".csv"),paste("IBM3_fert_",n,".csv"),paste("IBM3_null_",n,".csv"))
  
  IBM3_trans <- read.csv(paste(File_Root,IBM3_Names[1], sep=""),encoding ="UTF-8")
  IBM3_align <- read.csv(paste(File_Root,IBM3_Names[2], sep=""),encoding ="UTF-8")
  IBM3_fert <- read.csv(paste(File_Root,IBM3_Names[3], sep=""),encoding ="UTF-8")
  IBM3_null <- read.csv(paste(File_Root,IBM3_Names[4], sep=""),encoding ="UTF-8")
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
    null_insert <- runif(length(words),0,1)< rep(IBM3_null[1], length(words))
    ind_null <- which(null_insert==T)
    ind_null <- ind_null + 1:length(ind_null)
    null_insert <- rep("",length(words)+length(ind_null))
    null_insert[ind_null] <- "null"
    null_insert[null_insert != "null"] <- words
    # Now we align the words
    n_fre = length(french)
    n_eng = length(null_insert)
    align = IBM3_align[IBM3_align[,"fre_len"] == (n_fre+1) & IBM3_align[,"eng_len"] == n_eng,]
    aligned = rep("", length(null_insert))
    fert_index = 1
    fert_current = 1
    # The next step is to deal with alignment
    for (f in 1:length(null_insert)){

      if (null_insert[f] == "null"){
        aligned[ceiling(runif(1,0,length(null_insert) + 1))] = "null"
      }
      else{
        if (fert_current == 1){
          alignment <- align[align[,"f_ind"] == f,]
        }
        else {
          alignment <- alignment[-index,]
        }
        max_index <- which.max(alignment[,"prob"])[1]
        index <- alignment[,"e_ind"][max_index]
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
    aligned <- aligned[aligned != ""]
    translation <- rep("", length(aligned))
    for (f in 1:length(aligned)){
      lex <- IBM3_trans[IBM3_trans["fre"]==aligned[f],]
      max_trans <- which.max(lex[,"prob"])[1]
      eng <- as.character(lex[,"eng"][max_trans])
      translation[f] <- eng
    }
   return(translation)
}


IBM4_Translate <- function(french, n)
{
  # Load in the IBM4 data
  IBM4_Names <- c(paste("IBM4_trans_",n,".csv"),paste("IBM4_align_",n,".csv"),paste("IBM4_fert_",n,".csv"),paste("IBM4_null_",n,".csv"))
  
  IBM4_trans <- read.csv(paste(File_Root,IBM4_Names[1], sep=""),encoding ="UTF-8")
  IBM4_align <- read.csv(paste(File_Root,IBM4_Names[2], sep=""),encoding ="UTF-8")
  IBM4_fert <- read.csv(paste(File_Root,IBM4_Names[3], sep=""),encoding ="UTF-8")
  IBM4_null <- read.csv(paste(File_Root,IBM4_Names[4], sep=""),encoding ="UTF-8")
  
  # Create a vector to contain the translation
  words <- c()
  ferts <- c()
  
  for (i in 1:length(french)){
    # Select the word that we are translating
    fre <- french[i]
    
    # Carry out the fertility step
    Fre_ferts<- IBM4_fert[IBM4_fert["french"]==fre,]
    ind <- which.max(unlist(Fre_ferts[,"prob"]))
    fert <- Fre_ferts[,"fertility"][ind]
    
    if (fert != 0) {
      ferts <- append(ferts,fert)
      words <- append(words, rep(fre,fert))
    }
  }
  
  # Carry out the null insertion step ;)
  null_insert <- runif(length(words),0,1)< rep(IBM4_null[1], length(words))
  ind_null <- which(null_insert==T)
  ind_null <- ind_null + 1:length(ind_null)
  null_insert <- rep("",length(words)+length(ind_null))
  null_insert[ind_null] <- "null"
  null_insert[null_insert != "null"] <- words
  # Now we align the words
  n_fre = length(french)
  n_eng = length(null_insert)
  align = IBM4_align
  aligned = rep("", length(null_insert))
  fert_index = 1
  fert_current = 1
  last_cept <- 0
  cept_map <- c()
  # The next step is to deal with alignment
  for (f in 1:length(null_insert)){
    # Special case where the word is a null token
    if (null_insert[f] == "null"){
      aligned[ceiling(runif(1,0,length(null_insert) + 1))] = "null"
    }
    else{
      # If the word is the first in the cept
      if (fert_current == 1){
        alignment <- align[align["fert"]==ferts[fert_index],]
        alignment <- alignment[alignment$rel_dist %in% (1-last_cept):(length(null_insert)-last_cept),]
        greatest_prob <- which.max(unlist(alignment["prob"]))[1]
        
        mapping <- last_cept+alignment[greatest_prob,"rel_dist"]
        aligned[mapping] <- null_insert[f] 
        cept_map <- append(cept_map,mapping)

        
      }
      # otherwise
      else {
        last_point <- cept_map[fert_current-1]
        range_of_dist <- (1-last_point):(length(null_insert)-last_point)
        alignment <- align[align["fert"]==ferts[fert_index],]
        alignment <- alignment[alignment$rel_dist %in% unlist(range_of_dist),]
        greatest_prob <- which.max(unlist(alignment["prob"]))[1]
        mapping <- last_point+alignment[greatest_prob,"rel_dist"]
        aligned[mapping] <- null_insert[f] 
        cept_map <- append(cept_map,mapping)
        
      }
      if (fert_current == ferts[fert_index]){
          
          fert_index = fert_index+1
          fert_current = 0
          last_cept <- as.integer(ceiling(mean(cept_map)))
          print(c(last_cept, cept_map))
          cept_map <- c()
      }
      fert_current <- fert_current+1
    }
    
    
  }

  translation <- rep("", length(aligned))
  fert_index = 1
  fert_current = 1
  for (f in 1:length(aligned)){
    if (fert_current == 1){
      lex <- IBM4_trans[IBM4_trans["fre"]==aligned[f],]
    }
    else {
      lex <- lex[lex$eng != eng,]
    }
    #return(lex)
    max_trans <- which.max(lex[,"prob"])[1]
    eng <- as.character(lex[,"eng"][max_trans])
    translation[f] <- eng
    fert_current = fert_current+1
    
    if (fert_current == ferts[fert_index]){
      fert_index = fert_index+1
      fert_current = 0
      last_cept <- as.integer(ceiling(mean(cept_map)))
    }
    
    
  }
  return(translation)
}


IBM5_Translate <- function(french, IBM5_trans,IBM5_align,IBM5_fert,IBM5_null)
{
  # Load in the IBM5 data
  IBM5_Names <- c(paste("IBM5_trans_",n,".csv"),paste("IBM5_align_",n,".csv"),paste("IBM5_fert_",n,".csv"),paste("IBM5_null_",n,".csv"))
  
  IBM5_trans <- read.csv(paste(File_Root,IBM5_Names[1], sep=""),encoding ="UTF-8")
  IBM5_align <- read.csv(paste(File_Root,IBM5_Names[2], sep=""),encoding ="UTF-8")
  IBM5_fert <- read.csv(paste(File_Root,IBM5_Names[3], sep=""),encoding ="UTF-8")
  IBM5_null <- read.csv(paste(File_Root,IBM5_Names[4], sep=""),encoding ="UTF-8")
  
  # Create a vector to contain the translation
  words <- c()
  ferts <- c()
  
  for (i in 1:length(french)){
    # Select the word that we are translating
    fre <- french[i]
    
    # Carry out the fertility step
    Fre_ferts<- IBM5_fert[IBM5_fert["french"]==fre,]
    ind <- which.max(unlist(Fre_ferts[,"prob"]))
    fert <- Fre_ferts[,"fertility"][ind]
    
    if (fert != 0) {
      ferts <- append(ferts,fert)
      words <- append(words, rep(fre,fert))
    }
  }
  
  # Carry out the null insertion step ;)
  null_insert <- runif(length(words),0,1)< rep(IBM5_null[1], length(words))
  ind_null <- which(null_insert==T)
  ind_null <- ind_null + 1:length(ind_null)
  null_insert <- rep("",length(words)+length(ind_null))
  null_insert[ind_null] <- "null"
  null_insert[null_insert != "null"] <- words
  # Now we align the words
  n_fre = length(french)
  n_eng = length(null_insert)
  align = IBM5_align
  aligned = rep("", length(null_insert))
  fert_index = 1
  fert_current = 1
  last_cept <- 0
  cept_map <- c()
  vmax <- length(aligned)
  print(null_insert)
  # The next step is to deal with alignment
  for (f in 1:length(null_insert)){
    # Special case where the word is a null token
    if (null_insert[f] == "null"){
      aligned[aligned == ""][ceiling(runif(1,0,vmax))] = "null"
      vmax = vmax-1
    }
    else{
      # If the word is the first in the cept
      if (fert_current == 1){
        alignment <- align[align["fert"]==ferts[fert_index],]
        print(vmax)
        alignment <- alignment[alignment$vac_max == vmax,]
        print(alignment)
        alignment <- alignment[alignment$last_cept == last_cept, ]
        alignment <- alignment[alignment$rel_dist %in% 1:vmax, ]
        
        greatest_prob <- which.max(unlist(alignment["prob"]))
        print(greatest_prob)
        # find the indices of the missing entries
        empties <- which(aligned == "")
        mapping <- empties[alignment[greatest_prob,"rel_dist"]]
        print(mapping)
        aligned[mapping] <- null_insert[f] 
        cept_map <- append(cept_map,mapping)
        vmax = vmax-1
        print(cept_map)
      }
      # otherwise
      else {
        last_point <- sum(aligned[1:cept_map[fert_current-1]]=="")
        vmax_1 <- vmax - last_point
        alignment <- align[align["fert"]==ferts[fert_index],]
        alignment <- alignment[alignment$vac_max == vmax_1,]
        alignment <- alignment[alignment$last_cept == last_point, ]
        alignment <- alignment[alignment$rel_dist %in% 1:vmax_1, ]
        greatest_prob <- which.max(unlist(alignment["prob"]))[1]
        empties_1 <- which(aligned == "")[(last_point+1):vmax]
        mapping <- empties_1[alignment[greatest_prob,"rel_dist"]]
        aligned[mapping] <- null_insert[f] 
        cept_map <- append(cept_map,mapping)
        print(cept_map)
        vmax = vmax-1
        
      }
      if (fert_current == ferts[fert_index]){
        
        fert_index = fert_index+1
        fert_current = 0
        last_cept <- as.integer(ceiling(mean(cept_map)))
        last_cept <- sum(aligned[1:last_cept]=="")
        cept_map <- c()
      }
      fert_current <- fert_current+1
    }
    
    
  }
  
  translation <- rep("", length(aligned))
  fert_index = 1
  fert_current = 1
  for (f in 1:length(aligned)){
    if (fert_current == 1){
      lex <- IBM5_trans[IBM5_trans["fre"]==aligned[f],]
    }
    else {
      lex <- lex[lex$eng != eng,]
    }
    #return(lex)
    max_trans <- which.max(lex[,"prob"])[1]
    eng <- as.character(lex[,"eng"][max_trans])
    translation[f] <- eng
    fert_current = fert_current+1
    
    if (fert_current == ferts[fert_index]){
      fert_index = fert_index+1
      fert_current = 0
      last_cept <- as.integer(ceiling(mean(cept_map)))
    }
    
    
  }
  return(translation)
}

