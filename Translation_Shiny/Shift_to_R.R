# import needed librarys
require(rhdf5)

IBM1_Translate <-function(french,n, File_Root){
  # Purpose:  to apply the IBM1 model to translate a sentence in french
  # Inputs:   french - the french sentence
  #           n - the number of sentences used to train the model
  # Outputs:  the french sentence translated to english
  
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


IBM2_Translate <- function(french, n, File_Root){
  # Purpose:  to apply the IBM2 model to translate a sentence in french
  # Inputs:   french - the french sentence
  #           n - the number of sentences used to train the model
  # Outputs:  the french sentence translated to english
  
  # Load in the IBM2 data
  IBM2_Names <- c(paste("IBM2_trans_",n,".csv", sep=""),paste("IBM2_align_",n,".csv", sep=""))
  IBM2_trans <- read.csv(paste(File_Root,IBM2_Names[1], sep=""),encoding ="UTF-8")
  IBM2_align <- read.csv(paste(File_Root,IBM2_Names[2], sep=""),encoding ="UTF-8")
  
  # Add in the null word & save down the length of the sentence
  french <- append(french,"null")
  l_fre <- length(french)
  eng <- c()
  print("hi")
  for (f in 1:l_fre){
    
    align <-IBM2_align[IBM2_align["fre_len"] == l_fre & IBM2_align["eng_len"] == l_fre, ]
    
    # Select the correct alignment table
    tryCatch({
      index <- unname(which.max(unlist(align[align['e_ind']==f,]["prob"]))[1])
    }
    , error = function(e) { index <-f})
       
    
    print(french)
    # Find the most likely alignment point for the fth target word
    # Find the most likely translation for the source word aligned with by the fth target word
    
    word_index = unname(which.max(unlist(IBM2_trans[IBM2_trans["fre"]==french[f],]["prob"]))[1])
    
    # Build the output vector of translated words
    engl <- as.character(IBM2_trans[IBM2_trans["fre"]==french[f],][,"eng"][word_index])
    eng <- append(eng,engl)
  }
  
  return(eng)
}


IBM3_Translate <- function(french, n, File_Root){
  # Purpose:  to apply the IBM3 model to translate a sentence in french
  # Inputs:   french - the french sentence
  #           n - the number of sentences used to train the model
  # Outputs:  the french sentence translated to english
  
  # Load in the IBM3 data
  IBM3_Names <- c(paste("IBM3_trans_",n,".csv", sep=""),paste("IBM3_align_",n,".csv",sep=""),paste("IBM3_fert_",n,".csv", sep=""),paste("IBM3_null_",n,".csv", sep=""))
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
    rand <- runif(length(words),0,1)
    null_insert <- rand < rep(IBM3_null[1], length(words))
    if (is.null(words)){
      return("N/A")
    }
    
    if (sum(null_insert)!=0){
      ind_null <- which(null_insert==T)
      ind_null <- ind_null + 1:length(ind_null)
      null_insert <- rep("",length(words)+length(ind_null))
      null_insert[ind_null] <- "null"
      null_insert[null_insert != "null"] <- words
    }
    else{
      null_insert <- words
    }
    # Now we align the words
    n_fre = length(french)
    n_eng = length(null_insert)
    align = IBM3_align[IBM3_align[,"fre_len"] == (n_fre+1) & IBM3_align[,"eng_len"] == n_eng,]
    aligned = rep("", length(null_insert))
    
    # to keep track of the fertility of the current word
    fert_index = 1
    fert_current = 1
    # The next step is to deal with alignment
    for (f in 1:length(null_insert)){
      # null tokens follow uniform distribution
      if (null_insert[f] == "null"){
        aligned[ceiling(runif(1,0,length(null_insert) + 1))] = "null"
      }
      
      else{
        if (fert_current == 1){
          alignment <- align[align[,"f_ind"] == f,]
        }
        else {
          # temporarily remove each alignment point so words in the same
          # cept are aligned differently
          alignment <- alignment[-index,]
        }
        # find the most likely alignment
        max_index <- which.max(alignment[,"prob"])[1]
        index <- alignment[,"e_ind"][max_index]
        aligned[index]<- null_insert[f]
        
        
        # increment fertilty index and reset fert_current if we just aligned the 
        # last word in the cept
        if (fert_current==ferts[fert_index]){
          fert_index = fert_index + 1
          fert_current = 1

        }
        
        # otherwise increment the current fertility
        else{
          fert_current =fert_current + 1
        }
      }


    }
    aligned <- aligned[aligned != ""]
    # if we can't find and alignment table skip alignment
    if (is.na(alignment[1,1])){
      aligned <- null_insert
    }
    translation <- rep("", length(aligned))
    fert_index = 1
    fert_current = 1

    for (f in 1:length(aligned)){
      if (fert_current == 1){
        lex <- IBM3_trans[IBM3_trans["fre"]==aligned[f],]
      }
      else {
        # prevent words in the same cept having identical translations
        lex <- lex[lex$eng != eng,]
      }
      # find the word with highest probability
      max_trans <- which.max(lex[,"prob"])[1]
      eng <- as.character(lex[,"eng"][max_trans])
      translation[f] <- eng
      

      # increment fertilty index and reset fert_current if we just aligned the 
      # last word in the cept
      if (fert_current == ferts[fert_index] & aligned[f]!="null"){
        fert_index = fert_index+1
        fert_current = 0
      }
      
      # otherwise increment the current fertility
      if (aligned[f] != "null"){
        fert_current = fert_current+1
      }
  }
   return(translation)
}


IBM4_Translate <- function(french, n, File_Root){
  # Purpose:  to apply the IBM4 model to translate a sentence in french
  # Inputs:   french - the french sentence
  #           n - the number of sentences used to train the model
  # Outputs:  the french sentence translated to english
  
  # Load in the IBM4 data
  IBM4_Names <- c(paste("IBM4_trans_",n,".csv", sep=""),paste("IBM4_align_",n,".csv", sep=""),paste("IBM4_fert_",n,".csv", sep=""),paste("IBM4_null_",n,".csv", sep=""))
  
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
  
  # set up variables to track word fertility 
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
        
        # select the right alignment table for this word
        alignment <- align[align["fert"]==ferts[fert_index],]
        alignment <- alignment[alignment$rel_dist %in% (1-last_cept):(length(null_insert)-last_cept),]
        
        # find the likliest relative distortion
        greatest_prob <- which.max(unlist(alignment["prob"]))[1]
        mapping <- last_cept+alignment[greatest_prob,"rel_dist"]
        aligned[mapping] <- null_insert[f] 
        
        # keep track of the indicies in this cept
        cept_map <- append(cept_map,mapping)
        
        
      }
      
      # for words after the first in a cept
      else {
        
        # calculate the last index mapped to
        last_point <- cept_map[fert_current-1]
        
        # find the range of possible distortions
        range_of_dist <- (1-last_point):(length(null_insert)-last_point)
        
        # select the correct alignment table
        alignment <- align[align["fert"]==ferts[fert_index],]
        alignment <- alignment[alignment$rel_dist %in% unlist(range_of_dist),]
        
        # find the most likely alignment
        greatest_prob <- which.max(unlist(alignment["prob"]))[1]
        mapping <- last_point+alignment[greatest_prob,"rel_dist"]
        aligned[mapping] <- null_insert[f] 
        
        # keep track of the indicies in this cept
        cept_map <- append(cept_map,mapping)
        
      }
    }
  }
  translation <- rep("", length(aligned))
  
  # initialise variables to track fertility
  fert_index = 1
  fert_current = 1
  for (f in 1:length(aligned)){
    if (fert_current == 1){
      lex <- IBM4_trans[IBM4_trans["fre"]==aligned[f],]
    }
    else {
      
      # remove a word so that in the same cept do not get translated to the same word
      lex <- lex[lex$eng != eng,]
    }
    
    # find the translation with maximum probability 
    max_trans <- which.max(lex[,"prob"])[1]
    eng <- as.character(lex[,"eng"][max_trans])
    translation[f] <- eng
    
    
    # reset the fertility counts
    if (fert_current == ferts[fert_index]){
      fert_index = fert_index+1
      fert_current = 0
      last_cept <- as.integer(ceiling(mean(cept_map)))
    }
    fert_current = fert_current+1
    
  }
  return(translation)
}


IBM5_Translate <- function(french, IBM5_trans,IBM5_align,IBM5_fert,IBM5_null){
  # Purpose:  to apply the IBM5 model to translate a sentence in french
  # Inputs:   french - the french sentence
  #           n - the number of sentences used to train the model
  # Outputs:  the french sentence translated to english
  
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
  
  # initilise variables to track fertility
  fert_index = 1
  fert_current = 1
  last_cept <- 0
  cept_map <- c()
  vmax <- length(aligned)
  
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
        
        # select the correct alignment function
        alignment <- align[align["fert"]==ferts[fert_index],]
        alignment <- alignment[alignment$vac_max == vmax,]
        alignment <- alignment[alignment$last_cept == last_cept, ]
        alignment <- alignment[alignment$rel_dist %in% 1:vmax, ]
        
        greatest_prob <- which.max(unlist(alignment["prob"]))
        # find the indices of the missing entries
        empties <- which(aligned == "")
        mapping <- empties[alignment[greatest_prob,"rel_dist"]]
        aligned[mapping] <- null_insert[f] 
        cept_map <- append(cept_map,mapping)
        vmax = vmax-1
      }
      
      # after the first word in a cept
      else {
        
        # calculate the last index mapped to
        last_point <- sum(aligned[1:cept_map[fert_current-1]]=="")
        
        # consider only vacencies after the last places word
        vmax_1 <- vmax - last_point
        
        # select the correct alignment function
        alignment <- align[align["fert"]==ferts[fert_index],]
        alignment <- alignment[alignment$vac_max == vmax_1,]
        alignment <- alignment[alignment$last_cept == last_point, ]
        alignment <- alignment[alignment$rel_dist %in% 1:vmax_1, ]
        
        # choose the most likely alignment
        greatest_prob <- which.max(unlist(alignment["prob"]))[1]
        empties_1 <- which(aligned == "")[(last_point+1):vmax]
        mapping <- empties_1[alignment[greatest_prob,"rel_dist"]]
        aligned[mapping] <- null_insert[f] 
        cept_map <- append(cept_map,mapping)
        
        # deincrement the number of vacencies
        vmax = vmax-1
      }
      
      # reset the counts
      if (fert_current == ferts[fert_index]){
        
        fert_index = fert_index+1
        fert_current = 0
        last_cept <- as.integer(ceiling(mean(cept_map)))
        last_cept <- sum(aligned[1:last_cept]=="")
        cept_map <- c()
      }
      
      # increment the fertilities
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
      # remove one word so that two words in a cept are not translated the same
      lex <- lex[lex$eng != eng,]
    }
    
    # find the most likely translation
    max_trans <- which.max(lex[,"prob"])[1]
    eng <- as.character(lex[,"eng"][max_trans])
    translation[f] <- eng
    fert_current = fert_current+1
    
    # reset the fertility count
    if (fert_current == ferts[fert_index]){
      fert_index = fert_index+1
      fert_current = 0
      last_cept <- as.integer(ceiling(mean(cept_map)))
    }
    
    
  }
  return(translation)
}


BLEU <- function(refer, trans){
  # Purpose:  To calculate the BLEU for a translated sentence
  # Input:    refer - a correct translation to use as a comparison
  #           trans - our translation to compare to 
  # Output:   The BLEU score for the reference-translation pair
  
  # split the words into vectors
  ref <- unlist(strsplit(refer, split = " "))
  sent <- unlist(strsplit(trans, split = " "))
  
  # calculate the brevity penalty
  if (length(ref) <= length(sent)){
    bp <- 1
  }
  else {
    bp <- exp(1-length(ref)/length(sent))
  }
  
  # initialise the total
  total <- 0
  
  for (i in 1:min(c(4,length(sent), length(ref)))){
    
    # if we have ngons of length greater than one
    if (i != 1){
      L <- c()
      rence <- c()
      # grab every collection of n words that are next to one another in the translation
      for (step in 1:(length(sent)-(i-1))){
        L <- append(L, paste(sent[step:(step+i-1)], sep=" ", collapse=" "))
      }
      
      # grab every collection of n words that are next to one another in the reference
      for (step in 1:(length(ref)-(i-1))){
        rence <- append(rence, paste(ref[step:(step+i-1)], sep=" ", collapse=" "))
      }

      
    }
    # if n = 1 set L and rence
    else{
      L <- sent
      rence <- ref
    }
    
    # initialise the count
    count = 0
    
    # for each of the ngons in L
    for (ngon in 1:length(L)){
      if (L[ngon] %in% rence){
        
        # increment the count
        count = count + 1
        
        # remove the matched n-gon from rence
        index <- unlist(which(rence==L[ngon]))[1]
        rence <- rence[-index]
        
      } 
    }
    
    # update the total
    total = total + count/length(L)
    
    
  }
  
  # find the average total over all n
  total <- bp*total/4
  
  # return the total
  return(total)
  
}
