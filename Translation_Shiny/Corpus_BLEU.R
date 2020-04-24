# Load in the Functions for Translating and Finding BLEU
source("Shift_to_R.R")
library(readr)
Corpus_BLEU <- function(filepath1,filepath2, choice, leng, folder){
    # Purpose:  calculate and average the BLEU score for the first n sentences in 
    #           the corpus
    # Inputs:   filepath1 - the filepath to the french portion of the corpus
    #           filepath2 - the filepath to the English portion of the corpus
    #           choice - which model to use
    #           leng - number of sentences to find the average over
    #           folder - where the trained models are to be found
    # Outputs:  an averaged BLEU score for the first n sentences in the corpus
  
    # reassign the chosen model for translation
    choice <- unlist(choice)[1]
    leng <- as.numeric(leng)
    
    # open the corpus for reading line by line
    con1 <- as.character(unlist(read.table(url(filepath1), sep="\n", encoding = "UTF-8", nrows = leng)[,1]))
    con2 <- as.character(unlist(read.table(url(filepath2), sep="\n", encoding = "UTF-8", nrows = leng)[,1]))
     # initialise variables to store the sum of sentence BLEUs 
    count <- 0
    ping <- 1
    
    # for each of the 
    for (i in 1:leng) {
      
      # read the next line from the corpus
      fr_line = unlist(strsplit(con1[i], split = " "))
      eng_line = unlist(strsplit(con2[i], split = " "))

      # generate the translation using the right model
      if (choice == "IBM 1"){
        translation <- IBM1_Translate(fr_line, 10000, folder)
        
      }
      else if (choice == "IBM 2"){
        translation <- IBM2_Translate(fr_line, 10000, folder)
        
      }
      else if (choice == "IBM 3"){
        translation <- IBM3_Translate(fr_line, 10000, folder)
        
      }
      else if (choice == "IBM 4"){
        translation <- IBM4_Translate(fr_line, 10000, folder)
        
      }
      else if (choice == "IBM 5"){
        translation <- IBM5_Translate(fr_line, 10000, folder)
      }
      
      # increment the BLEU sum
      count = count+ BLEU(eng_line,translation)

      
      
      if ( length(line) == 0 ) {
        break
      }
      
      ping <- ping + 1
      print(ping)
    }
    
  
    
    # return the average BLEU
    return(count/i)
    
}
