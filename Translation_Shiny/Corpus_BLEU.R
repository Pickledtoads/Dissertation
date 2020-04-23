# Load in the Functions for Translating and Finding BLEU
source("Shift_to_R.R")

Corpus_BLEU <- function(filepath1,filepath2, choice, leng, folder){
    choice <- unlist(choice)[1]
    con1 <- file(filepath1, "r")
    con2 <- file(filepath2, "r")
    count <- 0
    ping <- 0
    for (i in 1:leng) {
      fr_line = unlist(strsplit(readLines(con1, n = 1, encoding = "UTF-8"), split = " "))
      eng_line = unlist(strsplit(readLines(con2,n = 1, encoding = "UTF-8"), split = " "))
      
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
      
      count = count+ BLEU(eng_line,translation)

      
      
      if ( length(line) == 0 ) {
        break
      }
      ping = ping +1 
      print(ping)
    }
    close(con1)
    close(con2)
    
    return(count/i)
    
}
