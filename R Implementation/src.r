#Set Working Directory
wd <- "D:/Projects/SMS-SPAM-HAM-Detection/R Implementation"
setwd(wd)

#Set File Path
filepath <- file.path("D:/Projects/SMS-SPAM-HAM-Detection/R Implementation")
dir(filepath)

#Read the .csv file (raw spam/ham messages)
corpus <- read.csv("smsSpamCollection_untouched.csv", header = TRUE, sep = ",")

#We will use 'stringr' package for data manipulation, other packages can also be used.
install.packages("stringr")
#corpus[,"sms"]
library("stringr")

#Calc no of characters
chars <- str_length(corpus[,"sms"])
#str(chars)

#Calc Alphabets
alpha <- str_count(corpus[,"sms"],"[a-zA-Z]")

#Ratio of Alphabest / #Characters
alpha_by_c <- alpha/chars

#Ratio of Digit / #Characters
digit_by_c <- str_count(corpus[,"sms"],"[0-9]")/chars

#Ratio of Whitespace Count/ #Characters
whitesp_by_ch <- str_count(corpus[,"sms"],"[ ]")/chars

#Ratio of Special Characters / #Characters
spec_by_c <- str_count(corpus[,"sms"],"[^a-zA-Z0-9 ]")/chars

#Count no of words
words <- str_count(corpus[,"sms"],"[ ]")+1

#Count short words using user defned function
# wordless <- function(v){
#   length<-str_count(v," ")+1
#   nlength<-1:length
#   count <- 0
#   i <- 1
#   tmp<-str_split(v," ")
#   for(i in nlength)
#   {
#     if(nchar(tmp[[1]][i]) < 3)
#     {
#       count <- count + 1
#     }
#   }
#   count
# }

#OR Inline as below
# Count short words using for loop (Alternate approach)
strwd <- str_split(corpus[,"sms"]," ")
count<-0
swcount <- vector(mode = "integer" , length=length(strwd))
for(i in 1:length(strwd))
{
  for(j in 1:length(strwd[[i]]))
  {
    if((nchar(strwd[[i]][j]))<=2)
    {
      count=count+1
    }
  }
swcount[i]<-count
count<-0
}
#Ratio of Short Words /#Words
shtwd_by_w <- swcount/words 


wdlensum<-0

#Calc Average Average Word length
avgwdlen <- vector(mode = "integer" , length=length(strwd))
for(i in 1:length(strwd))
{
  for(j in 1:length(strwd[[i]]))
  {
    wdlensum<-wdlensum+nchar(strwd[[i]][j])
  }
  avgwdlen[i]<-wdlensum/length(strwd[[i]])
  wdlensum<-0
}

#Ratio of Number of Unique Words/#Words
unique_words_count<-str_count(vapply(lapply(strsplit(toupper(as.vector(corpus[,"sms"])), " "),unique),paste,character(1L), collapse=" ")," ")+1

#OR using loop
# uniqstrwd<-strwd
# ucount<-0
# uwcount <- vector(mode = "integer" , length=length(strwd))
# for(i in 1:length(strwd))
# {
#   for(j in 1:length(strwd[[i]]))
#   {
#     uniqstrwd[[i]]<-unique(strwd[[i]])
#     ucount <- length(uniqstrwd[[i]])
#   }
#   uwcount[i]<-ucount
#   ucount<-0
# }    

uniqwd_by_w <- unique_words_count/words

final_dataset <- data.frame(chars,alpha_by_c,digit_by_c,whitesp_by_ch,spec_by_c,words,shtwd_by_w,avgwdlen,uniqwd_by_w,corpus$class)
 
final_dataset$class_binary <- 10
final_dataset$class_binary[fv$corpus.class == "spam"] <- 1
final_dataset$class_binary[fv$corpus.class == "ham"] <- 0
 
install.packages("neuralnet")
library(neuralnet)
net.sqrt <- neuralnet(formula = class_binary~chars+alpha_by_c+digit_by_c+whitesp_by_ch+spec_by_c+words+shtwd_by_w+avgwdlen+uniqwd_by_w,data = fv, hidden=8, threshold=0.01)

