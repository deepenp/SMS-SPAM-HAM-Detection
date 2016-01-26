#Testing
wd <- "D:/Projects/SMS-SPAM-HAM-Detection/R Implementation"
setwd(wd)
filepath <- file.path("D:/Projects/SMS-SPAM-HAM-Detection/R Implementation")
dir(filepath)
corpus <- read.csv("smsSpamCollection_untouched.csv", header = TRUE, sep = ",")
install.packages("stringr")
corpus[,"sms"]
library("stringr")
c <- str_length(corpus[,"sms"])
str(c)
alpha <- str_count(corpus[,"sms"],"[a-zA-Z]")
alpha_by_c <- alpha/c
digit_by_c <- str_count(corpus[,"sms"],"[0-9]")/c
whitesp_by_ch <- str_count(corpus[,"sms"],"[ ]")/c
spec_by_c <- str_count(corpus[,"sms"],"[^a-zA-Z0-9 ]")/c
w <- str_count(corpus[,"sms"],"[ ]")+1
strwd <- str_split(corpus[,"sms"]," ")
str(strwd)
head(strwd)
length(strwd[[10]])
nchar(strwd[[1]][2])
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
shtwd_by_w <- swcount/w  
wdlensum<-0
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
uniqstrwd<-strwd
ucount<-0
uwcount <- vector(mode = "integer" , length=length(strwd))
for(i in 1:length(strwd))
{
  for(j in 1:length(strwd[[i]]))
  {
    uniqstrwd[[i]]<-unique(strwd[[i]])
    ucount <- length(uniqstrwd[[i]])
  }
  uwcount[i]<-ucount
  ucount<-0
}    
uniqwd_by_w <- uwcount/w
fv <- data.frame(c,alpha_by_c,digit_by_c,whitesp_by_ch,spec_by_c,w,shtwd_by_w,avgwdlen,uniqwd_by_w,corpus$class)
str(fv)
fv$class_binary <- 10
fv$class_binary[fv$corpus.class == "spam"] <- 1
fv$class_binary[fv$corpus.class == "ham"] <- 0
head(fv)
install.packages("neuralnet")
library(neuralnet)
net.sqrt <- neuralnet(formula = class_binary~c+alpha_by_c+digit_by_c+whitesp_by_ch+spec_by_c+w+shtwd_by_w+avgwdlen+uniqwd_by_w,data = fv, hidden=8, threshold=0.01)

