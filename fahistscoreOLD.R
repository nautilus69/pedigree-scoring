##

paperData <- read.csv(file = "/home/gryphosonix/Downloads/Telegram Desktop/test.csv",header = TRUE, sep=",")

authors <- data$Author.Names
publishedyear <- data$Year
conference <- data$Conference
papertitle <- data$Paper.Title

df <- data.frame(publishedyear,conference,papertitle,authors)
dim(df)

firstnames <- c()

for (names in authors){
  ns <- strsplit(names,';')
  for (name in ns[[1]]){
    firstname <- strsplit(name,' ')[[1]][1]
    firstnames <- c(firstnames,firstname)
  }
}
firstnames <- matrix(firstnames)
dafr <- data.frame()

m <- 1
for(i in 1:nrow(df)){
  
  for (j in 1:nrow(firstnames)) {
    
    
    dafr[m,1] <- firstnames[j,1]
    dafr[m,2] <- df[i,1]
    dafr[m,3] <- df[i,2]
    dafr[m,4] <- df[i,3]
    
  }
  m <- m + 1
}
dafr