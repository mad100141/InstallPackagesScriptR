# Speed tests of different ways to read in large numbers of CSV files
# specifically read.csv.sql, read.csv (optimised) and fread


library(sqldf)
setwd("~/Downloads/wordcounts")
files <- sample(list.files(".", pattern="*.csv|CSV$"), 10000)

############# read.csv.sql ###################
system.time(
df.list.sql <- sapply(files, read.csv.sql, 
                          sql = "select * from file ",
                          eol = "\n",
                          comment.char = "",  
                          simplify = FALSE, 
                          field.types = c(V1="TEXT", V2="INTEGER"),
                          dbname = tempfile())
           )

## 1000 files 
# 37 sec, defaults
# 34 sec, with field types and comment char specified 

## 10000 files 
# 411 sec, 
# 419 sec

############# read.csv ###################
system.time(
  df.list.csv <- lapply(files, read.csv, 
                        comment.char = "",
                        colClasses = c("character", "integer"), 
                        stringsAsFactors=FALSE, 
                        header = TRUE, 
                        quote = ""
                        )
           )

## 1000 files 
# 3.1 sec

## 10000 files
# 100.4 sec, 42 sec, 39 sec 

############# fread ###################
system.time(
  df.list.fread<- lapply(files, data.table::fread, 
                         sep = ",",                    
                        stringsAsFactors=FALSE
  )
)

## 1000 files
# 1.282 sec

## 10000
# 18 sec

etwd("C:\\Users\\marwick\\Downloads\\American-Antiquity-Research-Article-3928\\wordcounts")
myfiles <- (dir(pattern = "\\.(csv|CSV)$", full.names = TRUE))
# read CSV files into a R data object
# fread is 10x faster than read.csv...
read2dt <- function(x) data.table::data.table(data.table::fread(x, sep = ",", stringsAsFactors=FALSE))
aawc <- vector("list", length = length(myfiles))
system.time(for(i in 1:length(myfiles))  aawc[[i]] <- read2dt(myfiles[[i]]))
aawcx <- c(rep(aawc, 5))
# untable
untable <- function(x) rep(x$WORDCOUNTS, times = x$WEIGHT); invisible(gc(v=FALSE))
aawc1 <- vector("list", length = length(aawcx))
# speedtests
lapply(1:i, function(i) dat)
microbenchmark(
(for(i in 1:length(aawcx))  aawc1[[i]] <- untable(aawcx[[i]])),
(aawc2 <- lapply(aawcx, function(i) untable(i))),
times = 10)

# self-contained spee test of untable
n = 50; i = 100
WORD <- vector(mode = "integer", length = n)
for (i in 1:n){
  WORD[i] <- paste(sample(c(rep(0:9,each=5),LETTERS,letters),5,replace=TRUE),collapse='')
}
# as data table
library(data.table)
dat_dt <- data.table(data.frame(WORD =  WORD, COUNTS = sample(1:50, n, replace = TRUE)))
dat_list_dt <- lapply(1:i, function(i) dat_dt)

# as data frame
dat_df <- data.frame(WORD =  WORD, COUNTS = sample(1:50, n, replace = TRUE))
dat_list_df <- lapply(1:i, function(i) dat_df)

# increase object size
y <- 100
dt <- c(rep(dat_list_dt, y))
df <- c(rep(dat_list_df, y))
# untable
untable <- function(x) rep(x$WORD, times = x$COUNTS); invisible(gc(v=FALSE))
# preallocate objects for loop to fill
df1 <- vector("list", length = length(df))
dt1 <- vector("list", length = length(dt))
# speedtests
library(microbenchmark)
microbenchmark(
for(i in 1:length(df))  df1[[i]] <- untable(df[[i]]),
for(i in 1:length(dt))  dt1[[i]] <- untable(dt[[i]]),
df2 <- lapply(df, function(i) untable(i)),
dt2 <- lapply(dt, function(i) untable(i)),
times = 100)


n = 300; j <- 500
WORD <- vector(mode = "integer", length = n)
for (i in 1:n){
  WORD[i] <- paste(sample(c(rep(0:9,each=5),LETTERS,letters),5,replace=TRUE),collapse='')
}
# as data table
library(data.table)
dat_dt <- data.table(WORD =  WORD, COUNTS = sample(1:50, n, replace = TRUE))
dat_list_dt <- lapply(1:j, function(j) dat_dt)

# as data frame
dat_df <- data.frame(WORD =  WORD, COUNTS = sample(1:50, n, replace = TRUE))
dat_list_df <- lapply(1:j, function(j) dat_df)

# increase object size
y <- 1
dt <- c(rep(dat_list_dt, y))
df <- c(rep(dat_list_df, y))
# untable
untable <- function(x) rep(x$WORD, times = x$COUNTS); invisible(gc(v=FALSE))
# preallocate objects for loop to fill
df1 <- vector("list", length = length(df))
dt1 <- vector("list", length = length(dt))
# speedtests
library(microbenchmark)
microbenchmark(
for(i in 1:length(df))  df1[[i]] <- untable(df[[i]]),
for(i in 1:length(dt))  dt1[[i]] <- untable(dt[[i]]),
df2 <- lapply(df, function(i) untable(i)),
dt2 <- lapply(dt, function(i) untable(i)),
dbsql$y <- lapply(dbsql$x, function(i) untable(i)),
times = 10)

# parallel processing
library(parallel)
library(snow)
cl <- makeCluster(detectCores(), type = "SOCK") 
clusterExport(cl, c("dt", "untable"))
system.time( dtp <- parLapply(cl, dt, function(i) untable(i)) )
stopCluster(cl)

# disk storage of data objects
# with filehash
library(filehash)
dbCreate("testDB")
db <- dbInit("testDB")
db$x <- dt
system.time(db$y <- lapply(db$x, function(i) untable(i)))

# with sqlite - faster?
library(filehashSQLite)
dbCreate("myTestDB", type = "SQLite")
dbsql <- dbInit("myTestDB", type = "SQLite")
dbsql$x <- dt
system.time(dbsql$y <- lapply(dbsql$x, function(i) untable(i)))

############################## testing of various parallel methods of lappply ###########
## parallel methods
library(parallel)
cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
clusterExport(cl=cl, varlist=c("dt"), envir=environment())
# `untable' each CSV file into a list of data frames, one data frame per file
system.time(pa <- parLapply(cl, dt, untable))
stopCluster(cl)
# 18.17 lots of cores...

library(multicore)
system.time(mc <- mclapply(dt, untable))
# error in fork...

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
system.time(do <- foreach(i = 1:length(dt)) %dopar% untable(dt[[i]]))
stopCluster(cl)
# 27 sec, lots of cores...

library(snowfall)
sfInit( parallel=TRUE, cpus=detectCores() )
sfExport( 'dt', 'untable' )
system.time(sf <- sfClusterApplyLB( dt, untable ) )
# 10.5 sec, lots of cores...
system.time(sf <- sfLapply( dt, untable ) )
# 13.5 sec, lots of cores
sfStop()
