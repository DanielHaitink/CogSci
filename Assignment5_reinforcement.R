#############                   ASSIGNMENT 5 - REINFORCEMENT LEARNING
#############   In this assignment, you are expected to produce two graphs based on the output of the model with the function (do-fbt-n 100). Therefore, you will have 100 output files, which have 100 lines in each file, representing 100 children repeated the task 100 times.
#############   The first graph's x-axis (from 1 to 100) should present the number of repetitions of the model. The y-axis (from 0 to 1) should show the proportion of reasoning strategies in which the model used at each repetition. This means that the graph should have 3 lines for each reasoning level (i.e., 1 line for zero-order, 1 line for first-order and 1 line for second-order reasoning strategy).
#############   The second graph's x-axis (from 1 to 100) should present the number of repetitions of the model. The y-axis should show the utility values of the production rules that you wrote in the model qith !safe-eval! functions. Thee graph should have 3 lines for each reasoning level (i.e., 1 line for zero-order reasoning strategy's utility, 1 line for first-order and 1 line for second-order reasoning strategy's utility).
#############   Based on the proportion of the reasoning strategies graph, you are expected to write one qualitative prediction for your model.
#############   The prediction will be related to children's wrong answers before they start to give correct second-order answers most of the time. 
#############   If you have more predictions, you can write them, too. 
#############   I will provide you the behavioral results after you submit this assignment.
#############   Below you can find some hints to write the code for the graphs but you don't have to use the hints and you can do it in your own way, too.

setwd("./Documents/CogSci/Data")  ## set your workspace to the path where your output files are.

## You might need to use the following packages
require(ggplot2)
require(reshape2)
require(tidyr)  
require(plyr)

## The below function will be used in order to print the two graphs together

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

##################################################################


### Before you make the graphs, you should read the output files into R and make merge them into one big data file that has 16 columns and 10,000 rows.

####You need to write a for loop in order to read the files.

#Before writing the for loop, do these two steps:

#1) create a blank matrix which has 17 columns in order to prepare to store the information in the outputs and assign it to "dat" below.
dat<-matrix(nrow=0,ncol=17)
#2)produce a character vector of the names of txt output files in the named directory and assign it to "files" below.
files<-list.files()

#Now write a for loop for each file 
for (i in files) {     
      # read each file in table format and create a dataframe (read.table function) and assign it to "X" below.
    X<-read.table(i)
   for (j in 1:100) {      # I provide you another for loop in order to add a new column (17th column) and add numbers to that column's rows from 1 to 100 for each file. You will use this new 17th column with numbers 1 to 100 for the x-axes in your graphs. 
       X$V17[j]<-j
   }
    #now, you should combine the X and dat matrices by rows and assign it to "dat" below.
    dat<-rbind(dat,X)
    #now, I provide you some manupilations in the data together with their explanations
    ds<-paste("",i, sep="")      #this adds " " to the name of file
    ds<-substr(ds, 1, nchar(ds)-4)#this removes the last 4 char (.txt)
    assign(ds, X)          # this assigns X to ds
}
#dat = dat[-1,]      #this deletes the first row

## now, look at your dat matrix and if you see ERROR in some cells, change all of those ERRORs to the value 0.
        
####### In order to produce the proportion of reasoning strategies graph:

# Encode the matrix named dat 's V16 column as a factor below. V16 is the column that shows the reasoning levels that was pushed to *response* function in your models (i.e, 0,1,2).
dat$V16<- as.factor(dat$V16)
# Build a contingency table (the function is called table) of the counts at each combination of V16 and V17 columns and make that table a dataframe and assign it to "y.df" below. When you use the table function together with the as.data.frame function, the y.df will have 3 columns named Var1, Var2, and Freq and 300 rows. Var1 shows the reasoning levels; Var2 shows the number of simulations (from 1 to 100, meaning that over time) 

# Create 100 by 3 matrix containing the frequencies. Rows are the trails, collumns the strategy
y.df<-matrix(0L, nrow = 100, ncol = 3, dimnames = list(c(1:100), c(0:2)))
for(trail in 1:100) {
  for(datRow in 1:length(dat$V1)) {
    if(dat$V17[datRow] == trail) {
      # Count freq
      y.df[trail, dat$V16[datRow]] <- y.df[trail, dat$V16[datRow]] + 1
    }
  }
}
# Divide the Freq column of y.df by 100 and assign to the new column called proportion below:    
y.df$proportion<-
# Encode y.df$Var1 as factor and assign it to y.df$Var1 below:   
y.df$Var1<- 
# Encode y.df$Var2 as numeric and assign it to y.df$Var2 below:      
y.df$Var2<-

# Below, write 3 lines of code in order to change the name of the levels of factor Var1 from 0 to "Zero-order"; from 1 to "First-order"; from 2 to "Second-order".          

    
## now, you can make the first line graph showing the proportion of reasoning strategies over time (Var2).

    
    
    
####### In order to produce the utility values graph:   
        
# write a for loop that takes the average of V2 column for each time point (V17) and assign it to meanVectorZero
meanVectorZero<-c(1)
for (i in 1:100) {
  meanVectorZero[i]<-        # write here if 17th column of dat is equal to 1,2,..100 get the mean of the second column      
}

# write a for loop that takes the average of V7 column for each time point (V17) and assign it to meanVectorFirst

meanVectorFirst<-c(1)
for (i in 1:100) {
  meanVectorFirst[i]<-   # write here if 17th column of dat is equal to 1,2,..100 get the mean of the seventh column
}

# write a for loop that takes the average of V12 column for each time point (V17) and assign it to meanVectorSecond

meanVectorSecond<-c(1)
for (i in 1:100) {
  meanVectorSecond[i]<-  # write here if 17th column of dat is equal to 1,2,..100 get the mean of the 12th column
} 
}

# combine meanVectorZero,meanVectorFirst and meanVectorSecond by column and assign it to utility Values below
utilityValues<-

# create a vector called time that contains integers starting from 1 to 100 and assign to time below.
time<-

# combine time and utilityValues by column and assign it to utilityValues below
utilityValues<-
  
# make utilityValues a data frame and assign it to utilityValues below
utilityValues<-


####### Make the utility values graph
  
# Before you make the graph, you need to convert the data to long data format and do couple of things. Below I provide you the code for these things.   

utilityValues_long <- gather(utilityValues, reasoning.level, utility.value, meanVectorZero:meanVectorSecond)

utilityValues_n <- utilityValues_long 
levels(utilityValues_n$reasoning.level)[levels(utilityValues_n$reasoning.level)=="meanVectorZero"] <- "Zero-order"
levels(utilityValues_n$reasoning.level)[levels(utilityValues_n$reasoning.level)=="meanVectorFirst"] <- "First-order"
levels(utilityValues_n$reasoning.level)[levels(utilityValues_n$reasoning.level)=="meanVectorSecond"] <- "Second-order"

names(utilityValues_n)[names(utilityValues_n)=="Reasoning level"]  <- "reasoning.level"

### Now, the data you will use for this graph is utilityValues_n. x is time and y is utility value. Similar to the first graph, second graph will have three lines indicating each reasoning strategies' utilities over time (1 to 100)




## Now, use multiplot function in order to present the two graphs together in 2 columns

