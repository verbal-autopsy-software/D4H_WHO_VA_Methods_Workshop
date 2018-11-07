# Prepare data to clean as an example in presentation

## load data & InterVA5 (for names)
library(InterVA5)
getwd()
dir("../../../Data")
afgData <- read.csv("../../../Data/va16Data8_errors.csv")
names(afgData)

## grab subset of variables
keepCols <- c(1:12, 21:26, 60:65, 75:84)
afgData <- afgData[ , keepCols]
dim(afgData)
summary(afgData)
apply(afgData, 2, table)

data(probbaseV5)
probbaseV5[ , c(1,3)]
probbaseV5[ , 2]

## add useful names
newNames <- probbaseV5[keepCols, 3]
newNames <- gsub("\\+", "", newNames)
newNames <- gsub("\\-", "_", newNames)
newNames <- gsub(" ", "_", newNames)
newNames <- gsub("<", "less", newNames)
newNames

names(afgData) <- newNames
names(afgData)[1] <- "ID"
names(afgData)

## add more errors
scramble <- function(vaData, errors = 1:4, numErrors = c(10, 10, 10, 10), seed = 1){

    ## vaData = ams2010[1001:1100,]

    set.seed(seed)
    out <- apply(vaData, 2, as.character)
    n <- numErrors
    numCols <- ncol(out)
    numRows <- nrow(out)

    if(length(n) != length(errors)){
        n = rep(n[1], length(errors))
        cat("\n Warning: errors and numErrors are not the same length.
             \n Using numErrors[1] for all errors.\n ")
    }


    # error 1: include "Yes" & "No" (as opposed to "Y" and "N")
    if (1 %in% errors){

        indexError1 <- which(errors == 1)
        if(numRows < n[indexError1]) n[indexError1] <- numRows

        n0 <- floor(n[indexError1]/2)
        n1 <- n[indexError1] - n0

        ## leave out$ID alone
        cols_with_yes_errors <- sample(2:numCols, size = n1, replace = FALSE)
        rows_with_yes_errors <- sample(1:numRows, size = n1, replace = FALSE)

        cols_with_no_errors <- sample(2:numCols, size = n0, replace = FALSE)
        rows_with_no_errors <- sample(1:numRows, size = n0, replace = FALSE)

        out[rows_with_yes_errors, cols_with_yes_errors] <- "Yes"
        out[rows_with_no_errors, cols_with_no_errors] <- "No"
    }

    # error 2: include NAs
    if (2 %in% errors){

        indexError2 <- which(errors == 2)
        if(numRows < n[indexError2]) n[indexError2] <- numRows

        ## leave out$ID alone
        cols_with_NA_errors <- sample(2:numCols, size = n[indexError2],
                                      replace = FALSE)
        rows_with_NA_errors <- sample(1:numRows, size = n[indexError2],
                                      replace = FALSE)

        out[rows_with_NA_errors, cols_with_NA_errors] <- NA
    }

    # error 3: insert "1" & "0" (along with "Y" and "N")
    if (3 %in% errors){

        indexError3 <- which(errors == 3)
        if(numRows < n[indexError3]) n[indexError3] <- numRows

        n0 <- floor(n[indexError3]/2)
        n1 <- n[indexError3] - n0

        ## leave out$ID alone
        cols_with_1_errors <- sample(2:numCols, size = n1, replace = FALSE)
        rows_with_1_errors <- sample(1:numRows, size = n1, replace = FALSE)

        cols_with_0_errors <- sample(2:numCols, size = n0, replace = FALSE)
        rows_with_0_errors <- sample(1:numRows, size = n0, replace = FALSE)

        out[rows_with_1_errors, cols_with_1_errors] <- "1"
        out[rows_with_0_errors, cols_with_0_errors] <- "0"
    }

    # error 4: insert "Don't Know" & "DK" (along with "Y" and "N")
    if (4 %in% errors){

        indexError4 <- which(errors == 4)
        if(numRows < n[indexError4]) n[indexError4] <- numRows

        n0 <- floor(n[indexError4]/2)
        n1 <- n[indexError4] - n0

        ## leave out$ID alone
        cols_with_DK1_errors <- sample(2:numCols, size = n1, replace = FALSE)
        rows_with_DK1_errors <- sample(1:numRows, size = n1, replace = FALSE)

        cols_with_DK2_errors <- sample(2:numCols, size = n0, replace = FALSE)
        rows_with_DK2_errors <- sample(1:numRows, size = n0, replace = FALSE)

        out[rows_with_DK1_errors, cols_with_DK1_errors] <- "Don't Know"
        out[rows_with_DK2_errors, cols_with_DK2_errors] <- "DK"
    }

    out <- data.frame(out, stringsAsFActors = TRUE)
    out$ID <- vaData$ID

    return(out)

}

afgData2 <- scramble(afgData)
names(afgData2)
afgData2 <- afgData2[ , -ncol(afgData2)]
names(afgData2)
names(afgData2) <- gsub("^X", "age_", names(afgData2))
names(afgData2)
apply(afgData2, 2, table)

## add some logical inconsistencies
#### d_wet/d_dry and male /female
table(afgData2[,2], afgData2[,3])

afgData2[c(2, 8, 18, 24, 33, 49), 2:3] <- "Y"
table(afgData2[,2], afgData2[,3])

levels(afgData2[,4]) <- c(levels(afgData2[,4]), "1", "Y")
levels(afgData2[,5]) <- c(levels(afgData2[,5]), "yes", "YES")

afgData2[c(6, 12, 40), 4] <- "Y"
afgData2[c(22, 31), 4] <- "1"
afgData2[c(6, 22, 31), 5] <- "yes"
afgData2[c(12, 22, 40), 5] <- "YES"
table(afgData2[,4], afgData2[,5])

#### ages
names(afgData2)

levels(afgData2[,7]) <- c(levels(afgData2[,7]), "1", "YES")
levels(afgData2[,9]) <- c(levels(afgData2[,9]), "Yes", "y")

afgData2[c(1, 18, 40), 7] <- "YES"
afgData2[c(21, 35), 7] <- "1"
afgData2[c(1, 21, 35), 9] <- "Yes"
afgData2[c(18, 40), 9] <- "y"
table(afgData2[,7], afgData2[,9])

#### "Did (s)he suffer from any injury or accident that led to her/his death?"
table(afgData2[,13])
afgData2[,13] <- "Y"
table(afgData2[,13])

#### "Was there any diagnosis by a health professional of high blood pressure?"
levels(afgData2[,21]) <- c(levels(afgData2[,21]), "23", "88", "10", "41")
afgData2[c(2, 9, 28, 25, 17, 44), 21] <- c("23", "88", "10", "41", "41", "88")

#### cols 24:29 -- 24 was fever present? and 25-29 are duration
##### present but no duration
afgData2[11, 24] <- "Y"
levels(afgData2[,25]) <- c(levels(afgData2[,25]), "N")
levels(afgData2[,26]) <- c(levels(afgData2[,26]), "N")
levels(afgData2[,27]) <- c(levels(afgData2[,27]), "N")
levels(afgData2[,28]) <- c(levels(afgData2[,28]), "N")
levels(afgData2[,29]) <- c(levels(afgData2[,29]), "N")
afgData2[11, 25:29] <- "N"

##### not present but duration
levels(afgData2[,24]) <- c(levels(afgData2[,24]), "no")
levels(afgData2[,28]) <- c(levels(afgData2[,28]), "Y")
levels(afgData2[,29]) <- c(levels(afgData2[,29]), "Y")
afgData2[c(20, 45), 24] <- c("-", "no")
afgData2[20, 26] <- "Y"
afgData2[45, 28:29] <- "Y"

#### col 34: duration of cough
levels(afgData2[,33]) <- c(levels(afgData2[,33]), "Y")
index <- sample(1:nrow(afgData2), 28)
afgData2[index, 33] <- "Y"
afgData2[,34] <- as.numeric(afgData2[,34])
afgData2[afgData2[,33] == "-", 34] <- NA
for (i in 1:28) {
    afgData2[index[i], 34] <- rpois(1, 20)
    if(is.element(i, c(5, 19, 23))){
        afgData2[index[i], 34] <- rpois(1, 20000)
    }
}
afgData2[index[11], 34] <- -43

#### observations with all missing
afgData2[c(26, 32, 41), ] <- "-"

## Longer description of question (for different CSV file)
questionDesc <- probbaseV5[keepCols, 2]
questionDesc[1] <- "ID"
questionDesc[34] <- "How many days did the cough last?"
questionDesc

write.csv(afgData2, "small_vaAfghanistan.csv", row.names = FALSE)
write.csv(questionDesc, "small_vaAfghanistan_quesions.csv", row.names = FALSE)
