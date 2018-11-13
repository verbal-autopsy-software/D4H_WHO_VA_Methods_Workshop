library(openVA)
fit_inter <- fit_ins <- NULL
for(i in 1:3){
  data <- read.csv(paste("Data/va16Data", i, "_hw.csv", sep=""))
  dim(data)
  fit_inter[[i]] <- codeVA(data = data, data.type = "WHO2016", 
                           model = "InterVA", version = "5.0", 
                           HIV = "l", Malaria = "l", directory = paste0("InterVA_output", i))
  fit_ins[[i]] <- codeVA(data = data, data.type = "WHO2016", 
                         model = "InSilicoVA", Nsim = 5000)
}

csmf1 <- getCSMF(fit_ins[[1]])
csmf2 <- getCSMF(fit_ins[[2]])
csmf3 <- getCSMF(fit_ins[[3]])

csmf.ins <- data.frame(time1 = csmf1[, "Mean"], 
                       time2 = csmf2[, "Mean"], 
                       time3 = csmf3[, "Mean"])
csmf.ins$ave <- apply(csmf.ins, 1, mean)
csmf.ins.ordered <- csmf.ins[order(csmf.ins$ave, decreasing = TRUE), ]
head(csmf.ins.ordered)

data("SampleCategory")
head(SampleCategory)
add <- rbind(c("Haemorrhagic fever (non-dengue)", "Communicable"), 
             c("Dengue fever", "Communicable"), 
             c(" Other and unspecified NCD", "NCD"))
add <- data.frame(add)
colnames(add) <- colnames(SampleCategory)
SampleCategory <- rbind(SampleCategory, add)
stackplotVA(list(time1 = fit_ins[[1]], 
                 time2 = fit_ins[[2]], 
                 time3 = fit_ins[[3]]), 
            grouping = SampleCategory)

causes <- colnames(fit_ins[[1]]$csmf)
grouping2 <- cbind(causes, causes)
stackplotVA(list(time1 = fit_ins[[1]], 
                 time2 = fit_ins[[2]], 
                 time3 = fit_ins[[3]]), 
            grouping = grouping2)
