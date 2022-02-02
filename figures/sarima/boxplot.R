library(ggplot2)

setwd("C:/Users/jlcah/Documents/caisprojectx/projectx2021/figures/sarima")

data <- read.csv("SARIMA_reshape.csv") #####this reads in the data file I made you will need to change the path to your computer

View(data)

p <- ggplot(data, aes(x=reorder(district, absolute_error, FUN = median), y=absolute_error, fill=reg)) + 
  geom_boxplot(outlier.shape = NA)+
  xlab("Districts Ranked by Median Percent Absolute Error")+
  ylab("Percent Absolute Error")+
  labs("Regressors")+
  ylim(0,150)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Impact of Exogenous Regressors on SARIMA") 
p

median = c()
mean = c()
sd=c()
for (i in unique(data$district)) {
  median=append(median,median(data$absolute_error[data$district==i & data$reg=="Regressors_Excluded"] ))
  mean=append(mean,mean(data$absolute_error[data$district==i & data$reg=="Regressors_Excluded"]))
  sd = append(sd,sd(data$absolute_error[data$district==i & data$reg=="Regressors_Excluded"]))
}

l <- list(district = unique(data$district),
             median = median,
             average = mean,
             st_dev=sd)
df <- as.data.frame(l)
print(df)
write.csv(df,"summary_regExcluded.csv")

median = c()
mean = c()
sd=c()
for (i in unique(data$district)) {
  median=append(median,median(data$absolute_error[data$district==i & data$reg=="Regressors_Included"] ))
  mean=append(mean,mean(data$absolute_error[data$district==i & data$reg=="Regressors_Included"]))
  sd = append(sd,sd(data$absolute_error[data$district==i & data$reg=="Regressors_Included"]))
}

l <- list(district = unique(data$district),
          median = median,
          average = mean,
          st_dev=sd)
df <- as.data.frame(l)
print(df)
write.csv(df,"summary_regIncluded.csv")