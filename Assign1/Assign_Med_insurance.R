
## Make a plot that answers the question: what is the relationship 
## between mean covered charges (Average.Covered.Charges) 
## and mean total payments (Average.Total.Payments) in New York?

library(sqldf)

data_set <- read.csv("plot_exer.csv")

colnames(data_set)<-gsub("\\.","_",colnames(data_set))


data_plot <- sqldf("select Average_Covered_Charges ,Average_Total_Payments,Provider_State  from data_set where Provider_State = 'NY' ")

pdf("plot1.pdf")
with(data_plot,plot(Average_Covered_Charges,Average_Total_Payments,xlab = "Average Covered Charges (in $)",ylab = "Average Total Payments (in $)", main = "Relationship between Charges and Payments in New York", col = adjustcolor("blue", alpha = 0.5), pch = 16))
abline(lm(data_plot$Average_Total_Payments~ data_plot$Average_Covered_Charges), col = "red", lwd = 1.5)

dev.off()





## Make a plot (possibly multi-panel) that answers the question: 
## how does the relationship between mean covered charges (Average.Covered.Charges) 
## and mean total payments (Average.Total.Payments) vary by medical condition (DRG.Definition) 
## and the state in which care was received (Provider.State)?


DRG <- unique(data_set$DRG_Definition)
states <- unique(data_set$Provider_State)
pdf("plot2.pdf")
par(mfrow = c(6,6), oma = c(4,4,4,2), mar = rep(2,4))

for (i in states)
{ for (j in DRG)
{    with(subset(data_set, data_set$Provider_State == i & data_set$DRG_Definition == j),
          plot(Average_Covered_Charges,Average_Total_Payments, 
              col = adjustcolor("blue", alpha = 0.5),
               pch = 16, main = paste(i,j),
               ylim = range(Average_Total_Payments),
              xlim = range(Average_Covered_Charges)
         
          )
) 
       abline(lm(Average_Total_Payments~Average_Covered_Charges,
                subset(data_set, Provider_State == i & DRG_Definition == j)), 
            col = "red")
}
}

dev.off()

