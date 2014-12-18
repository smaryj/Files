Files
=====

###################################################
##Easy Auto Risk Analysis
##Start Date: Dec 10, 2014
###################################################

# Dec 16-Dec 18, 2014

###############################################################
##############################################

# EDA

########################################################
########################################
install.packages("stargazer")
library(stargazer)

#all variables EDA
stargazer(data1, type="html")
#########################################

# correlation table
cortable=cor(data[sapply(data, is.numeric)])
write.table(cortable, "c:/Users/Username/Desktop/cor.txt", sep="\t")

######################### Plots

#(Plot #1) scatterplot of days to repo vs. EAC points
cor(data[,19], data[,2], use="complete.obs")
plot(data[,2], data[,19], xlab="EAC Points", 
     ylab="Days to repo",
     main="Days to Rep Vs. EAC Points
     \n(Correlation=0.15)", col=rgb(1,0,0, 0.4), 
     cex.main=0.9,pch=19,cex=2, font=2,font.lab=2)
abline(lm(data[,19]~data[,2]), col="black", lwd=3) # regression line (y~x)
lines(lowess(data[,2],data[,19]), col="black", lwd=5, lty=4) # lowess line (x,y)
legend(locator(1), c("Regression Line", "Lowess Line"), cex=0.8,
       lty=c(1,4), lwd=3)
################################################################
#(Plot #2) scatterplot of days to repo vs. job-time
cor(data[,19], data[,4], use="complete.obs")
plot(data[,4], data[,19], xlab="Duration Time at Job", 
     ylab="Days to repo",
     main="Days to Rep Vs. Duration Time at Job
     \n(Correlation=0.07)", col=rgb(1,0,0, 0.4), 
     cex.main=0.9,pch=19,cex=2, font=2,font.lab=2)
abline(lm(data[,19]~data[,4]), col="black", lwd=3) # regression line (y~x)
lines(lowess(data[,4],data[,19]), col="black", lwd=5, lty=4) # lowess line (x,y)
legend(locator(1), c("Regression Line", "Lowess Line"), cex=0.8,
       lty=c(1,4), lwd=3)
########################################
#(Plot #3) scatterplot of days to repo vs. Net Income
cor(data[,19], data[,5], use="complete.obs")
plot(data[,5], data[,19], xlab="Net Income", 
     ylab="Days to repo",
     main="Days to Rep Vs. Net Income
     \n(Correlation=0.09)", col=rgb(1,0,0, 0.4), 
     cex.main=0.9,pch=19,cex=2, font=2,font.lab=2)
abline(lm(data[,19]~data[,5]), col="black", lwd=3) # regression line (y~x)
lines(lowess(data[,5],data[,19]), col="black", lwd=5, lty=4) # lowess line (x,y)
legend(locator(1), c("Regression Line", "Lowess Line"), cex=0.8,
       lty=c(1,4), lwd=3)
#################################################
#(Plot #4) scatterplot of days to repo vs. Beacon (FICO Score)
cor(data[,19], data[,20], use="complete.obs")
plot(data[,20], data[,19], xlab="Beacon (FICO Score)", 
     ylab="Days to repo",
     main="Days to Rep Vs. Beacon (FICO Score)
     \n(Correlation=0.04)", col=rgb(1,0,0, 0.4), 
     cex.main=0.9,pch=19,cex=2, font=2,font.lab=2)
abline(lm(data[,19]~data[,20]), col="black", lwd=3) # regression line (y~x)
#lines(lowess(data[,20],data[,19]), col="black", lwd=5, lty=4) # lowess line (x,y)
legend(locator(1), c("Regression Line"), cex=0.8,
       lty=c(1), lwd=3)
################################################
#(Plot #5) scatterplot of days to repo vs. Debt.ratio
cor(data[,19], data[,6], use="complete.obs")
plot(data[,6], data[,19], xlab="Debt.ratio", 
     ylab="Days to repo",
     main="Days to Rep Vs. Debt.ratio
     \n(Correlation=-0.03)", col=rgb(1,0,0, 0.4), 
     cex.main=0.9,pch=19,cex=2, font=2,font.lab=2)
abline(lm(data[,19]~data[,6]), col="black", lwd=3) # regression line (y~x)
lines(lowess(data[,6],data[,19]), col="black", lwd=5, lty=4) # lowess line (x,y)
legend(locator(1), c("Regression Line", "Lowess Line"), cex=0.8,
       lty=c(1,4), lwd=3)
#######################################################
#(Plot #6) scatterplot of days to repo vs. Payment/Income
cor(data[,19], data[,8], use="complete.obs")
plot(data[,8], data[,19], xlab="Payment/Income", 
     ylab="Days to repo",
     main="Days to Rep Vs. Payment/Income
     \n(Correlation=-0.08)", col=rgb(1,0,0, 0.4), 
     cex.main=0.9,pch=19,cex=2, font=2,font.lab=2)
abline(lm(data[,19]~data[,8]), col="black", lwd=3) # regression line (y~x)
lines(lowess(data[,8],data[,19]), col="black", lwd=5, lty=4) # lowess line (x,y)
legend(locator(1), c("Regression Line", "Lowess Line"), cex=0.8,
       lty=c(1,4), lwd=3)
##########################################
#(Plot #7) scatterplot of days to repo vs. ACV (Car Cash Value)
cor(data[,19], data[,12], use="complete.obs")
plot(data[,12], data[,19], xlab="ACV (Car Cash Value)", 
     ylab="Days to repo",
     main="Days to Rep Vs. ACV (Car Cash Value)
     \n(Correlation=0.006)", col=rgb(1,0,0, 0.4), 
     cex.main=0.9,pch=19,cex=2, font=2,font.lab=2)
abline(lm(data[,19]~data[,12]), col="black", lwd=3) # regression line (y~x)
lines(lowess(data[,12],data[,19]), col="black", lwd=5, lty=4) # lowess line (x,y)
legend(locator(1), c("Regression Line", "Lowess Line"), cex=0.8,
       lty=c(1,4), lwd=3)
###############################################
#(Plot #8) scatterplot of days to repo vs. Loan Term
cor(data[,19], data[,22], use="complete.obs")
plot(data[,22], data[,19], xlab="Loan Term", 
     ylab="Days to repo",
     main="Days to Rep Vs. Loan Term", col=rgb(1,0,0, 0.4), 
     cex.main=0.9,pch=19,cex=2, font=2,font.lab=2)
abline(lm(data[,19]~data[,22]), col="black", lwd=3) # regression line (y~x)
lines(lowess(data[,22],data[,19]), col="black", lwd=5, lty=4) # lowess line (x,y)
legend(locator(1), c("Regression Line", "Lowess Line"), cex=0.8,
       lty=c(1,4), lwd=3)
#####################################################
#(Plot #9) scatterplot of days to repo vs. Weekly Payment
cor(data[,19], data[,9], use="complete.obs")
plot(data[,9], data[,19], xlab="Weekly Payment", 
     ylab="Days to repo",
     main="Days to Rep Vs. Weekly Payment
     \n(Correlation=0.01)", col=rgb(1,0,0, 0.4), 
     cex.main=0.9,pch=19,cex=2, font=2,font.lab=2)
abline(lm(data[,19]~data[,9]), col="black", lwd=3) # regression line (y~x)
lines(lowess(data[,9],data[,19]), col="black", lwd=5, lty=4) # lowess line (x,y)
legend(locator(1), c("Regression Line", "Lowess Line"), cex=0.8,
       lty=c(1,4), lwd=3)
#####################################################
#(Plot #10) scatterplot of days to repo vs. Down Payment
cor(data[,19], data[,13], use="complete.obs")
plot(data[,13], data[,19], xlab="Down Payment", 
     ylab="Days to repo",
     main="Days to Rep Vs. Down Payment
     \n(Correlation=0.02)", col=rgb(1,0,0, 0.4), 
     cex.main=0.9,pch=19,cex=2, font=2,font.lab=2)
abline(lm(data[,19]~data[,13]), col="black", lwd=3) # regression line (y~x)
lines(lowess(data[,13],data[,19]), col="black", lwd=5, lty=4) # lowess line (x,y)
legend(locator(1), c("Regression Line", "Lowess Line"), cex=0.8,
       lty=c(1,4), lwd=3)
#####################################################






