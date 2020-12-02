Ds_Data <- read.csv("C:/Users/nikol/Desktop/Ds_Data.csv")
# Grouped

matrix.response_age <- matrix(c(10,16,2,6,16,1,3,8,0,4,0,0), nrow = 4, ncol = 3, byrow = T)
chisq.test(matrix.response_age)

matrix.response_occupation <- matrix(c(17,30,3,6,10,0), nrow = 2, ncol = 3, byrow = T)
chisq.test(matrix.response_occupation)


ggplot(Ds_Data, aes(fill=Response, y=Value, x=Age_Group)) + ggtitle("Fishers and Trader Responses for Shark Catches") + 
        labs(x="Age Group", y = "Count of Responses") +
        geom_bar(position="stack", stat="identity") + theme_classic()


ggplot(Ds_Data, aes(fill=Response, y=Value_2, x=Occupation)) + ggtitle("Fishermen and Fish Trader Responses for Shark Catches") + 
        labs(x="Occupation", y = "Count of Responses") +
        geom_bar(position="stack", stat="identity") + theme_classic()



#BTS stuff

Shark_Data <- read.csv("C:/Users/nikol/Desktop/Shark_Data.csv")

#Pt1

Difference_Prop <- c(Shark_Data$Y2019-Shark_Data$Y2020)
wilcox.test(Difference_Prop, alternative = "greater")

#Pt2
Difference_Cond <- c(Shark_Data$Threshold-Shark_Data$Actual)
wilcox.test(Difference_Cond, alternative = "less")

#Pt3
model.all=lm(data=Shark_Data,Weight~Length)
summary(model.all)
cor(Shark_Data$Length,Shark_Data$Weight)


plot_2020=ggplot(data = Shark_Data, aes(x=Length, y=Weight)) +
  geom_point(aes(color = factor(Year)), shape=16, size =4) + theme_classic() + 
  annotate("text", label = "y = 0.905x - 65.58", x = 150, y = 95) +
  annotate("text", label = "R^2 = 0.93", x = 150, y = 91)+ 
  geom_smooth(method=lm, se=FALSE,col="black")+
  labs(x="Total Length (cm)", y = "Weight (kg)") +
  ggtitle("Shark Length to Weight Relationship for 2019 and 2020")

print(plot_2020)


















