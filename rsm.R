install.packages("rsm")
library(rsm)
data <- read_xlsx("C:/Users/sa/Downloads/Experiment Data.xlsx")
head(data)
str(data)
#Building a model for The Respose variable

response_model<-rsm(Response ~ SO(X1,X2,X3,X4),data = data)

summary(response_model)
#residual plots 





#contour plots 
par(mfrow = c(1,1))
contour(response_model,~ X1+X2,
        image = T)
points(data$Temp,data$Agitation)

persp(response_model,X1~X2,col = terrain.colors(50),contours = "colors",zlab = "Yield",xlabs = c("Time(min)","Temperature(c)"))


contour(response_model,~ Temp+Agitation,
        image = T)


model_response<-rsm(Response ~ SO(Agitation,Temp,Adsorbent dosage,Initial dye conc), data = data)

names(data)
data$Adsorbent dosage<-data$Adsorbent_dosage


library(readxl)
library(rsm)
data1 <- read_xlsx("C:/Users/sa/Downloads/Experiment Data.xlsx")
head(data1)
model_response<-rsm(Response ~ SO(Agitation,Temp,Adsorbent_dosage,Initial_dye_conc), data = data1)



Plot1 <-contour(model_response,~ Agitation+Temp,
                image = T,title("Contour Plot Of Adsorption Vs Temperature,Temperature")


plot2 <-contour(model_response,~ Agitation+Adsorbent_dosage,
                image = T,title("Contour Plot Of Adsorption Vs Temperature,Adsorbent Dosage")

plot3<-contour(model_response,~ Agitation+Initial_dye_conc,
               image = T,title("Contour Plot Of Adsorption Vs Temperature,Initial Dye Conc")
               
 plot4<-contour(model_response,~ Temp+Adsorbent_dosage,
            image = T,title("Contour Plot Of Adsorption Vs Temperature,Adsorbent_dosage")
            

plot5<-contour(model_response,~ Temp+Initial_dye_conc,
          image = T,title("Contour Plot Of Adsorption Vs Temperature,Initial_dye_conc")
          
          

plot6<-contour(model_response,~ Initial_dye_conc + Adsorbent dosage,
      image = T,title("Contour Plot Of Adsorption Vs Temperature,Initial Dye Conc")
                         
                              
      persp(model_response,Agitation~Temp,col = terrain.colors(50),contours = "colors",zlab = "Yield",xlabs = c("Agitation","Temperature"))  
               
#perpetual Plots              
      persp(model_response,Agitation~Temp,col = terrain.colors(50),contours = "colors",zlab = "Response",xlabs = c("Agitation","Temperature"))  
      
      plot_p2<-persp(model_response,Agitation~Initial_dye_conc,col = terrain.colors(50),contours = "colors",zlab = "Response",xlabs = c("Agitation","Initial Dye Conc"))  
      
      Plot_p3<-persp(model_response,Agitation~Adsorbent_dosage,col = terrain.colors(50),contours = "colors",zlab = "Response",xlabs = c("Agitation","Adsorbent Dosage"))  
      
      plot_p4<-persp(model_response,Temp~	Adsorbent_dosage,col = terrain.colors(50),contours = "colors",zlab = "Response",xlabs = c("Temperature","Adsorbent_dosage"))  
      