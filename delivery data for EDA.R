#preliminaries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(geosphere)
library(farver)

#load Data
delivery_data<-read.csv("C:/Users/Computer/Documents/R data/deliverytime.csv")
View(delivery_data)
str(delivery_data)
attach(delivery_data)

#overview of data
summary(delivery_data)

#PART A: AGE VS SPEED


#transforming the data
delivery_data$distance_meters<- distHaversine(
  matrix(c(Restaurant_longitude,Restaurant_latitude), ncol = 2),
  matrix(c(Delivery_location_longitude, Delivery_location_latitude), ncol = 2)
                                )
delivery_data$Time_taken.hrs<- Time_taken.min./60
delivery_data$average_speed_kmph<- delivery_data$distance_meters/1000/delivery_data$Time_taken.hrs

#new data set
mydata<- data.frame(
  delivery_data$ID,
  delivery_data$Delivery_person_ID,
  delivery_data$Delivery_person_Age,
  delivery_data$Delivery_person_Ratings,
  delivery_data$Type_of_order,
  delivery_data$Type_of_vehicle,
  delivery_data$average_speed_kmph
  )

#renaming the variables
mydata<- mydata%>%
  rename(
    ID = delivery_data.ID,
    delivery_person_ID = delivery_data.Delivery_person_ID,
    age = delivery_data.Delivery_person_Age,
    rating = delivery_data.Delivery_person_Ratings,
    order_type = delivery_data.Type_of_order,
    vehicle_type = delivery_data.Type_of_vehicle,
    speed = delivery_data.average_speed_kmph
  )

View(mydata)
detach(delivery_data)
attach(mydata)

#analyzing the data
summary(age)
summary(speed) #some unrealistically high values are observed in the data
quantile(speed, 0.975) #97.5% of speeds < 71.24 kmph


#new data set
new_data<- filter(mydata, speed < 71.24)
detach(mydata)
attach(new_data)
View(new_data)
summary(speed)

#a relationship between age and speed

komparison_0<-ggplot(data = new_data, aes(x = age, y = speed))+
  geom_point(size = 0.5, alpha = 0.6)+
  geom_smooth(method = "lm", color = "blue", se = TRUE)+
  theme_minimal()+
  labs(title = "age vs speed",
       x = "delivery person age",
       y = "delivery person speed")

print(komparison_0) #there appears to be a slight negative relationship

cor.test(age, speed) #there is strong evidence of a slight negative relationship

#a relationship between speed and age as per vehicle type

komparison<-ggplot(new_data, aes(x = age,
                     y = speed)) + 
  geom_point(size = 0.5, alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  facet_wrap(~ vehicle_type) +
  labs(title = "Age and Delivery Speed by Vehicle",
       x = "Age of Delivery Person (years)",
       y = "Average Speed (km/h)") +
  theme_minimal()
print(komparison) #suggests slight negative relationship over all vehicles

cor_test_summary <- function(x, y) {
  result <- cor.test(x, y)
  return(
    data.frame(correlation = result$estimate,
               p_value = result$p.value)
  )
}
summary <- new_data %>%
  group_by(vehicle_type) %>%
  summarize(cor_summary = list(cor_test_summary(age, speed))) %>%
  unnest(cols = c(cor_summary))
print(summary) #strong evidence of a slight negative relationship over all vehicles
               #bicycles having the most negative of the relationships   



#PART B: RATINGS VS SPEED

#analysis of ratings data
summary(rating)

#a relationship between rating and speed
Comparison_0<-ggplot(data = new_data, aes(x = rating, y = speed))+
  geom_point(size = 0.5, alpha = 0.6)+
  geom_smooth(method = "lm", color = "blue", se = TRUE)+
  theme_minimal()+
  labs(title = "ratings vs speed",
       x = "delivery person ratings",
       y = "delivery person speed")

print(Comparison_0) #there appears to be a moderate positive relationship 

cor.test(rating, speed) #there is strong evidence of a small positive relationship

#a relationship between rating and speed by order type

comparison<-ggplot(data = new_data, aes(x = rating,
                                   y  = speed))+ 
  geom_point(size=0.5, alpha=0.5)+
  facet_wrap(~order_type)+
  geom_smooth(method = "lm", color = "blue", se = TRUE)+
  labs(title = "ratings and speed",
       x = "delivery person rating",
       y = "delivery perosn speed")+
  theme_minimal()
print(comparison) #the rating does seem to increase with speed for all order types

summary1 <- new_data %>%
  group_by(order_type) %>%
  summarize(cor_summary = list(cor_test_summary(rating, speed))) %>%
  unnest(cols = c(cor_summary))

print(summary1) #there exists strong evidence of a very small positive correlation 

