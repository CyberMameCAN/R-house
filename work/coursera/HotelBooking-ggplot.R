library(readr)
library(ggplot2)
library(tidyverse)

hotel_bookings <- read_csv("project/coursera/GL0bk8O2Sja9G5PDtko2uQ_31e445d7ca64417eb45aeaa08ec90bf1_hotel_bookings.csv")
#View(hotel_bookings)

head(hotel_bookings)
str(hotel_bookings)
colnames(hotel_bookings)
dim(hotel_bookings)
glimpse(hotel_bookings)

table(hotel_bookings$hotel)

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  labs(title="Hotelの種類")

mindate <- min(hotel_bookings$arrival_date_year)
maxdate <- max(hotel_bookings$arrival_date_year)

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       caption=paste0("Data from: ", mindate, " to ", maxdate),
       x="Market Segment", y="Number of Bookings")

ggsave('project/coursera/hotel_booking_chart.png', width=16, height=8)

