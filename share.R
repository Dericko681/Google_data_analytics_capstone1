#group and visualize rider type by day of the week
mc_week <- cleanridedata %>% group_by(day, member_casual) %>%
	  count()
  ggplot(data=mc_week)+
	    geom_col(mapping=aes(x=day, y=n, fill=member_casual))+
	      facet_grid(~member_casual)+
	        labs(y="Trip length", x="Rider type", fill="Member/Casual",
		            title="Ride length by day of the week")+
  theme(axis.text.x=element_text(angle=45))

# Vizualization

## Group, summarize and visualize data to determine ride count by rider type
mcdata <- cleanridedata %>% group_by(member_casual) %>% count()
ggplot(data=mcdata)+
	  geom_col(mapping=aes(x=member_casual, y=n, fill=member_casual))+
	    labs(y="Trip length", x="Rider type", fill="Member/Casual",
		        title="Number of trips by different rider types")

## Summarize and visualize data by rider type and average trip length to ascertain lenght of ride taken by different rider types
trip_len <- cleanridedata %>% group_by(member_casual) %>% summarise(mean_trip_length = mean(trip_length, na.rm = TRUE))
ggplot(data=trip_len)+
	  geom_col(mapping=aes(x=member_casual, y=mean_trip_length, fill=member_casual))+
	    labs(y="Trip length", x="Rider type", fill="Member/Casual",
		        title="Lenght of ride taken by different rider types")

## Group and visualize data by member_casual and rideable_type to determine prefered type of bike by riders
ride_type <- cleanridedata %>% group_by(member_casual, rideable_type) %>%
	  count()
  ggplot(data=ride_type)+
	    geom_col(mapping=aes(x=rideable_type, y=n, fill=member_casual))+
	      facet_wrap(~member_casual)+
	        labs(y="Number of rides", x="Type of bike", fill="Member/Casual",
		            title="Most prefered type of bike by riders")


                              # TOP 10 STATIONS

  ## Creat a new data frame for all stations
  all_stations <- bind_rows(data.frame("stations" = cleanridedata$start_station_name, 
				                                            "member_casual" = cleanridedata$member_casual),
			                              data.frame("stations" = cleanridedata$end_station_name,
								                                      "member_casual" = cleanridedata$member_casual))

  # Exclude entries with no station name
  all_stations_v2 <- all_stations[!(all_stations$stations == "" | is.na(all_stations$stations)),]

  # Separate the data frame by rider type
  all_stations_member <- all_stations_v2[all_stations_v2$member_casual == 'member',]
  all_stations_casual <- all_stations_v2[all_stations_v2$member_casual == 'casual',]


  # Get the top 10 popular stations all, members and casual riders
  top_10_station <- all_stations_v2 %>% 
	    group_by(stations) %>% 
	      summarise(station_count = n()) %>% 
	        arrange(desc(station_count)) %>% 
		  slice(1:10)

	  ## Viz
	  ggplot(data=top_10_station)+
		    geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
		      labs(title="Top 10 stations", subtitle = "Most populous bike stations.")+
		        theme(axis.text = element_text(angle=45))

		# Get the top 10 popular stations for members
		top_10_stations_member <- all_stations_member %>% 
			  group_by(stations) %>% 
			    summarise(station_count = n()) %>% 
			      arrange(desc(station_count)) %>% 
			        head(n=10)

			## Viz
			ggplot(data=top_10_stations_member)+
				  geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
				    labs(title="Top 10 stations", subtitle = "Most populous bike stations amongst members.")+
				      theme(axis.text = element_text(angle=45))

			      # Get the top 10 popular stations for members
			      top_10_stations_casual <- all_stations_casual %>% 
				        group_by(stations) %>% 
					  summarise(station_count = n()) %>% 
					    arrange(desc(station_count)) %>% 
					      head(n=10)

				      ## Viz
				      ggplot(data=top_10_stations_casual)+
					        geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
						  labs(title="Top 10 stations", subtitle = "Most populous bike stations amongst members.")+
						    theme(axis.text = element_text(angle=45))

