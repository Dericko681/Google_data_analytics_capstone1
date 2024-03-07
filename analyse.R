#DESCRIPTIVE ANALYSIS

## Analyse data to determine average trip length of riders for each month of the year using aggregate
aggregate(trip_length ~ member_casual + month, cleanridedata, mean)

## Analyse data to determine average trip length of riders on each day of the week using aggregate function
aggregate(trip_length ~ day + member_casual, cleanridedata, mean)

## Analyse data to determine how riders use the different bike types available using the aggregate function
aggregate(trip_length ~ rideable_type + member_casual, cleanridedata, mean)
