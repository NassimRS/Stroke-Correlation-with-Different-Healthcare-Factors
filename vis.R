require(gdata)
require(ggmap)
require(dplyr)
require(ggplot)
require(rworldmap)
library(fiftystater)

data("fifty_states") # this line is optional due to lazy data loading
resDir <- "/Users/marbabshirani/Learning/Visualization/Results" 
dataDir <- "/Users/marbabshirani/Learning/Visualization/data"
# Obesity Date Prepration -------------------------------------------------
obesity <- read.csv(file.path(dataDir, 'Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv'))
obesity2013 <- obesity[obesity$YearStart == 2013 &
                         obesity$StratificationID1 == "OVERALL", ]
obesity2013 <- obesity2013[obesity2013$LocationAbbr != "US", ]
obesity2013$LocationDesc <- tolower(obesity2013$LocationDesc)

# Stroke Mortality Data Prepration ----------------------------------------
stroke <- read.csv(file.path(dataDir, 'Stroke_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv'))
stroke2013 <- stroke[stroke$Stratification1 == "Overall" &
                       is.na(stroke$Data_Value),  ]
strokeAgg <- aggregate(Data_Value ~ LocationAbbr, stroke, mean)
colnames(strokeAgg) <- c("LocationAbbr", "Mortality.Rate")

# Merging Datasets --------------------------------------------------------
mergedData <- merge(obesity2013, strokeAgg, by="LocationAbbr")


# Visualization by Health Question ----------------------------------------
healthQuestions <- as.character(unique(mergedData$Question))
correlationResults <- data.frame(healthQuestions)
correlationResults$Question.Summerized <- c('obesity', 'overweight', 'fruit', 'vegetable',
                                            'aerobic', 'aerobic_and_muscle_strength',
                                            'long_aerobic', 'muscle_strength', 
                                            'no_leisure_activity')
correlationResults$Question.Summerized2 <- c('obesity rate', 'overweight rate', 
                                             'less that one fruit a day', 
                                             'less than one vegetable a day',
                                            '150 min moderate aerobic per day', 
                                            '150 min aerobic and muscle strengthing',
                                            '300 min moderate aerobic per day', 
                                            'two or more muscle strengthing per week', 
                                            'no leisure physical activity')

for (i in 1:length(healthQuestions)){
  conditionData <- mergedData[mergedData$Question == healthQuestions[i], ]
  correlationResults$correlation[i] <- cor(conditionData$Data_Value, conditionData$Mortality.Rate)

  p <- ggplot(conditionData, aes(map_id = LocationDesc)) + 
    geom_map(aes(fill = Data_Value_Alt), map = fifty_states) + 
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    labs(x = "", y = "") +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) +
#    ggtitle(healthQuestions[i]) +
    ggtitle(paste(strwrap(healthQuestions[i], width = 85), collapse = "\n")) +
    theme(legend.position = "bottom", 
          panel.background = element_blank()) +
    scale_fill_continuous(name = "Percent") + 
    fifty_states_inset_boxes() 
  print(p)  
#  ggsave(file.path(resDir, paste(correlationResults$Question.Summerized[i], '.png', sep="")))
}

# Stroke Mortality Visualiztion  ------------------------------------------
p <- ggplot(mergedData, aes(map_id = LocationDesc)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = Mortality.Rate), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  ggtitle("Stroke Mortality Rate 2013") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +
  scale_fill_continuous(name = "Average Mortality per 100,000") + 
  fifty_states_inset_boxes() 
  ggsave(file.path(resDir, 'stroke_mortality.png'))
print(p)


# Correlation Bar Plot ---------------------------------------------------
g <- ggplot(correlationResults, aes(x=reorder(Question.Summerized2, -correlation), y=correlation)) + 
  geom_bar(stat="identity", aes(fill=correlation)) +
  theme(axis.text.x = element_text(angle =55, hjust = 1)) +
  labs(x = "", y = "Correlation") +
  scale_y_continuous(breaks = seq(-0.7, 0.9, by = 0.2)) +
  ggsave(file.path(resDir, 'correlation_analysis.png'))
print(g)



