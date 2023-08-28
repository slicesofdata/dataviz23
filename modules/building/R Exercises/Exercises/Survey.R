

## Read in the Survey 

Survey <- read.csv("SurveyQuestions.csv")
Survey <- Survey[-c(2,3,4,5,6,7,8,9,10,11,12, 58, 59, 60)]
Survey = Survey[-1,]

View(Survey)
attach(Survey)

Survey$Q1 = Survey$Q1 - 1

##Recoding Variables


Survey$Q1 <- ifelse(Survey$Q1 == 6, 0, ifelse(Survey$Q1 == 1, 1, ifelse(Survey$Q1 == 2, 2, ifelse(Survey$Q1 == 3, 3, ifelse(Survey$Q1 == 4, 4, ifelse(Survey$Q1 == 5, 5, NA))))))
Survey$Q2 <- ifelse(Survey$Q2 == 1, 0, ifelse(Survey$Q2 == 2, 1, NA))
Survey$Q3 <- ifelse(Survey$Q3 == 7, 0, ifelse(Survey$Q3 == 1, 1, ifelse(Survey$Q3 == 2, 2, ifelse(Survey$Q3 == 3, 3, ifelse(Survey$Q1 == 4, 4, ifelse(Survey$Q1 == 5, 5, NA))))))
Survey$Q4 <- ifelse(Survey$Q4 == 3, 0, ifelse(Survey$Q4 == 1, 1, ifelse(Survey$Q4 == 2, 2, NA)))
Survey$Q5 <- ifelse(Survey$Q5 == 1, 0, ifelse(Survey$Q5 == 2, 1, ifelse(Survey$Q5 == 3, 2, ifelse(Survey$Q5 == 4, 3, ifelse(Survey$Q5 == 5, 4, ifelse(Survey$Q5 == 6, 5, NA))))))
Survey$Q6 <- ifelse(Survey$Q6 == 1, 0, ifelse(Survey$Q6 == 2, 1, ifelse(Survey$Q6 == 3, 2, ifelse(Survey$Q6 == 4, 3, ifelse(Survey$Q6 == 5, 4, ifelse(Survey$Q6 == 6, 5, ifelse(Survey$Q6 == 7, 6, NA)))))))
Survey$Q7 <- ifelse(Survey$Q7 == 1, 0, ifelse(Survey$Q7 == 2, 1, ifelse(Survey$Q7 == 3, 2, ifelse(Survey$Q7 == 4, 3, ifelse(Survey$Q7 == 5, 4, ifelse(Survey$Q7 == 6, 5, ifelse(Survey$Q7 == 7, 6, NA)))))))
Survey$Q8 <- ifelse(Survey$Q8 == 1, 0, ifelse(Survey$Q8 == 2, 1, ifelse(Survey$Q8 == 3, 2, ifelse(Survey$Q8 == 4, 3, ifelse(Survey$Q8 == 5, 4, ifelse(Survey$Q8 == 6, 5, ifelse(Survey$Q8 == 7, 6, ifelse(Survey$Q8 == 8, 7, NA))))))))
Survey$Q9 <- ifelse(Survey$Q9 == 1, 0, ifelse(Survey$Q9 == 2, 1, ifelse(Survey$Q9 == 3, 2, ifelse(Survey$Q9 == 4, 3, ifelse(Survey$Q9 == 5, 4, ifelse(Survey$Q9 == 6, 5, NA))))))
Survey$Q10 <- ifelse(Survey$Q10 == 1, 0, ifelse(Survey$Q10 == 2, 1, ifelse(Survey$Q10 == 3, 2, ifelse(Survey$Q10 == 4, 3, ifelse(Survey$Q10 == 5, 4, NA)))))
Survey$Q11 <- ifelse(Survey$Q11 == 1, 0, ifelse(Survey$Q11 == 2, 1, ifelse(Survey$Q11 == 3, 2, ifelse(Survey$Q11 == 4, 3, ifelse(Survey$Q11 == 5, 4, ifelse(Survey$Q11 == 6, 5, ifelse(Survey$Q11 == 7, 6, NA)))))))
Survey$Q12 <- ifelse(Survey$Q12 == 1, 0, ifelse(Survey$Q12 == 2, 1, ifelse(Survey$Q12 == 3, 2, ifelse(Survey$Q12 == 4, 3,  NA))))
Survey$Q13 <- ifelse(Survey$Q13 == 1, 0, ifelse(Survey$Q13 == 2, 1, ifelse(Survey$Q13 == 3, 2, ifelse(Survey$Q13 == 4, 3,  NA))))
Survey$Q14 <- ifelse(Survey$Q14 == 1, 0, ifelse(Survey$Q14 == 2, 1, ifelse(Survey$Q14 == 3, 2, ifelse(Survey$Q14 == 4, 3,  NA))))
Survey$Q15 <- ifelse(Survey$Q15 == 1, 0, ifelse(Survey$Q15== 2, 1, ifelse(Survey$Q15 == 3, 2, ifelse(Survey$Q15 == 4, 3, ifelse(Survey$Q15 == 5, 4, ifelse(Survey$Q15 == 6, 5, ifelse(Survey$Q15 == 7, 6, NA)))))))
Survey$Q16 <- ifelse(Survey$Q16 == 1, 0, ifelse(Survey$Q16 == 2, 1, ifelse(Survey$Q16 == 3, 2, NA)))
Survey$Q17 <- ifelse(Survey$Q17 == 1, 0, ifelse(Survey$Q17 == 2, 1, ifelse(Survey$Q17 == 3, 2, ifelse(Survey$Q17 == 4, 3, ifelse(Survey$Q17 == 5, 4, NA)))))
Survey$Q37 <- ifelse(Survey$Q37 == 1, 0, ifelse(Survey$Q37 == 2, 1, ifelse(Survey$Q37 == 3, 2, ifelse(Survey$Q37 == 4, 3, ifelse(Survey$Q37 == 5, 4, ifelse(Survey$Q37 == 6, 5, NA))))))
Survey$Q37 <- ifelse(Survey$Q37 == 1, 0, ifelse(Survey$Q37 == 2, 1, ifelse(Survey$Q37 == 3, 2, ifelse(Survey$Q37 == 4, 3, ifelse(Survey$Q37 == 5, 4, ifelse(Survey$Q37 == 6, 5, NA))))))
Survey$Q39_1 <- ifelse(Survey$Q39_1 == 1, 0, ifelse(Survey$Q39_1 == 2, 1, ifelse(Survey$Q39_1 == 3, 2, ifelse(Survey$Q39_1 == 4, 3, ifelse(Survey$Q39_1 == 5, 4,  NA)))))
Survey$Q39_2 <- ifelse(Survey$Q39_2 == 1, 0, ifelse(Survey$Q39_2 == 2, 1, ifelse(Survey$Q39_2 == 3, 2, ifelse(Survey$Q39_2 == 4, 3, ifelse(Survey$Q39_2 == 5, 4,  NA)))))                   
Survey$Q39_3 <- ifelse(Survey$Q39_3 == 1, 0, ifelse(Survey$Q39_3 == 2, 1, ifelse(Survey$Q39_3 == 3, 2, ifelse(Survey$Q39_3 == 4, 3, ifelse(Survey$Q39_3 == 5, 4,  NA)))))
Survey$Q39_4 <- ifelse(Survey$Q39_4 == 1, 0, ifelse(Survey$Q39_4 == 2, 1, ifelse(Survey$Q39_4 == 3, 2, ifelse(Survey$Q39_4 == 4, 3, ifelse(Survey$Q39_4 == 5, 4,  NA)))))
Survey$Q39_5 <- ifelse(Survey$Q39_5 == 1, 0, ifelse(Survey$Q39_5 == 2, 1, ifelse(Survey$Q39_5 == 3, 2, ifelse(Survey$Q39_5 == 4, 3, ifelse(Survey$Q39_5 == 5, 4,  NA))))) 
Survey$Q40 <- ifelse(Survey$Q40 == 1, 0, ifelse(Survey$Q40 == 2, 1, ifelse(Survey$Q40 == 3, 2, ifelse(Survey$Q40 == 4, 3, ifelse(Survey$Q40 == 5, 4,  NA))))) 
Survey$Q41 <- ifelse(Survey$Q41 == 1, 0, ifelse(Survey$Q41 == 2, 1, ifelse(Survey$Q41 == 3, 2, ifelse(Survey$Q41 == 4, 3, ifelse(Survey$Q41 == 5, 4,  NA)))))                 
Survey$Q42 <- ifelse(Survey$Q40 == 1, 0, ifelse(Survey$Q42 == 2, 1, ifelse(Survey$Q42 == 3, 2, ifelse(Survey$Q42 == 4, 3, ifelse(Survey$Q42 == 5, 4,  NA))))) 
                     
##Adding Levels to the variables
names(Survey)
#Question 1
names(Survey)[names(Survey) == "Q1"] <- "PolParty"
levels(Survey$Q1)[1] <- " 0 - No Affiliation"
levels(Survey$Q1)[2] <- " 1 - Democrat"
levels(Survey$Q1)[3] <- " 2 - Independent"
levels(Survey$Q1)[4] <- " 3 - Libertarian"
levels(Survey$Q1)[5] <- " 4 - Republican"
levels(Survey$Q1)[6] <- " 5 - Green"
Survey$Q1

#Question 2
names(Survey)[names(Survey) == "Q2"] <- "NonFiction"
levels(Survey$Q2)[1] <- "0 - No"
levels(Survey$Q2)[2] <- "1 - Yes"
Survey$Q2
                          
#Question 3
names(Survey)[names(Survey) == "Q3"] <- "Religion"
levels(Survey$Q3)[1] <- "0 - Other"
levels(Survey$Q3)[2] <- "1 - Christian"
levels(Survey$Q3)[3] <- "2 - Jewish"
levels(Survey$Q3)[4] <- "3 - Buddhist"
levels(Survey$Q3)[5] <- "4 - Muslim"
levels(Survey$Q3)[6] <- "5 - Agnostic"
levels(Survey$Q3)[7] <- "6 - Atheist"

#Question 4
names(Survey)[names(Survey) == "Q4"] <- "Dress"
levels(Survey$Q4)[1] <- "0 - I don't know"
levels(Survey$Q4)[2] <- "1 - White and Gold"
levels(Survey$Q4)[3] <- "2 - Blue and Black"

#Question 5
names(Survey)[names(Survey) == "Q5"] <- "Superpower"
levels(Survey$Q5)[1] <- "0 - Mind Reading"
levels(Survey$Q5)[2] <- "1 - Invisibility"
levels(Survey$Q5)[3] <- "2 - Flight"
levels(Survey$Q5)[4] <- "3 - Shape-Shifting"
levels(Survey$Q5)[5] <- "4 - Telekinesis"
levels(Survey$Q5)[6] <- "5 - Super Speed"

#Question 6
names(Survey)[names(Survey) == "Q6"] <- "Fight"
levels(Survey$Q6)[1] <- "0 - Wolf pack of 10"
levels(Survey$Q6)[2] <- "1 - 500 Wasps"
levels(Survey$Q6)[3] <- "2 - Highly trained military veteran"
levels(Survey$Q6)[4] <- "3 - Three Lions"
levels(Survey$Q6)[5] <- "4 - Two Gorillas"
levels(Survey$Q6)[6] <- "5 - Donald Trump"
levels(Survey$Q6)[7] <- "6 - Other"

#Question 7
names(Survey)[names(Survey) == "Q7"] <- "GlobalIssue"
levels(Survey$Q7)[1] <- "0 - Climate Change"
levels(Survey$Q7)[2] <- "1 - Terrorism"
levels(Survey$Q7)[3] <- "2 - Poverty"
levels(Survey$Q7)[4] <- "3 - Education"
levels(Survey$Q7)[5] <- "4 - Food Scarcity"
levels(Survey$Q7)[6] <- "5 - Financial Collapse"
levels(Survey$Q7)[7] <- "6 - Other"

#Question 8
names(Survey)[names(Survey) == "Q8"] <- "MovGenre"
levels(Survey$Q8)[1] <- "0 - Comedy"
levels(Survey$Q8)[2] <- "1 - Romance"
levels(Survey$Q8)[3] <- "2 - Horror"
levels(Survey$Q8)[4] <- "3 - Science Fiction"
levels(Survey$Q8)[5] <- "4 - Documentary"
levels(Survey$Q8)[6] <- "5 - Action"
levels(Survey$Q8)[7] <- "6 - Drama"
levels(Survey$Q8)[8] <- "7 - Not Interested"

#Question 9
names(Survey)[names(Survey) == "Q9"] <- "Music"
levels(Survey$Q9)[1] <- "0 - iTunes"
levels(Survey$Q9)[2] <- "1 - Apple Music"
levels(Survey$Q9)[3] <- "2 - Spotify"
levels(Survey$Q9)[4] <- "3 - Soundcloud"
levels(Survey$Q9)[5] <- "4 - Tidal"
levels(Survey$Q9)[6] <- "5 - Pandora"
levels(Survey$Q9)[7] <- "6 - Other"

#Question 10
names(Survey)[names(Survey) == "Q10"] <- "Electronic"
levels(Survey$Q10)[1] <- "0 - Smartphone"
levels(Survey$Q10)[2] <- "1 - Laptop"
levels(Survey$Q10)[3] <- "2 - Television"
levels(Survey$Q10)[4] <- "3 - Gaming Console"
levels(Survey$Q10)[5] <- "4 - Other"


#Question 11
names(Survey)[names(Survey) == "Q11"] <- "Sport"
levels(Survey$Q11)[1] <- "0 - Football"
levels(Survey$Q11)[2] <- "1 - Soccor"
levels(Survey$Q11)[3] <- "2 - Baseball"
levels(Survey$Q11)[4] <- "3 - Basketball"
levels(Survey$Q11)[5] <- "4 - Volleyball"
levels(Survey$Q11)[6] <- "5 - None of the Above"
levels(Survey$Q11)[7] <- "6 - Don't like sports"

#Question 12
names(Survey)[names(Survey) == "Q12"] <- "FavTime"
levels(Survey$Q12)[1] <- "0 - Morning"
levels(Survey$Q12)[2] <- "1 - Afternoon"
levels(Survey$Q12)[3] <- "2 - Evening"
levels(Survey$Q12)[4] <- "3 - Late Night"

#Question 13
names(Survey)[names(Survey) == "Q13"] <- "Hand"
levels(Survey$Q13)[1] <- "0 - Left-Handed"
levels(Survey$Q13)[2] <- "1 - Ambidextrous"
levels(Survey$Q13)[3] <- "2 - Mixed Handed"
levels(Survey$Q13)[4] <- "3 - Right Handed"

#Question 14
names(Survey)[names(Survey) == "Q14"] <- "Quad"
levels(Survey$Q14)[1] <- "0 - North Quad"
levels(Survey$Q14)[2] <- "1 - South Quad"
levels(Survey$Q14)[3] <- "2 - Mid Quad"
levels(Survey$Q14)[4] <- "3 - Don't live on CMC"

#Question 15
names(Survey)[names(Survey) == "Q15"] <- "MusGenre"
levels(Survey$Q15)[1] <- "0 - Rock"
levels(Survey$Q15)[2] <- "1 - Classical"
levels(Survey$Q15)[3] <- "2 - Alternative"
levels(Survey$Q15)[4] <- "3 - Hit-Hop/Rap"
levels(Survey$Q15)[5] <- "4 - Electronic"
levels(Survey$Q15)[6] <- "5 - country/Blues"
levels(Survey$Q15)[7] <- "6 - Other"

#Question 16
names(Survey)[names(Survey) == "Q16"] <- "Voting"
levels(Survey$Q16)[1] <- "0 - No"
levels(Survey$Q16)[2] <- "1 - Yes"
levels(Survey$Q16)[3] <- "2 - Have not decided"

#Question 17
names(Survey)[names(Survey) == "Q17"] <- "Partners"
levels(Survey$Q17)[1] <- "0 - None"
levels(Survey$Q17)[2] <- "1 - Less than 3"
levels(Survey$Q17)[3] <- "2 - Between 3 and 6"
levels(Survey$Q17)[4] <- "3 - Between 6 and 10"
levels(Survey$Q17)[5] <- "4 - More than 10"


names(Survey)[names(Survey) == "Q18"] <- "YearBorn"
names(Survey)[names(Survey) == "Q19"] <- "Tip"
names(Survey)[names(Survey) == "Q20"] <- "SocialMedia"
names(Survey)[names(Survey) == "Q21"] <- "Siblings"
names(Survey)[names(Survey) == "Q22"] <- "MovMonth"
names(Survey)[names(Survey) == "Q23"] <- "Potter"
names(Survey)[names(Survey) == "Q24"] <- "MilesHome"
names(Survey)[names(Survey) == "Q25"] <- "Excercise"
names(Survey)[names(Survey) == "Q26"] <- "HourSleep"
names(Survey)[names(Survey) == "Q27"] <- "HourPhone"
names(Survey)[names(Survey) == "Q28"] <- "Commuting"
names(Survey)[names(Survey) == "Q29"] <- "Parents"
names(Survey)[names(Survey) == "Q30"] <- "SportsPlay"
names(Survey)[names(Survey) == "Q31"] <- "GELeft"
names(Survey)[names(Survey) == "Q32"] <- "Alone"
names(Survey)[names(Survey) == "Q33"] <- "Countries"
names(Survey)[names(Survey) == "Q34"] <- "Dogs"
names(Survey)[names(Survey) == "Q35"] <- "Snack"






#Question 37
names(Survey)[names(Survey) == "Q37"] <- "OffCampus"
levels(Survey$Q37)[1] <- "0 - Never"
levels(Survey$Q37)[2] <- "1 - Rarely"
levels(Survey$Q37)[3] <- "2 - Occasionally"
levels(Survey$Q37)[4] <- "3 - Frequently"
levels(Survey$Q37)[5] <- "4 - Very Frequently"
levels(Survey$Q37)[6] <- "5 - I am not a  student"

#Question 39
names(Survey)[names(Survey) == "Q39_1"] <- "Sleep7"
levels(Survey$Q39_1)[1] <- "0 - Strongly Disagree"
levels(Survey$Q39_1)[2] <- "1 - Somewhat Disagree"
levels(Survey$Q39_1)[3] <- "2 - Neither agree nor disagree"
levels(Survey$Q39_1)[4] <- "3 - Somewhat agree"
levels(Survey$Q39_1)[5] <- "4 - Strongly Agree"

names(Survey)[names(Survey) == "Q39_2"] <- "Reusable"
levels(Survey$Q39_2)[1] <- "0 - Strongly Disagree"
levels(Survey$Q39_2)[2] <- "1 - Somewhat Disagree"
levels(Survey$Q39_2)[3] <- "2 - Neither agree nor disagree"
levels(Survey$Q39_2)[4] <- "3 - Somewhat agree"
levels(Survey$Q39_2)[5] <- "4 - Strongly Agree"

names(Survey)[names(Survey) == "Q39_3"] <- "Recycle"
levels(Survey$Q39_3)[1] <- "0 - Strongly Disagree"
levels(Survey$Q39_3)[2] <- "1 - Somewhat Disagree"
levels(Survey$Q39_3)[3] <- "2 - Neither agree nor disagree"
levels(Survey$Q39_3)[4] <- "3 - Somewhat agree"
levels(Survey$Q39_3)[5] <- "4 - Strongly Agree"

names(Survey)[names(Survey) == "Q39_4"] <- "JobSec"
levels(Survey$Q39_4)[1] <- "0 - Strongly Disagree"
levels(Survey$Q39_4)[2] <- "1 - Somewhat Disagree"
levels(Survey$Q39_4)[3] <- "2 - Neither agree nor disagree"
levels(Survey$Q39_4)[4] <- "3 - Somewhat agree"
levels(Survey$Q39_4)[5] <- "4 - Strongly Agree"

names(Survey)[names(Survey) == "Q39_5"] <- "Enjoyment"
levels(Survey$Q39_5)[1] <- "0 - Strongly Disagree"
levels(Survey$Q39_5)[2] <- "1 - Somewhat Disagree"
levels(Survey$Q39_5)[3] <- "2 - Neither agree nor disagree"
levels(Survey$Q39_5)[4] <- "3 - Somewhat agree"
levels(Survey$Q39_5)[5] <- "4 - Strongly Agree"

#Question 40
names(Survey)[names(Survey) == "Q40"] <- "Present"
levels(Survey$Q40)[1] <- "0 - Never"
levels(Survey$Q40)[2] <- "1 - Rarely"
levels(Survey$Q40)[3] <- "2 - Occasionally"
levels(Survey$Q40)[4] <- "3 - Frequently"
levels(Survey$Q40)[5] <- "4 - Always"


#Question 41
names(Survey)[names(Survey) == "Q41"] <- "FamPhone"
levels(Survey$Q41)[1] <- "0 - Never"
levels(Survey$Q41)[2] <- "1 - Once every few months"
levels(Survey$Q41)[3] <- "2 - At least once a month"
levels(Survey$Q41)[4] <- "3 - At least once a week"
levels(Survey$Q41)[5] <- "4 - At least once a day"

#Question 42
names(Survey)[names(Survey) == "Q42"] <- "GoodNight"
levels(Survey$Q42)[1] <- "0 - Never"
levels(Survey$Q42)[2] <- "1 - few times every 3 months"
levels(Survey$Q42)[3] <- "2 - A few times every month"
levels(Survey$Q42)[4] <- "3 - A few times a week"
levels(Survey$Q42)[5] <- "4 - Every Night"

Survey <- Survey[complete.cases(Survey),]

write.csv(Survey, file = "SurveyNames.csv")

#Still trying to export levels

Survey2 <- read.csv("SurveyQuestions.csv")


SocialMedia <- subset(Survey2, select=c(ID, Gender, Q20))
