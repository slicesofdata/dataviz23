GENERAL <- readr::read_csv(paste(data_dir, "GCDS22_11-15_numeric.csv", sep = "/"))

# get the question labels
questions.labels <- GENERAL[1,]   # assign row 1 to an object

#view(questions.labels)  # what were the actual questions?

# make new vars
cleaned_variables <- questions.labels %>%
  gsub("Enter the code provided to you. If you do not remember it, reach out to your leader before you continue.", "code", .) %>%
  gsub("\\?", "", .) %>%
  gsub("Do you tend to like games that are designed to include elements of: - ", "like games of ", .) %>% 
  gsub("Games involve different types of game play or rules that make them different from one another. Tell us whether you like each of the following types of game play. If you do not know what a form of game play is based on the label, select \"Not familiar with\". - ", "MECH_", .) %>%
  gsub("Games follow different themes. Tell us whether you like or dislike games involving or are about: - ", "CAT_", .) %>%
  gsub("I enjoy a challenge/achieving the goals of a game.", "enjoy_challenge", .) %>%
  gsub("I enjoy discovering the breadth of a game.", "enjoy_discovering", .) %>%
  gsub("I enjoy fellowship and relationships with others that comes from a game.", "enjoy_fellowship", .) %>%
  gsub("I enjoy competing with others and defeating them.", "enjoy_competing", .) %>%
  gsub("Which of the following do you find most pleasurable about playing board games", "most_pleasurable", .) %>%
  gsub("Tell us whether you find these characteristics about games to be pleasurable. - ", "pleasurable_", .) %>%
  gsub("Would you describe yourself as someone who likes to play board games", "gamer", .) %>%
  gsub("When you play a board game, what is the approximate duration of your preferred game play \\(e.g., 0:15 = 15 minutes\\)", "game_dur", .) %>%
  gsub("For a typical year, approximately how frequently would you play board games", "play_frequency", .) %>%
  gsub("In which year were you born", "dob", .) %>%
  gsub("With which gender do you identify", "gender", .) %>%
  gsub("For the imaginary world that games take me; imagining being someplace else or being someplace else.", "play_for_imaginary", .) %>%
  gsub("For the dramatic unfolding of a sequence of events:", "play_for_drama", .) %>%
  gsub("For the challenge:", "play_for_challenge", .) %>%
  gsub("For friendship, cooperation, and community:", "play_for_community", .) %>%
  gsub("To discover new things about game play:", "play_for_discovery", .) %>%
  gsub("To express myself and to create things:", "play_for_creating", .) %>%
  gsub("To leave the real world and escape into a new world with new rules and meaning:", "play_for_expression", .) %>%
  gsub("&", "and", .) %>% 
  gsub(",|'|\\(|\\)", "", .) %>%
  gsub("/|-", "_", .) %>%
  gsub(" ", "_", .) %>%
  tolower(.)

names(GENERAL) <- cleaned_variables

GENERAL <- GENERAL %>%   
  filter(., 
         !row_number() %in% c(1,2),
         code != "99999"
  ) %>%
  select(., -c(start_date:user_language, id))

view(GENERAL, filter = "none")
