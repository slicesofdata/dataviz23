
SPAN <- pavlovia_Merge(
  dir_path = 'C:/Users/gcook/Sync/Courses/GCDS/ospan4/data', 
  pattern_select = '.csv')


SPAN <- SPAN %>%
  dplyr::select(., starts_with("task_")) %>% 
  rename_with(., ~(gsub("task_", "", .x, fixed = T))) %>%
  filter(., participant > 0) %>% 
  select(., -c(date, subid, exp, blockfile, 
               operation, letter, operList, letter_end,
               operation_resp, operation_rt))
