
span_scores %>%
  select(., span_pos_z, span_pos_adj_z) %>%
  GGally::ggpairs(.) + see::theme_modern()

#reorder()
ggplot(span_scores, aes(span_pos_z)) +
  geom_histogram(color = "gray", fill = "yellow") + #binwidth = .1 
  xlab("Span Score (standardized)") + 
  #  scale_x_continuous(breaks = c(5, 15, 25)) + 
  see::theme_modern()

span_scores %>%
  #filter(., id > 9999) %>%
  ggplot(., 
         aes(x = reorder(id, span_pos_z),
             y = span_pos_z)
  ) +  
  #           fill = variable)) + 
  geom_bar(stat = "identity", fill = "lightblue") + 
  scale_y_continuous(n.breaks = 20) +
  #coord_flip() + 
  see::theme_modern() + 
  theme(axis.text.x = element_text(
    angle = 90, hjust = 1)) + #
  xlab("id") + ylab("Span Score (standardized)")
