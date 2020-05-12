#Some plots for the slides
uno <-as.character(readRDS("Silvia/uno.rds"))
due <-as.character(readRDS("Silvia/due.rds"))
tre <-as.character(readRDS("Silvia/tre.rds"))

states_to_sel <- unique(c(uno,due,tre))
length(states_to_sel) #27

datA <- dat %>% filter(id %in% states_to_sel)
dim(datA)

datA$Clusters <- ifelse(datA$id %in% uno, 1, 
                        ifelse(datA$id %in% due, 2, 3))

colnames(datA)[121] <- "Retail"

p<-datA %>%
  ggplot() +
  geom_tile(aes(x=date, y=id, fill=Retail)) +
  labs(y = "Countries", x = "Date", color = "Percent change in
visits Retails") +
  scale_fill_distiller(palette = "YlGnBu", direction = -1)

ggplotly(p)


p<-datA %>%
  ggplot() +
  geom_tile(aes(x=date, y=id, fill=stay_home_restrictions)) +
  labs(y = "Countries", x = "Date") +
  scale_fill_distiller(palette = "YlGnBu", direction = 1)

ggplotly(p) %>% add_annotations( text="Stay Home", xref="paper", yref="paper",
                                 x=1.02, xanchor="left",
                                 y=0.8, yanchor="bottom",    # Same y as legend below
                                 legendtitle=TRUE, showarrow=FALSE )  %>%
  layout( legend=list(y=0.8, yanchor="top" ) )

