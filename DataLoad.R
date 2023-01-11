library("cowplot")
library("gridExtra")
library(ggplot2)
library(dplyr)
img <- readPNG(("3CRCLogo.png"))
df <- read.csv('./2022_23_Results.csv', stringsAsFactors = F)



df <- df[df$Avg.Pace != "0:00",]
df <- df %>% rename ('AvgPaceS' = Avg.Pace..s.,
                     'RunTimeS' = Run.Time..s.,
                     'ClockTimeS' = Clock.Time..s.,
                     'HandicapS' = Handicap..s.)

best = df %>% 
  group_by( Run.Date, Run.Length)%>%
  summarise(bestTotalPoints = max(Total.Points),
            bestPoints = max(Points), 
            bestPaceS = min(AvgPaceS))
df = merge(df, best)
df$divisionLeader = 0
df <- df %>% mutate(divisionLeader = replace(divisionLeader, Total.Points == bestTotalPoints , 1 ))

filtered_df <-  df %>%
  filter( (Run.Length == 'Long' | Run.ClassID == 'g1-3') & Run.Date == '2022-11-30') 
filtered_df <-  filtered_df[order(filtered_df$Total.Points),]

sel = 'Chris.Jardine'

ggplot(filtered_df, aes(x = Total.Points, 
                        y = reorder(Athlete.Name, Total.Points),
                        fill = ifelse(Athlete.Name == 'Chris Jardine', 'Highlighted','Normal'))) +
  geom_col() +
  theme(legend.position = "none", axis.title.y = element_blank()) 
  


filtered_df <-  df %>%
  filter( Run.Length == 'Long'  ) %>%
  mutate(Athlete.Name <- as.factor(Athlete.Name)) %>%
  select(Athlete.Name, Run.Date, Total.Points)
ggplot(filtered_df, aes(x = Run.Date, y = Total.Points, color = Athlete.Name )) +
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_line(aes(color = Athlete.Name),group = 'Run.Date' ) +
  ylab('Total Points') + xlab ('Run Date') +
  theme(axis.text.x = element_text(angle = 45))
