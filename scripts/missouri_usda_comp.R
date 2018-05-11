
# libraries -------
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(scales)

load('data/code_percent.RData')
load('data/codes.RData')

# data import -------
if (.Platform$OS.type == 'windows'){
codes <- read_excel('C:/Users/emeador/Downloads/ruralurbancodes2013 (3).xls')
    
} else {
codes <-
    read_excel('/Users/ElliotMeador/Downloads/ruralurbancodes2013 (1).xls')
    }




# Description recode/reorder --------



codes$Description <- fct_recode(
    codes$Description,
    'Metro 250k - 1m' = 'Metro - Counties in metro areas of 250,000 to 1 million population',
    'Metro < 250k' = 'Metro - Counties in metro areas of fewer than 250,000 population',
    'Urban 2.5k - 19.9k' = 'Nonmetro - Urban population of 2,500 to 19,999, adjacent to a metro area',
    'Metro > 1m' = 'Metro - Counties in metro areas of 1 million population or more',
    'Rural < 2.5k*' = 'Nonmetro - Completely rural or less than 2,500 urban population, not adjacent to a metro area',
    'Urban 2.5k - 19.9k*' = 'Nonmetro - Urban population of 2,500 to 19,999, not adjacent to a metro area',
    'Rural < 2.5k' = 'Nonmetro - Completely rural or less than 2,500 urban population, adjacent to a metro area',
    'Urban > 20k' = 'Nonmetro - Urban population of 20,000 or more, adjacent to a metro area',
    'Urban > 20k*' = 'Nonmetro - Urban population of 20,000 or more, not adjacent to a metro area'
)


codes$Description <- fct_relevel(
    codes$Description,
    c(
        "Metro > 1m",
        "Metro 250k - 1m",
        "Metro < 250k",
        "Urban > 20k",
        "Urban > 20k*",
        "Urban 2.5k - 19.9k",
        "Urban 2.5k - 19.9k*",
        "Rural < 2.5k",
        "Rural < 2.5k*"
    )
)
codes <- codes %>%
    mutate(MO = ifelse(State == 'MO', 'Missouri', 'Rest of U.S.'))

glimpse(codes)
# Description plots -------------------------------------------------------

colors <- RColorBrewer::brewer.pal(9, 'Blues')[c(4, 7)]

#####
# q <- 
codes %>% # interesting plot, not sure what to do with it
    mutate(Population_2010 = Population_2010) %>% 
    select(MO, Population_2010, Description) %>%
    mutate(Population_2010 = log(Population_2010)) %>% 
    ggplot(aes(Population_2010)) +
    geom_density(aes(color = MO,
                     fill = MO),
                 alpha = .8,
                 show.legend = T) +
    theme_bw() +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme(
        legend.position = 'bottom',
        panel.grid  = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank()
    ) +
    facet_wrap( ~ Description,scales = 'free')+
    labs(
        title = 'Comparing U.S. and Missouri',
        x = 'Population transformed\nusing natural log',
        y = 'Proportion',
        caption = '*Not adjacent to\nan urban county'
    ) 



data_frame(x = 6:12) %>% 
    mutate(y = exp(x)) %>% 
    ggplot(aes(x,y,group = 1))+
    geom_point(color = colors[2],fill = colors[2])+
    geom_line(color = colors[2])+
    theme_bw() +
    scale_x_continuous(breaks = seq(6,16,1))+
    scale_y_continuous(labels = scales::comma_format())+
    theme(axis.line = element_line(color = 'black'),
        legend.position = 'bottom',
        panel.grid  = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank()
    ) +
    labs(y = 'Population',
         x = 'Log (population)')
# bar_plots ---------------------------------------------------------
code_percent <- codes %>%
    select(MO, Population_2010, Description) %>%
    group_by(MO) %>%
    count(Description) %>%
    mutate(Percent = n / sum(n))

a <- bind_cols(
    code_percent %>%
        filter(MO == 'Missouri') %>%
        select(Percent),
    code_percent %>%
        filter(MO != 'Missouri') %>%
        select(Percent)
) %>%
    mutate(diff = abs(Percent1 - Percent)) %>%
    pull(diff)
code_percent$order <- as.numeric(c(a, a))
code_percent$MO <- fct_relevel(code_percent$MO,
                               c('Rest of U.S.', 'Missouri'))




# Code percent long -------------------------------------------------------

code_percent_MO <- code_percent[1:9,]
code_percent_US <- code_percent[10:18,]


code_percent_long <- bind_cols(code_percent_US,

code_percent_MO %>% 
    ungroup() %>% 
   select(n_MO = n,
          Percent_MO = Percent)
)


code_percent_wide <- code_percent_long %>%
    mutate(Percent_diff = abs(Percent - Percent_MO)) %>% 
    arrange(Percent_diff) %>% 
    mutate(rank = 1:nrow(.)) %>% 
    ungroup() %>% 
    select(Description,Percent_diff,rank) %>% 
    mutate(Percent_diff = scales::percent(Percent_diff))
    
    
save(code_percent_wide,file = 'html/code_percent_wide.RData')


code_percent_wide  
    




# plot --------------------------------------------------------------
code_percent$MO <- fct_relevel(code_percent$MO,'Rest of U.S.',after = 1)

comparing_US_and_Missouri_bar <- code_percent %>%
    ggplot(aes(fct_reorder(Description, Percent), Percent, label = n)) +
    geom_bar(stat = 'identity',
             width = .85,
             position = position_dodge(),
             aes(color = MO,
                 fill = MO)) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme(
        legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid  = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0)
    ) +
    labs(
        x = 'USDA Rural - Urban\nClassification',
        y = 'Percent of total counties',
        caption = '*Not adjacent to\nan urban county'
    )
# ggsave('png/comparing_US_and_Missouri_bar.png',width = 5,height = 7)
# Tables ------------------------------------------------------------------
MO_proportions <- codes %>% 
    group_by(State) %>% 
    count(RUCC_2013) %>% 
    group_by(State) %>% 
    mutate(total = n/sum(n)) %>% 
    filter(State == 'MO') %>% 
    ungroup() %>% 
    select(MO_proportion = total)

Proportion_Tables <- codes %>% 
    group_by(State) %>% 
    count(RUCC_2013) %>% 
    group_by(State) %>% 
    mutate(total = n/sum(n)) %>% 
    filter(State != 'MO') %>% 
    group_by(RUCC_2013) %>% 
    mutate(Average_proportion = mean(total)) %>% 
    ungroup() %>% 
    select(RUCC_2013,Average_proportion) %>% 
    distinct() %>% 
    bind_cols(.,MO_proportions) %>% 
    mutate(Average_proportion = scales::percent(Average_proportion),
           MO_proportion = scales::percent(MO_proportion)) %>% 
    arrange(RUCC_2013)

Proportion_Tables 



# MO vs all other states --------------------------------------------------

codes %>% 
    group_by(State,Description) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(freq = n/sum(n)) %>% 
    filter(Description == 'Rural < 2.5k*') %>% 
    arrange(desc(freq))





















