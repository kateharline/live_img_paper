library(dplyr)
library(stringr)
library(ggplot2)
dot_dir <- '/Users/kateharline/workspace/li_methods'
attr_dir <- 'attributes'

# import my styling
source('/Users/kateharline/workspace/data_analysis/final_plotting/light_paper_plots.R')
source('/Users/kateharline/workspace/data_analysis/li_methods/li_plotting.R')

setwd(file.path(dot_dir, attr_dir))
csv_names <- dir(pattern = '.csv')

all_attrs <- data.frame()

for(i in 1:length(csv_names)) {
  a_df <- read.csv(csv_names[[i]], check.names = F)
  
  # get day from the file name
  a_df$time <- rep(str_match(csv_names[[i]], regex('(?:d)([[:digit:]])'))[,2], length(a_df[[1]]))
  # add 3 for das
  a_df$time <- as.character(as.numeric(a_df$time) +3)
  a_df$sample_id <- rep(str_match(csv_names[[i]], regex('2-[[:digit:]]+')), length(a_df[[1]]))
  # add column for undiss or diss
  a_df$cond <- if_else(a_df$sample_id %in% c('2-1', '2-3', '2-6'), 'dissected', 'undissected')
  # make nas to zeros for width
  a_df <- a_df %>% mutate_at(vars('Medial-Lateral_Distance'), ~replace(., is.na(.), 0))
  # add nas if measure missing
  if (str_match(csv_names[[i]], regex('(?:d)([[:digit:]])'))[,2] > 1) {
    a_df$d_Area <- NA
    a_df$d_Proliferation <- NA
  }
  if (str_match(csv_names[[i]], regex('(?:d)([[:digit:]])'))[,2] == 1) {
    a_df$Parent <- NA
  }
  
  # add to big df 
  all_attrs <- rbind(all_attrs, a_df)
}

# summarize the data by dissection condition and day
cell_amt_sum <- all_attrs[!is.na(all_attrs$`Geometry/Area`),] %>% group_by(sample_id, time) %>% 
  summarise(area = sum(`Geometry/Area`),
            cell_count = length(Label),
            width = max(`Medial-Lateral_Distance`),
            max_area = max(`Geometry/Area`))

cell_amt_sum$cond <- if_else(cell_amt_sum$sample_id %in% c('2-1', '2-3', '2-6'), 'dissected', 'undissected')
cell_amt_sum$sum_fill <- as.character(cell_amt_sum$time)
cell_amt_sum$cond <- factor(cell_amt_sum$cond, levels = c('undissected', 'dissected'))

grow_div_sum <- all_attrs[all_attrs$time == '4' & !(is.na(all_attrs$d_Area)),] %>% group_by(sample_id) %>%
  summarise(avg_growth = mean(d_Area), avg_divs = mean(d_Proliferation), med_growth = median(d_Area),
             med_divs = median(d_Proliferation))

grow_div_sum$cond <- if_else(grow_div_sum$sample_id %in% c('2-1', '2-3', '2-6'), 'dissected', 'undissected')
grow_div_sum$cond <- factor(grow_div_sum$cond, levels = c('undissected', 'dissected'))
grow_div_sum$sum_fill <- dissection_palette[1]
grow_div_sum$time <- '4-5'

parent_sum <- all_attrs[all_attrs$time == '5',] %>% group_by(sample_id) %>%
  summarise(num_tracked = sum(!is.na(Parent)),
            percent_tracked = sum(!is.na(Parent))/length(Label))

parent_sum$cond <- if_else(parent_sum$sample_id %in% c('2-1', '2-3', '2-6'), 'dissected', 'undissected')
parent_sum$cond <- factor(parent_sum$cond, levels = c('undissected', 'dissected'))
parent_sum$sum_fill <- dissection_palette[2]
parent_sum$time <- '5'

plot_small_scatter_helper(cell_amt_sum, dissection_palette[1:2], shapes, c('area', 'cell_count', 'width'), add_p = T)
plot_small_scatter_helper(grow_div_sum, dissection_palette[1:2], shapes, c('avg_growth', 'avg_divs'), add_p = T)
plot_small_scatter(parent_sum,  dissection_palette[1:2], shapes, 'time', 'percent_tracked', add_p = T)
