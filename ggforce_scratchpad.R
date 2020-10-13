library(ggforce)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(extrafont)
library(officer)
library(devEMF)

# https://ggforce.data-imaginist.com/
# https://cran.r-project.org/web/packages/ggforce/vignettes/Visual_Guide.html
# https://rviews.rstudio.com/2019/09/19/intro-to-ggforce/

# setwd
setwd("C:/Users/Stephen/Desktop/R/ggforce")

# from tutorial

# get data
titanic <- as_tibble(Titanic)
titanic %>% head()
titanic %>% glimpse()

original_data <- reshape2::melt(Titanic)
original_data
original_data %>% glimpse
original_data %>% head()

# note original_data is structured like the results of count(), where each distinct combo of variables has a row
# and these distinct variable combo rows all have a value field giving the count of observations of that type 
original_data %>% filter(Age == "Child", Class == "1st", Sex == "Male", Survived == "Yes")
original_data %>% count(Class, Sex, Age, Survived)
original_data %>% count(Class, Sex, Age, Survived) %>% nrow() # 32
original_data %>% nrow() # 32
original_data %>% count(Class, Sex, Age, Survived) %>% distinct(n) # 1

# format data using gather_set_data (see description of the resulting structure below)
data <- original_data %>% mutate(row_id = row_number()) %>% gather_set_data(x = 1:4, id_name = "id") %>% as_tibble()

# inspect
data
data %>% glimpse()
data %>% print(n = 50)
data %>% count(x, y)
original_data 
# note that the nrows of original data multiplied by number of x-axis variables equals nrow of data
# honestly not 100% sure whether this multiple/repeat is unvarying/necessary, so take it with a grain of salt
original_data %>% nrow() # 32
data %>% nrow() # 128
data %>% count(x) %>% nrow() # 4
32 * 4 == 128

# note that gather_set_data restructures data so that the rows are repeated
# as many times as there are x-axis variable categories
# the rows themselves look the same, except for the x and y variables
# in these x and y variables, the x-axis variables just take turns putting the x-axis variables
# the x-axis variable name is used in the x field, and the x-axis variable value is used in the y field
original_data %>% filter(Class == "1st")
data %>% filter(x == "Class", y == "1st")
data %>% filter(Class == "1st") %>% print(n = 20)

data %>% count(Age, Class, value)
data %>% filter(Age == "Child", Class == "1st")
data %>% filter(Age == "Child", Class == "1st", Sex == "Male", Survived == "No")
data %>% filter(Age == "Child", Class == "1st", Sex == "Male", Survived == "Yes")
data %>% filter(Age == "Child", Class == "1st", Sex == "Female", Survived == "No")
data %>% filter(Age == "Child", Class == "1st", Sex == "Female", Survived == "Yes")
data %>% filter(Class == "1st", Sex == "Male")


###############


# plot, note that default behavior is to order x axis categories alphabetically
ggplot(data, aes(x, id = id, split = y, value = value)) +
        geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
        geom_parallel_sets_axes(axis.width = 0.1) +
        geom_parallel_sets_labels(colour = 'white')


#///////////////////


# inspect values to confirm that y-axis count represents set size
original_data %>% filter(Age == "Child") %>% summarize(sum = sum(value))
original_data %>% filter(Age == "Adult") %>% summarize(sum = sum(value))
original_data %>% filter(Class == "3rd") %>% summarize(sum = sum(value))


###################


# same plot, renaming category to show default alphabetic ordering
data %>% rename(dage = Age) %>% mutate(x = case_when(x == "Age" ~ "dage", TRUE ~ x)) %>%
        ggplot(data = ., aes(x, id = id, split = y, value = value)) +
        geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
        geom_parallel_sets_axes(axis.width = 0.1) +
        geom_parallel_sets_labels(colour = 'white')


#######################


# same plot, manually ordering x axis categories using a new "order" variable
# note this works, but it's not actually necessary to create the new "order" var,
# you can just relevel the x var as a factor in the ggplot call (see example below)
data %>% mutate(order = case_when(x == "Age" ~ 4, x == "Class" ~ 3, 
                                  x == "Sex" ~ 2, x == "Survived" ~ 1, TRUE ~ NA_real_)) %>%
        ggplot(data = ., aes(x = fct_reorder(.f = factor(x), .x = order), id = id, split = y, value = value)) +
        geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
        geom_parallel_sets_axes(axis.width = 0.1) +
        geom_parallel_sets_labels(colour = 'white')


#######################


# add custom color palette (also relevel x axis factors directly in ggplot call for simplicity)

# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

data <- data %>% mutate(fill_color_bin = case_when(Sex == "Male" ~ "Male_fill", Sex == "Female" ~ "Female_fill"),
                fill_color = case_when(fill_color_bin == "Male_fill" ~ color_palette %>% slice(4) %>% pull(hex),
                                       fill_color_bin == "Female_fill" ~ color_palette %>% slice(9) %>% pull(hex), 
                                       TRUE ~ NA_character_))

fill_color_list <- data %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(fill_color_list) <- data %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
fill_color_list


#////////////////////////////////


# same plot, experimenting with formatting 

windowsFonts()
# font_import()
# loadfonts(device = "win")
windowsFonts()

# note getting the right alpha/hex balance may require trial and error
# note getting the y-axis max value right may require trial and error
# note the y-axis max also appears to be capped, so you can't enter a much larger number than the data contains
parallel_sets_example <- data %>% 
        ggplot(data = ., aes(x = factor(x, levels = c("Survived", "Sex", "Class", "Age")), id = id, split = y, value = value)) +
        scale_fill_manual(values = fill_color_list) +
        geom_parallel_sets(aes(fill = fill_color_bin), alpha = .3, axis.width = 0.2) +
        geom_parallel_sets_axes(fill = "#2171B5", axis.width = 0.2) +
        # note you can manually color set labels individually if you need
        # geom_parallel_sets_labels(color = c(rep("#2eb82e", times = 5), rep("#ff1a1a", times = 5))) +
        geom_parallel_sets_labels(color = "#000000") +
        scale_y_continuous(breaks = seq(from = 0, to = 2500, by = 500)) + 
        labs(x = NULL, y = "Number of passengers", color = NULL) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
parallel_sets_example


#///////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(parallel_sets_example)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "parrellel_sets_example.docx")


# /////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////


# geom_sina
# https://ggforce.data-imaginist.com/reference/geom_sina.html

midwest
midwest %>% glimpse()

# plot county area by state with distribution as overlapping circles
midwest %>% ggplot(data = ., aes(x = state, y = area)) + geom_point()

# plot county area by state as sina plot showing distribution as violin
midwest %>% ggplot(data = ., aes(x = state, y = area)) + geom_violin()

# plot county area by state as sina plot showing distribution as hittered circles
midwest %>% ggplot(data = ., aes(x = state, y = area)) + geom_jitter()

# plot county area by state as sina plot showing distribution as violin and sina
midwest %>% ggplot(data = ., aes(x = state, y = area)) + geom_violin() + geom_sina()

# plot county area by state as sina plot showing distribution as sina
midwest %>% ggplot(data = ., aes(x = state, y = area)) + geom_sina()


#////////////////////////////////////


# sina plot
midwest %>% ggplot(data = ., aes(x = state, y = popdensity)) + geom_sina()

# sina plot with log scale to better show distribution
midwest %>% ggplot(data = ., aes(x = state, y = popdensity)) + geom_sina() +
        scale_y_log10()

# sina plot with log scale and color mapping w/ continuous scale
midwest %>% ggplot(data = ., aes(x = state, y = popdensity, color = inmetro)) + geom_sina() +
        scale_y_log10()

# sina plot with log scale and color mapping w/ discrete scale 
# note you need to use position_dodge to overlap the distributions, since categorical color var by default separates them
midwest %>% ggplot(data = ., aes(x = state, y = popdensity, color = as.character(inmetro))) + 
        geom_violin(position = position_dodge(width = 0)) + geom_sina(position = position_dodge(width = 0)) + 
        labs(color = "") + scale_y_log10()

# sina plot with log scale, color mapping, and cut intervals on x axis
midwest %>% ggplot(data = ., aes(x = cut_width(area, 0.02), y = popdensity, color = inmetro)) + geom_sina() +
        scale_y_log10()


#////////////////////


# manually assign discrete colors to sina plot

# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- midwest %>% mutate(color_bin = case_when(inmetro == 1 ~ "metro",
                                                          inmetro == 0 ~ "rural"),
                                    color = case_when(inmetro == 1 ~ color_palette %>% slice(9) %>% pull(hex),
                                                      inmetro == 0 ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# sina plot with log scale and color mapping w/ discrete scale
chart_data %>% ggplot(data = ., aes(x = state, y = popdensity, 
        color = factor(color_bin, levels = c("metro", "rural")))) + geom_sina(position = position_dodge(width = 0)) +
        scale_color_manual(values = chart_data_color_list) +
        labs(color = "Legend") +
        scale_y_log10()





