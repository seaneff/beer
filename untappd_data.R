## set working directory
setwd("~/Documents/workspace/beer")

## load required libaries
library(ggplot2)
library(dplyr)

## center ggplot2 titles by default
theme_update(plot.title = element_text(hjust = 0.5))

## read in data
dat <- read.csv("~/Documents/workspace/beer/untappd_data_may_2017.csv", 
                header = TRUE, stringsAsFactors = FALSE)

## split feature beer_type into two separate categories
## also remove trailing spaces
dat$beer_category <- sub("\\s+$", "", sapply(strsplit(dat$beer_type, split = "-"), "[", 1))
dat$beer_subcategory <- sapply(strsplit(dat$beer_type, split = "-"), "[", 2)


## create grouping variable for beer category
## all beers with over 5 unique beers get their own category, others grouped as "other
dat$beer_category_groups <- ifelse(dat$beer_category == "IPA", "IPA",
                            ifelse(dat$beer_category %in% c("Stout", 'Porter'), "Stout/Porter",
                            ifelse(dat$beer_category == "Lager", "Lager",
                            ifelse(dat$beer_category == "Pale Ale", "Pale Ale",
                            ifelse(dat$beer_category %in% "Saison / Farmhouse Ale", "Saison",
                            ifelse(dat$beer_category %in% c("Sour", "Lambic"), "Sour",
                            ifelse(dat$beer_category %in% c("Belgian Dubbel",
                                                            "Belgian Tripel",
                                                            "Belgian Quad",
                                                            "Belgian Strong Golden Ale"),
                            "Belgian",
                            "Other Beer Type")))))))
                           
                           
## treat beer_category as a factor, ordered by frequency of consumption
dat$beer_category <- factor(dat$beer_category,
                            levels = names(sort(table(dat$beer_category))))

## created feature called created_at_date, including only the date and no timestamp
## of datapoint creation
dat$created_at_date <- as.Date(sapply(strsplit(dat$created_at, split = " "), "[", 1), 
                               format = "%m/%d/%y")

## create dataset that only includes unique beers
## only include most recent report for that beer when counting
## order all elements of dat from most recently reported to 
dat <- dat[order(dat$created_at_date, decreasing = TRUE),]

## only look at most recently reported instance of each beer (by name)
unique_beers <- dat[-which(duplicated(dat$beer_name)),]

#################################################################################
## Beer Category ################################################################
#################################################################################

## categories which I've drank more than one unique beer
category <- unique_beers %>%
  group_by(beer_category) %>%
  summarize(mean_score = mean(rating_score),
         n = length(beer_name)) %>%
  filter(n > 1) %>%
  ## order from most beers to least beers consumed
  arrange(desc(n)) 

## most frequently consumed beer categories
pdf("beer_category_count.pdf", height = 5, width = 7)
ggplot(unique_beers, aes(beer_category)) +
  geom_bar(fill = "dark blue") +
  coord_flip() +
  xlab("") +
  ylab("Number of Unique Beers") +
  ggtitle("Most Frequently Consumed Beer Types")
dev.off()

## most frequently consumed beer categories by average rating
pdf("beer_category_count.pdf", height = 5, width = 7)
ggplot(category, aes(x = beer_category, y = n, fill = mean_score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Number of Unique Beers") +
  ggtitle("Most Frequently Consumed Beer Types") +
  ## rename legend
  scale_fill_gradient(low = "red", high = "blue", name = "Average\n Beer Rating")
dev.off()

#################################################################################
## Brewery #####################################################################
#################################################################################

## breweries from which I've drank more than one unique beer
brewery <- unique_beers %>%
  group_by(brewery_name) %>%
  tally() %>%
  filter(n > 1) %>%
  ## order from most beers to least beers consumed
  arrange(desc(n))

## treat brewery_name as a factor, ordered by number of unique beers from that brewery tried
brewery$brewery_name <- factor(brewery$brewery_name, levels = rev(brewery$brewery_name))

## most frequently consumed beer categorories
pdf("brewery_count_over1.pdf", height = 5, width = 7)
ggplot(brewery, aes(x = brewery_name, y = n)) +
  geom_bar(fill = "dark blue", stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Number of Unique Beers") +
  ggtitle("Most 'Popular' Breweries\n(with respect to total number of unique beers tasted)") +
  ## only integers on x axis (coordinates are flipped above)
  ## floor is okay here because only integers are possible
  scale_y_continuous(labels = function (x) floor(x))
dev.off()

#################################################################################
## Model ########################################################################
#################################################################################

## treat IPA as reference group
unique_beers$beer_category_groups <- factor(unique_beers$beer_category_groups)
unique_beers <- within(unique_beers, beer_category_groups <- relevel(beer_category_groups, ref = "IPA"))

## full model
full_model <- glm(rating_score ~ beer_category_groups + log(beer_ibu + 0.001) + beer_abv,
                    dat = unique_beers)

## beer categories only model
category_model <- glm(rating_score ~ beer_category_groups,
                      dat = unique_beers)

summary(full_model)
summary(category_model)

master_model_frame <- data.frame(variable = rownames(summary(full_model)$coef),
                                 coefficient = summary(full_model)$coef[, 1],
                                 se = summary(full_model)$coef[, 2],
                                 model_name = "Full Model")

category_model_frame <- data.frame(variable = rownames(summary(category_model)$coef),
                                   coefficient = summary(category_model)$coef[, 1],
                                   se = summary(category_model)$coef[, 2],
                                   model_name = "Beer Category Model")

all_models <- data.frame(rbind(master_model_frame,
                               category_model_frame))

## create indicator for beer group indicator variables
all_models$category_group <- grepl("beer_category_groups", all_models$variable)

## rename variables
all_models$variable <- gsub("beer_category_groups", "", as.character(all_models$variable))
all_models[which(all_models$variable == "log(beer_ibu + 0.001)"),]$variable <- "ln(IBU)"
all_models[which(all_models$variable == "beer_abv"),]$variable <- "ABV"

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

pdf("beer_type_effects.pdf", height = 4, width = 6)
ggplot(all_models[which(all_models$category_group == TRUE),], aes(colour = model_name)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = variable, ymin = coefficient - se*interval1,
                     ymax = coefficient + se*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + 
  ggtitle("Relative Effect of Beer Types on Ratings\n(reference group: IPA)") +
  xlab("") +
  ylab("Coefficient\n(Estimated Additive Effect on Rating)") +
  scale_colour_discrete(name = "Model Name")
dev.off()

