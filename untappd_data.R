#################################################################################
## Set up workspace #############################################################
#################################################################################

## set working directory
setwd("~/Documents/workspace/beer")

#################################################################################
## Load libararies/set ggplot2 preferences ######################################
#################################################################################

## load required libaries
library(ggplot2)
library(dplyr)
library(randomForest)
library(scales)

## center ggplot2 titles by default
theme_update(plot.title = element_text(hjust = 0.5))

#################################################################################
## Read in data #################################################################
#################################################################################

## read in data
dat <- read.csv("data/573638fd2d25620be1fb5d7e5b64ca70.csv", 
                header = TRUE, stringsAsFactors = FALSE)

#################################################################################
## Process data #################################################################
#################################################################################

## split feature beer_type into two separate categories
## also remove trailing spaces
dat$beer_category <- sub("\\s+$", "", sapply(strsplit(dat$beer_type, split = "-"), "[", 1))
dat$beer_subcategory <- sapply(strsplit(dat$beer_type, split = "-"), "[", 2)

## create grouping variable for beer category
## Pilsners, Marzens, Bocks are types of pale lagers
dat$beer_category_groups <- ifelse(dat$beer_category %in% c("IPA", "Stout", "Porter", 
                                                            "Pale Ale", "Fruit Beer",
                                                            "Kolsh", "Blonde Ale", "Märzen",
                                                            "Lager", "Red Ale"),
                                                            dat$beer_category,
                                   
                            ifelse(dat$beer_category %in% c("Saison / Farmhouse Ale"), "Saison",
                            ifelse(dat$beer_category %in% c("Pumpkin / Yam Beer"), "Pumpkin Beer",
                            ifelse(dat$beer_category %in% c("Sour", "Lambic"), "Sour",
                            ifelse(dat$beer_category %in% c("Belgian Dubbel",
                                                            "Belgian Tripel",
                                                            "Belgian Quad",
                                                            "Belgian Strong Golden Ale"), "Belgian",
                            ## note: leaving lambics out of wheatbeer category, since to me, they taste
                            ## categorically different (though also, witbiers and wheat ales are pretty different),
                            ## and seem more like sours than wheats
                            ## so eventually probably want to split these up if I ever get enough data per subgroup
                            ifelse(dat$beer_category %in% c("Witbier",
                                                            "Hefeweizen",
                                                            "Pale Wheat Ale"), "Wheat beer (witbier, hefeweizen, etc)",
                                   "Other Beer Type"))))))
                           
## treat beer_category as a factor, ordered by frequency of consumption
dat$beer_category <- factor(dat$beer_category,
                            levels = names(sort(table(dat$beer_category))))

## created feature called created_at_date, including only the date and no timestamp
## of datapoint creation
dat$created_at_date <- as.Date(sapply(strsplit(dat$created_at, split = " "), "[", 1), 
                               format = "%m/%d/%y")

#################################################################################
## Clean data ###################################################################
#################################################################################

## if IBU is not known, it's reported as zero (it should be NA)
## challenge: sometimes IBU actually is zero

## identify all beers currently identified as having zero IBU
zero_ibu <- dat[which(dat$beer_ibu == 0),]$beer_name

## identify beers that are likely to actually have zero IBU (e.g. ciders)
zero_ibu_updated <- zero_ibu[-which(zero_ibu%in% c("Traditional Dry Cider",
                                                   "Buzzwig",
                                                   "Hopped Cidah"))]

## impute IBUS using random forests based on beer category
imputed_ibus <- randomForest(dat$beer_ibu ~ dat$beer_category)$predicted

## indices of beers for which to update IBU data
replace_index <- which(dat$beer_name %in% zero_ibu_updated)

## replace selected IBUS
dat$beer_ibu[replace_index] <- imputed_ibus[replace_index]

#################################################################################
## Generate Dataset for Primary Analysis ########################################
#################################################################################

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
pdf("results/beer_category_count.pdf", height = 5, width = 7)
## most frequently consumed beer categories by average rating
ggplot(category, aes(x = beer_category, y = n, fill = mean_score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Number of Unique Beers") +
  ggtitle("Most Frequently Consumed Beer/Cider Types") +
  ## rename legend
  scale_fill_gradient2(low = "#0099F7", midpoint = 2.5, name = "Average\n Beer Rating")
dev.off()

#################################################################################
## ABV ##########################################################################
#################################################################################

pdf("results/abv_distribution.pdf", height = 3, width = 5)
ggplot(dat, aes(beer_abv)) +
  geom_histogram(fill = "dark blue", col = "gray90", binwidth = 0.5) +
  xlab("ABV") +
  ylab("Count") +
  ggtitle("Distribution of Beer ABV")
dev.off()

abv <- unique_beers %>%
  group_by(abv = round(beer_abv)) %>%
  summarize(mean_score = mean(rating_score),
            n = length(brewery_name)) %>%
  ## order from most beers to least beers consumed
  arrange(desc(n))

## most frequently consumed beer categorories
pdf("results/abv_count.pdf", height = 5, width = 7)
## todo fix colors
ggplot(abv, aes(x = abv, y = n, fill = mean_score)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Number of Unique Beers") +
  ggtitle("Beer ABV") +
  ## only integers on x axis (coordinates are flipped above)
  scale_y_continuous(breaks = pretty_breaks()) +
  ## rename legend
  scale_fill_gradientn(name = "Average\n Beer Rating", colours = rainbow(n = 2))
dev.off()

#################################################################################
## IBU ##########################################################################
#################################################################################

pdf("results/ibu_distribution.pdf", height = 3, width = 5)
ggplot(dat, aes(beer_ibu)) +
  geom_histogram(fill = "dark blue", col = "gray90", binwidth = 10) +
  xlab("ABV") +
  ylab("Count") +
  ggtitle("Distribution of Beer IBU")
dev.off()

## round ibu to the nearest value of ten
ibu <- unique_beers %>%
  group_by(ibu = round(beer_ibu, digits = -1)) %>%
  summarize(mean_score = mean(rating_score),
            n = length(brewery_name)) %>%
  ## order with respect to IBU value
  arrange(ibu)

## most frequently consumed IBU values
pdf("results/ibu_count.pdf", height = 5, width = 7)
## todo fix colors
ggplot(ibu, aes(x = ibu, y = n, fill = mean_score)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Number of Unique Beers") +
  ggtitle("Beer IBUs") +
  ## only integers on x axis (coordinates are flipped above)
  scale_y_continuous(breaks = pretty_breaks()) +
  ## rename legend
  scale_fill_gradientn(name = "Average\n Beer Rating", colours = rainbow(n = 2))
dev.off()

#################################################################################
## Brewery ######################################################################
#################################################################################

## breweries from which I've drank more than one unique beer
brewery <- unique_beers %>%
  group_by(brewery_name) %>%
  summarize(mean_score = mean(rating_score),
            n = length(brewery_name)) %>%
  filter(n > 1) %>%
  ## order from most beers to least beers consumed
  arrange(desc(n))

## most frequently consumed beer categorories
pdf("results/brewery_count_over1.pdf", height = 7, width = 7)

## ordered by number of beers
## treat brewery_name as a factor, ordered by number of unique beers from that brewery tried
brewery$brewery_name <- factor(brewery$brewery_name, levels = rev(brewery$brewery_name))

ggplot(brewery, aes(x = brewery_name, y = n, fill = mean_score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Number of Unique Beers") +
  ggtitle("Most 'Popular' Breweries\n(with respect to total number of unique beers tasted)") +
  ## only integers on x axis (coordinates are flipped above)
  scale_y_continuous(breaks= pretty_breaks()) +
  ## rename legend
  scale_fill_gradient2(low = "#0099F7", midpoint = 2.5, name = "Average\n Beer Rating")

## ordered by average beer rating
## treat brewery_name as a factor, ordered by avarege beer rating of brewery
brewery$brewery_name <- factor(brewery$brewery_name, 
                               levels = as.character(brewery$brewery_name)[order(brewery$mean_score)])

ggplot(brewery[order(brewery$mean_score),], 
       aes(x = brewery_name, y = n, fill = mean_score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Number of Unique Beers") +
  ggtitle("Most 'Popular' Breweries\n(with respect to total number of unique beers tasted)") +
  ## only integers on x axis (coordinates are flipped above)
  scale_y_continuous(breaks= pretty_breaks()) +
  ## rename legend
  scale_fill_gradient2(low = "#0099F7", midpoint = 2.5, name = "Average\n Beer Rating")
dev.off()

#################################################################################
## Country ######################################################################
#################################################################################

## countries from which I've drank more than one unique beer
country <- unique_beers %>%
  group_by(brewery_country) %>%
  summarize(mean_score = mean(rating_score),
            n = length(brewery_name)) %>%
  filter(n > 1) %>%
  ## order from most beers to least beers consumed
  arrange(desc(n))

## maintain order of countries, treat as factor to avoid ggplot2 sorting alphabetically
country$brewery_country <- factor(country$brewery_country, levels = rev(country$brewery_country))

## most frequently consumed beer categorories
pdf("results/country_count_over1.pdf", height = 5, width = 7)
ggplot(country, aes(x = brewery_country, y = n, fill = mean_score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Number of Unique Beers") +
  ggtitle("Most 'Popular' Beer Countries\n(with respect to total number of unique beers tasted)") +
  ## only integers on x axis (coordinates are flipped above)
  scale_y_continuous(breaks= pretty_breaks()) +
  ## rename legend
  scale_fill_gradient2(low = "#0099F7", midpoint = 2.5, name = "Average\n Beer Rating")
dev.off()

#################################################################################
## Model: Beer Type #############################################################
#################################################################################

## treat IPA as reference group
unique_beers$beer_category_groups <- factor(unique_beers$beer_category_groups)
unique_beers <- within(unique_beers, beer_category_groups <- relevel(beer_category_groups, ref = "IPA"))

## full model
## note: as of October 9, 2017, there is a quadratic relationship between IBU and rating,
## but no quadratic relationship between ABV and rating
full_model_category <- glm(rating_score ~ beer_category_groups + beer_ibu + I(beer_ibu^2) + 
                             beer_abv,
                           dat = unique_beers)

## beer categories only model
category_model <- glm(rating_score ~ beer_category_groups,
                      dat = unique_beers)

summary(full_model_category)
summary(category_model)

master_model_frame <- data.frame(variable = rownames(summary(full_model_category)$coef),
                                 coefficient = summary(full_model_category)$coef[, 1],
                                 se = summary(full_model_category)$coef[, 2],
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

## all_models dataframe with only coefficients from beer category indicators
category_variables <- all_models[which(all_models$category_group == TRUE),]

## treat variable as an ordered factor from highest to lowest coefficient
## as defined by beer category model coefficient
category_variables$variable <- factor(category_variables$variable, levels = 
category_variables[which(category_variables$model_name == "Beer Category Model"),]$variable[
  order(category_variables[which(category_variables$model_name == "Beer Category Model"),]$coefficient)])

# Specify the width of your confidence intervals
interval <- -qnorm((1-0.95)/2)  # 95% multiplier

#################################################################################
## Plot Results: Coefficients for Beer Type #####################################
#################################################################################

pdf("results/beer_type_effects.pdf", height = 4, width = 7)
ggplot(category_variables, aes(colour = model_name)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = variable, ymin = coefficient - se*interval,
                     ymax = coefficient + se*interval),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + 
  ggtitle("Relative Effect of Beer Type on Ratings\n(reference group: IPA)") +
  xlab("") +
  ylab("Coefficient\n(Estimated Additive Effect on Rating)") +
  scale_colour_discrete(name = "Model Name")
dev.off()

#################################################################################
## Plot Results: Coefficients for IBU ###########################################
#################################################################################

ibu_sim <- seq(from = 0, to = 0.9*max(dat$beer_ibu), by = 1)

## generate notional dataset
sim_dat <- cbind.data.frame(beer_category_groups = "IPA",
                            beer_ibu = ibu_sim,
                            beer_abv = mean(dat$beer_abv))

predict_by_ibu <- cbind.data.frame(ibu = ibu_sim,
                                   pred = predict(full_model_category, sim_dat))

plot(x = predict_by_ibu$ibu, y = predict_by_ibu$pred, type = "l",
     xlab = "IBU", ylab = "Predicted Score",
     main = "Predicted Score for an IPA by Beer IBU")
