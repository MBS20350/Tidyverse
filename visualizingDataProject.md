---
title: "Visualizing Data Project"
author:  Mark Sucato
output: 
  html_document:
    keep_md: true
---

# Visualizing Data Project
## by Mark Sucato


```r
library(tidyverse)
library(ggrepel)
```

## Problem #1


```r
fileName <- c("data_fastfood_sales.csv")
dataSales <- read_csv(fileName)
ggplot(dataSales, aes(us_sales, unit_count, label = restaurant)) +
	geom_point(aes(color = num_franchised_stores / unit_count)) +
	geom_text_repel() +
	scale_x_log10() +
	scale_y_log10() +
	theme_bw() +
	labs( x = "U.S. sales in millions (log10 scale)",
		y = "Total number of stores (log10 scale)",
		color = str_wrap("Proportion of stores franchised", width=30))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

## Problem #2


```r
fileName <- c("data_fastfood_sales.csv")
dataSales <- read_csv(fileName)
ggplot(dataSales, aes(x = reorder(restaurant, average_sales), y = average_sales)) +
	geom_bar(stat = "identity") +
	coord_flip() +
	theme_classic() +
	geom_text(aes(label = paste("$", round(average_sales))), hjust = -0.1) +
	scale_y_continuous(labels = scales::dollar_format()) +
	labs( y = "Average sales per unit store (in thousands)",
		x = "Restaurant")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

## Problem #3


```r
fileName2 <- c("data_fastfood_calories.csv")
dataCalories <- read_csv(fileName2)
ggplot(dataCalories, aes(calories, sodium)) +
	geom_point() +
	theme_bw() +
	labs(x = "Calories", y = "Sodium (mg)") +
	facet_wrap(~restaurant) +
	geom_hline(aes(yintercept = 2300)) +
	geom_text_repel(data = filter(dataCalories, sodium > 2300),
				box.padding = 1, 
				aes(label = item), size = 2.5, max.overlaps = Inf,
				min.segment.length = 0, xlim = c(600, NA),
				ylim = c(2000, NA), direction = "y")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

## Problem #4


```r
fileName2 <- c("data_fastfood_calories.csv")
dataCalories <- read_csv(fileName2)
dataCalories <- dataCalories %>%
	mutate(is_salad = str_detect(tolower(item), pattern = "salad"))
ggplot(dataCalories, aes(x = reorder(restaurant, calories, median), y = calories)) +
	geom_boxplot(outlier.shape = NA) +
	geom_jitter(aes(color = is_salad), position = position_jitterdodge()) +
	scale_y_log10() +
	theme_bw() + 
	coord_flip() +
	labs(y = "Calories (log10 scale)", x = "Restaurant",
		color = str_wrap("Is the entree a salad?", width=14)) +
	scale_color_hue(labels = c("Not a salad", "Salad"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

## Problem #5


```r
fileName <- c("data_fastfood_sales.csv")
dataSales <- read_csv(fileName)
fileName2 <- c("data_fastfood_calories.csv")
dataCalories <- read_csv(fileName2)
dataCalories <- dataCalories %>%
	filter(!restaurant == "Taco Bell") %>%
	group_by(restaurant) %>%
	summarize (medianSugar = median(sugar)) %>%
	inner_join(dataSales)
ggplot(dataCalories, aes(x = reorder(restaurant, us_sales), y = us_sales, fill = medianSugar)) +
	geom_bar(stat = "identity") +
	theme_classic() +
	scale_fill_gradient2(low = "purple4", high = "gold", mid = "turquoise4", midpoint = 7.5) +
	labs(x = "Restaurant", y = "U.S. sales (in millions)", 
		fill = str_wrap("Median sugar (grams) in fast food entrees", width = 20))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
