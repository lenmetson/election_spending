# Install and/or load packages 

magic_library <- function(x) {
  y <- x %in% rownames(installed.packages())
  if(any(!y)) install.packages(x[!y])
  invisible(lapply(x, library, character.only=T))
  rm(x, y)
}
  
magic_library(c(
          "here",
          "tidyverse"))

rm(magic_library)

# Download data 

data_url <- "http://search.electoralcommission.org.uk/api/csv/Spending?start={start}&rows={pageSize}&query=&sort=DateIncurred&order=asc&et=pp&date=&from=&to=&rptPd=&prePoll=false&postPoll=false&period=3696&isIrishSourceYes=true&isIrishSourceNo=true&includeOutsideSection75=true" # url is to a queried csv download link from the Electoral Commission's website

download.file(data_url, here("data_raw", "2019GE.csv")) # Download the csv into our working directory

data <- read.csv(here("data_raw", "2019GE.csv")) # Read in the data

# Clean data: the dataset comes with all variables encoded as characters 

# Convert Expenditure variable to numeric 
data$TotalExpenditure <- gsub("£", "", data$TotalExpenditure) # Remove £ symbol so we can convert it to numeric 
data$TotalExpenditure <- as.numeric(data$TotalExpenditure) # Convert to numeric 

# Convert Date incurred variable to date 
data$DateIncurred <- as.Date(data$DateIncurred, format = "%d/%m/%Y")


# Create relative date variable 

election_date19 <- as.Date("2019-12-12") # Set date to be relative to 
data$relative_DateIncurred <- difftime(data$DateIncurred, election_date19, units = "days")

# Plot daily spending on advertising by party 

ad_plot_party <- data %>% 
    filter(between(DateIncurred, as.Date('2019-06-12'), as.Date('2019-12-31'))) %>%
    filter(RegulatedEntityName %in% c("Conservative and Unionist Party", "Liberal Democrats",  "Labour Party", "Scottish National Party (SNP)", "Green Party", "Reform UK")) %>% 
    filter(ExpenseCategoryName == "Advertising") %>% 
        group_by(relative_DateIncurred, RegulatedEntityName) %>% 
        summarise(TotalExpenditure = sum(TotalExpenditure, na.rm = TRUE)) %>% 
            ggplot() + aes(x=relative_DateIncurred, y=TotalExpenditure, 
            color = RegulatedEntityName
            ) +
            geom_point(size=0.5) +
            geom_line(size=0.5) +
            geom_vline(xintercept = 0, linetype = "dotted") +
            facet_wrap(~factor(RegulatedEntityName, levels=c('Conservative and Unionist Party', 'Labour Party', 'Liberal Democrats', 'Green Party', 'Reform UK', 'Scottish National Party (SNP)'))) +
            labs(title = "Daily Spending on Advertising by Party during the 2019 General Election", x = "Days out from the election \n(Election Day = 0)", y = "Total Expenses Incurred on Advertising", color = "Party") +
            scale_color_manual(values = c("#2600ff", "#379700", "#ff0000", "#ff7b00", "#00ccff", "#bdc000")) +
            theme(
                legend.position = "none", 
                axis.text = element_text(size=8),
                axis.title=element_text(size=11,face="bold"),
                panel.background = element_rect(fill="white", color = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold", size = 15)
            )   

ad_plot_party

ggsave(here("output", "ad_plot_party.pdf"), ad_plot_party,
height = 8, width = 12)

ggsave(here("output", "ad_plot_party.png"), ad_plot_party,
height = 8, width = 12)
