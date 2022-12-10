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

data_url <- "http://search.electoralcommission.org.uk/api/csv/Spending?start={start}&rows={pageSize}&query=&sort=DateIncurred&order=asc&et=pp&date=&from=&to=&rptPd=&prePoll=false&postPoll=false&period=3696&isIrishSourceYes=true&isIrishSourceNo=true&includeOutsideSection75=true"

download.file(data_url, here("data_raw", "2019GE.csv"))

data <- read.csv(here("data_raw", "2019GE.csv"))

# Clean data 

data$TotalExpenditure <- gsub("£", "", data$TotalExpenditure) 
data$TotalExpenditure <- as.numeric(data$TotalExpenditure)


class(data$DateIncurred)
data$DateIncurred %>% head()
data$DateIncurred <- as.Date(data$DateIncurred, format = "%d/%m/%Y")
data$DateIncurred %>% head()

# Create relative date variable 

election_date19 <- as.Date("2019-12-12")

data$relative_DateIncurred <- difftime(data$DateIncurred, election_date19, units = "days")

# Plot overall day spend by party 

day_spend_party_plot <-  data %>%
    filter(between(DateIncurred, as.Date('2019-10-01'), as.Date('2019-12-31'))) %>%
    filter(RegulatedEntityName %in% c("Conservative and Unionist Party", "Liberal Democrats",  "Labour Party")) %>%
        group_by(relative_DateIncurred, RegulatedEntityName) %>% 
        summarise(TotalExpenditure = sum(TotalExpenditure, na.rm = TRUE)) %>%
            ggplot() + aes(x=relative_DateIncurred, y=TotalExpenditure) +
            geom_point() +
            geom_line() +
            scale_y_continuous(labels = scales::comma) +
            geom_vline(xintercept = 0, linetype = "dotted") +
            facet_wrap(~ RegulatedEntityName) +
            theme(
                legend.position = "none", 
                axis.text = element_text(size=8),
                axis.title=element_text(size=11,face="bold"),
                panel.background = element_rect(fill="white", color = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold", size = 15)
            )  
    
day_spend_party_plot

ggsave(here("output","day_spend_party_plot.pdf")), day_spend_party_plot)


# Plot daily spending by type 

mainpartyplot_by_type <- data %>% 
    filter(between(DateIncurred, as.Date('2019-01-01'), as.Date('2019-12-31'))) %>%
    filter(RegulatedEntityName %in% c("Conservative and Unionist Party", "Liberal Democrats",  "Labour Party")) %>% 
        group_by(relative_DateIncurred, RegulatedEntityName, ExpenseCategoryName) %>% 
        summarise(TotalExpenditure = sum(TotalExpenditure, na.rm = TRUE)) %>% 
            ggplot() + aes(x=relative_DateIncurred, y=TotalExpenditure, color = RegulatedEntityName) +
            geom_point(size=0.5) +
            geom_line(size=0.5) +
            geom_vline(xintercept = 0, linetype = "dotted") +
            facet_wrap(~ ExpenseCategoryName) +
            labs(title = "Daily Spending by Party and Spending Type", x = "Days out from the election \n(Election Day = 0)", y = "Total Expenses Incurred", color = "Party") +
             scale_color_manual(values = c("#2600ff", "#ff0000", "#ff7b00")) +
            theme(
                #legend.position = "none", 
                axis.text = element_text(size=8),
                axis.title=element_text(size=11,face="bold"),
                panel.background = element_rect(fill="white", color = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold", size = 15)
            )  


mainpartyplot_by_type


ggsave(here("output","mainpartyplot_by_type.pdf")), mainpartyplot_by_type,
height = 15, width = 20)

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
