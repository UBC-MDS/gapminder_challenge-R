library(dash)
#library(dashBootstrapComponents)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(tidyverse)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv(here::here("data", "raw", "world-data-gapminder_raw.csv"))


df[is.na(df)] <- 0

df_year_region <- df %>%
    group_by(year, region) %>%
    summarise(sum_co2_per_capita = sum(co2_per_capita))


marks_card2 <- list("1918" = "1918", 
                "1928" = "1928",
                "1938"="1938",
                "1948" = "1948",
                "1958" = "1958",
                "1968" = "1968",
                "1978" = "1978",
                "1988" = "1988",
                "1998" = "1998",
                "2008" = "2008",
                "2018" = "2018"
                )

plot <- function(year_range, filter) {
  if(filter=='') {
    df_by_year <- df %>%
      filter(year>=year_range[1] & year<year_range[2]) %>%
      group_by(year) %>%
      summarize(children_per_woman = mean(children_per_woman))
    p <- ggplot(df_by_year, 
          aes(y = children_per_woman,
              x = year)) +
          geom_line() +
          labs(y="Children per woman", x="Year") +
          ggtitle("Average Number of Children") +
          theme(plot.title = element_text(hjust = 0.5))
  } else {
    df_by_year <- df %>%
      filter(year>=year_range[1] & year<year_range[2]) %>%
      group_by(!!sym(filter), year) %>%
      summarize(children_per_woman = mean(children_per_woman))

    p = ggplot(df_by_year, 
            aes(y = children_per_woman,
                x = year)) +
            geom_line(aes(color=!!sym(filter))) +
            labs(y="Children per woman", x="Year") +
            ggtitle("Average Number of Children") +
            theme(plot.title = element_text(hjust = 0.5))
  }
  return(p)
}


app$layout(
    div(
        list(
            dbcRow(className = "text-center bg-warning",
                list(
                    h1("Gapminder Challenge"),
                    p(
                        paste(
                            "Take the challenges below to see how you understand global issues such as healthcare and finance development."
                        )
                    ),
                    p(
                        paste(
                            "Test yourselves now!"
                        )
                    )
                )
            ),
            dbcRow(
                list(
                    dbcCol(dbcCard(className = "m-3 p-3",
                        list(
                            dccGraph(id = "bar_chart"),
                            dccSlider(
                                id = "slider",
                                min = 1914,
                                max = 2014,
                                step = 10,
                                marks = list(
                                    "1914" = "1914",
                                    "1924" = "1924",
                                    "1934" = "1934",
                                    "1944" = "1944",
                                    "1954" = "1954",
                                    "1964" = "1964",
                                    "1974" = "1974",
                                    "1984" = "1984",
                                    "1994" = "1994",
                                    "2004" = "2004",
                                    "2014" = "2014"
                                ),
                                value = 1914,
                            ),
                            dccDropdown(
                                id = "dropdown",
                                options = c(
                                    "Americas", 
                                    "Asia", 
                                    "Europe", 
                                    "Oceania", 
                                    "Africa"
                                ),
                                value = c("Americas", "Oceania", "Africa"),
                                multi = TRUE
                            )
                        )
                    ),
                    md = 6
                    ),
                    dbcCol(dbcCard(className = "m-3 p-3", div(
                        list(
                            dccGraph(id='plot_area'),
                            htmlLabel('Zoom in years: '),
                            dccRangeSlider(min=1918, max=2018, step=1-10, value=list(1918, 2018),  id='year_range_slider',
                                        marks=marks_card2
                            ),
                            htmlLabel('See breakdown number by: '),
                            dccDropdown(
                            options=list(
                            list(label='All', value=''),
                            list(label='Income Group', value='income_group'),
                            list(label='Region', value='region')),
                            value='', clearable=FALSE, id='filter_dropdown')
                            )

                    )), md = 6)
                )
            ),
            dbcRow(
                list(
                    dbcCol(dbcCard(className = "m-3 p-3",div(
                        "Card 3"
                    )), md = 6),
                    dbcCol(dbcCard(className = "m-3 p-3",div(
                        "Card 4"
                    )), md = 6)
                )
            )
        )
    )
)


app$callback(
    output("bar_chart", "figure"),
    list(
        input("slider", "value"),
        input("dropdown", "value")
    ),
    function(selected_year, selected_regions) {
        p <- ggplot(df_year_region %>%
            filter(year == selected_year, region %in% selected_regions)) +
            aes(y = reorder(region, sum_co2_per_capita, ),
            x = sum_co2_per_capita, fill = region) +
            geom_bar(stat = "identity") +
            labs(x = "CO2 emissions per capita", y = "Region")
        ggplotly(p) %>% layout()
    }
)

app$callback(
    output('plot_area', 'figure'),
    list(
      input('year_range_slider', 'value'),
      input('filter_dropdown', 'value')
      ),
    function(year_range, filter) {
      return(ggplotly(plot(year_range, filter)))
    }
)

###### Card 4 #####
# Read in the raw data and subset the data for analysis
card4_df <- readr::read_csv(here::here('data', 'raw', 'combined-data-for-GDP-per-cap.csv'))

# Define constant values
years <- list("1960" = "1960",
              "1968" = "1968",
              "1978" = "1978",
              "1988" = "1988",
              "1998" = "1998",
              "2008" = "2008",
              "2018" = "2018"
)
regions <- list(list(label = "All", value = ''),
                list(label = "Asia", value = "Asia"),
                list(label = "Europe", value = "Europe"),
                list(label = "Africa", value = "Africa"),
                list(label = "Americas", value = "Americas"),
                list(label = "Oceania", value = "Oceania"))

COUNTRIES <- unique(df$country)

app$layout(
  dbcContainer(
    list(
      dccGraph(id='plot-area'),
      htmlLabel("Zoom in Years: "),
      dccRangeSlider(
        id = 'year_slider',
        min = 1960, 
        max = 2018,
        step = 1,
        value = list(1960, 2018),
        marks = years,
        tooltip = 'placement'
      ),
      htmlLabel("Filter by Geographic Region: "),
      dccDropdown(
        id = 'region_dropdown',
        options = regions,
        value = '',
        multi = TRUE)
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('year_slider', 'value'),
       input('region_dropdown', 'value')),
  
  function(year_range, region_filter) {
    df_sub <- card4_df %>%
      filter(year >= year_range[1] & year <= year_range[2])
    
    if ('' %in% region_filter) {
      p <- df_sub %>%
        group_by(year) %>%
        summarise(income = sum(`GDP total`),
                  pop = sum(population)) %>%
        mutate(income_per_capita = income / pop) %>%
        ggplot(aes(x = year, y = income_per_capita)) +
        geom_line() +
        labs(x = "Year",
             y = "income Per Capita (in US$)",
             title = "income Per Capita Has Been Rising") +
        ggthemes::scale_color_tableau()
    } else {
      df_sub <- df_sub %>%
        filter(region %in% region_filter)
      p <- df_sub %>%
        group_by(year, region) %>%
        summarise(income = sum(`GDP total`),
                  pop = sum(population)) %>%
        mutate(income_per_capita = income / pop) %>%
        ggplot(aes(x = year, y = income_per_capita, color=region)) +
        geom_line() +
        labs(x = "Year",
             y = "income Per Capita (in US$)",
             title = "Income Per Capita Has Been Rising") +
        ggthemes::scale_color_tableau()
    }
    ggplotly(p)
    
  }
)

app$run_server(host = "0.0.0.0")

