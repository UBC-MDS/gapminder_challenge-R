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
card4_df <- readr::read_csv(here::here('data', 'raw', 'combined-data-for-GDP-per-cap.csv'))

df[is.na(df)] <- 0

df_year_region <- df %>%
    group_by(year, region) %>%
    summarise(sum_co2_per_capita = sum(co2_per_capita))

df_subregion_year <- df %>%
  group_by(sub_region, year) %>%
  summarise(mean_life_expectancy = mean(life_expectancy))

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

years_card4 <- list("1960" = "1960",
              "1968" = "1968",
              "1978" = "1978",
              "1988" = "1988",
              "1998" = "1998",
              "2008" = "2008",
              "2018" = "2018"
)
regions_card4 <- list(list(label = "All", value = ''),
                list(label = "Asia", value = "Asia"),
                list(label = "Europe", value = "Europe"),
                list(label = "Africa", value = "Africa"),
                list(label = "Americas", value = "Americas"),
                list(label = "Oceania", value = "Oceania"))


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
                    dbcCol(dbcCard(className = "m-3 p-3", 
                                   list(
                                     dccGraph(id = "line_chart_3"),
                                     dccRangeSlider(
                                       id = "slider_3",
                                       min = 1918,
                                       max = 2018,
                                       step = 10,
                                       marks = list(
                                         "1918" = "1918",
                                         "1928" = "1928",
                                         "1938" = "1938",
                                         "1948" = "1948",
                                         "1958" = "1958",
                                         "1968" = "1968",
                                         "1978" = "1978",
                                         "1988" = "1988",
                                         "1998" = "1998",
                                         "2008" = "2008",
                                         "2018" = "2018"
                                       ),
                                       value = list(1938, 2018)
                                     ),
                                     dccDropdown(
                                       id = "dropdown_3",
                                       options = unique(df_subregion_year$sub_region),
                                       value = unique(df_subregion_year$sub_region)[15],
                                       multi = TRUE
                                     )
                                   )
                    ), md = 6),
                    dbcCol(dbcCard(className = "m-3 p-3",
                                   list(
                                     dccGraph(id='plot-area'),
                                     htmlLabel("Zoom in Years: "),
                                     dccRangeSlider(
                                       id = 'year_slider',
                                       min = 1960,
                                       max = 2018,
                                       step = 1,
                                       value = list(1960, 2018),
                                       marks = years_card4,
                                       tooltip = 'placement'
                                     ),
                                     htmlLabel("Filter by Geographic Region: "),
                                     dccDropdown(
                                       id = 'region_dropdown',
                                       options = regions_card4,
                                       value = '',
                                       multi = TRUE)
                                     )
                                   ), md = 6)
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

# card 3
app$callback(
  output("line_chart_3", "figure"),
  list(
    input("slider_3", "value"),
    input("dropdown_3", "value")
  ),
  function(selected_year, selected_regions) {
    title_text = paste0("Mean Life Expectancy from ",
                        as.character(selected_year[1]),
                        " to ",
                        as.character(selected_year[2]))
    p <- ggplot(df_subregion_year %>%
                  filter(year >= selected_year[1], year <= selected_year[2],
                         sub_region %in% selected_regions)) +
      aes(y = mean_life_expectancy,
          x = year, color = sub_region) +
      geom_line() +
      labs(x = "Year", y = "Mean Life Expectancy") +
      theme_bw(base_size = 13) +
      ggtitle(title_text)
    ggplotly(p) %>% layout()
  }
)

# callback for card 4
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
             y = "Income Per Capita (in 2017 US$)",
             title = "Income Per Capita Has Been Rising") +
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
             y = "Income Per Capita (in 2017 US$)",
             title = "Income Per Capita Has Been Rising") +
        ggthemes::scale_color_tableau()
    }
    ggplotly(p)

  }
)

app$run_server(host = "0.0.0.0")

