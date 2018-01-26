library(shiny)
library(leaflet)
library(RColorBrewer)
library(DT)
library(scales)
library(lattice)
library(dplyr)
library(plotly)
library(tidyr)

raw_anon_data <- 
  read.csv("Data/raw_data.csv",
           header = TRUE,
           stringsAsFactors = FALSE)

building_rosters <-
  read.csv("Data/building_data.csv",
           header = TRUE,
           stringsAsFactors = FALSE)

locations <-
  read.csv("Data/building_locations.csv",
           header = TRUE,
           stringsAsFactors = FALSE) %>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))

plot_data <- building_rosters %>%
  group_by(Building) %>%
  count(predrace) %>%
  spread(predrace, n, fill = 0) %>%
  mutate(total = p_asi + p_bla + p_his + p_whi) %>%
  left_join(locations)

total <- ungroup(plot_data) %>%
  summarize(
    asi = sum(p_asi),
    his = sum(p_his),
    bla = sum(p_bla),
    whi = sum(p_whi),
    tot = sum(total)
  ) %>%
  mutate(p_asi = asi / tot,
         p_his = his / tot,
         p_bla = bla / tot,
         p_whi = whi / tot)

pval = numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)) {
  pval[i] <- prop.test(plot_data[i, 2:5],
                       c(plot_data[i, 6], plot_data[i, 6], plot_data[i, 6], plot_data[i, 6]),
                       total[1, 6:9],
                       alternative = "two.sided")$p.value
}

plot_data$pval = pval

function(input, output, session) {
  pal <- colorBin(
    palette = "RdYlGn",
    domain = plot_data$pval,
    bins = c(0, 0.05, 0.1, 0.3, 1)
  )
  # Create the map
  output$map <- renderLeaflet({
    leaflet(data = plot_data) %>%
      addTiles() %>%
      setView(-73.20554, 42.71343, zoom = 16) %>%
      addCircles(
        ~ long,
        ~ lat,
        popup = ~ paste(
          paste0("<b>Ethinic/Racial Composition</b>"),
          paste0(
            "Percent White and Other: ",
            formatC(100 * p_whi / total, digits = 3),
            "%"
          ),
          paste0(
            "Percent Black: ",
            formatC(100 * p_bla / total, digits = 3),
            "%"
          ),
          paste0(
            "Percent Hispanic: ",
            formatC(100 * p_his / total, digits = 3),
            "%"
          ),
          paste0("Percent Asian: ", formatC(100 *
                                              p_asi / total, digits = 3), "%"),
          sep = "<br/>"
        ),
        label = ~ as.character(Building),
        radius = ~ total ^ (1 / 2) * 2,
        color = ~ pal(pval),
        opacity = 0.7
      ) %>%
      addLegend(
        "bottomleft",
        values = ~ pval,
        title = "How likely is this to<br>happen randomly?",
        colors = c("#D7191C", "#FDAE61", "#A6D96A", "#1A9641"),
        labels = c("Super Unlikely", "Very Unlikely", "Unlikely", "Likely")
      )
  })
  
  output$pie <- renderPlotly({
    if (is.null(input$map_shape_click$lat)) {
      x <- total[1, 1:4]
      x <-
        data.frame("Categories" = colnames(x), "Count" = as.numeric(x))
      name = "the College"
    } else{
      data = plot_data[which(plot_data$lat == input$map_shape_click$lat), ]
      x <- data[1, 2:5]
      x <-
        data.frame("Categories" = colnames(x), "Count" = as.numeric(x))
      name = data[1, 1]
    }
    p <-
      plot_ly(
        x,
        label = ~ Categories,
        values = ~ Count,
        type = "pie",
        labels = c("Asian", "Hispanic", "Black", "White and Other"),
        textposition = "inside",
        textinfo = "label+percent+count",
        hoverinfo = "text",
        text = ~ paste0("Raw count: ", Count)
      ) %>%
      layout(
        title = paste0('Ethinic/Racial Composition<br>of ', name),
        margin = list(
          l = 0,
          r = 0,
          b = 0,
          t = 30
        )
      )
  })
  
  output$chi <- renderPlotly({
    x1 <- seq(0, qchisq(0.7, 3), 0.1)
    x2 <- seq(qchisq(0.7, 3), qchisq(0.9, 3), 0.1)
    x3 <- seq(qchisq(0.9, 3), qchisq(0.95, 3), 0.1)
    x4 <- seq(qchisq(0.95, 3), 15, 0.1)
    x = c(x1, x2, x3, x4)
    y1 <- c(dchisq(x1, 3), rep(0, 114))
    y2 <- c(rep(0, 37), dchisq(x2, 3), rep(0, 88))
    y3 <- c(rep(0, 63), dchisq(x3, 3), rep(0, 72))
    y4 <- c(rep(0, 79), dchisq(x4, 3))
    if (is.null(input$map_shape_click$lat)) {
      y5 <- rep(0, 151)
      a = list()
    } else{
      data = plot_data[which(plot_data$lat == input$map_shape_click$lat), ]
      pval <- data$pval
      stat <- qchisq(1 - pval, 3)
      y5 <- ifelse(x >= stat, dchisq(x, 3), 0)
      a <- list(
        x = stat,
        y = dchisq(stat, 3),
        text = paste0(
          formatC(pval * 100, digits = 2),
          "% chance of<br>occuring randomly."
        ),
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 4,
        ax = 30,
        ay = -40
      )
    }
    data <- data.frame(x, y1, y2, y3, y4, y5)
    plot_ly(
      data,
      x = ~ x,
      y = ~ y1,
      type = "scatter",
      mode = "line",
      fill = "tozeroy",
      fillcolor = "#1A9641",
      line = list(color = "#1A9641"),
      autosize = F,
      height = 200,
      width = 300,
      hoverinfo = "none"
    ) %>%
      add_trace(
        y = ~ y2,
        type = "scatter",
        mode = "line",
        fill = "tozeroy",
        fillcolor = "#A6D96A",
        line = list(color = "#A6D96A")
      ) %>%
      add_trace(
        y = ~ y3,
        type = "scatter",
        mode = "line",
        fill = "tozeroy",
        fillcolor = "#FDAE61",
        line = list(color = "#FDAE61")
      ) %>%
      add_trace(
        y = ~ y4,
        type = "scatter",
        mode = "line",
        fill = "tozeroy",
        fillcolor = "#D7191C",
        line = list(color = "#D7191C")
      ) %>%
      add_trace(
        y = ~ y5,
        type = "scatter",
        mode = "line",
        fill = "tozeroy",
        fillcolor = "#A64BC6",
        line = list(color = "#A64BC6")
      ) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(title = "Test Statistic"),
        yaxis = list(title = ""),
        title = "Probability of Random Occurance",
        margin = list(
          l = 20,
          r = 20,
          b = 40,
          t = 30
        ),
        annotations = a
      )
    #shapes=list(type='line', x0= 8, x1= 8, y0=0, y1=max(y1), line=list(dash='dot', width=2, color = "turqoise")),
    
  })
  
  output$allbar <- renderPlot({
    
    col_data = data.frame(Building = rep("College-Wide", 4), 
                       type = c("Asian", "Hispanic", "Black", "White and Other"),
                       value = c(0.2014519, 0.09921355, 0.1113128, 0.5880218))
    
    allbar_data <- plot_data %>%
      mutate(
        Asian = p_asi / total,
        Hispanic = p_his / total,
        Black = p_bla / total,
        `White and Other` = p_whi / total
      ) %>%
      select(1, Asian, Hispanic, Black, `White and Other`) %>%
      gather(type, value, 2:5)%>%
      as.data.frame()%>%
      rbind(col_data)
    
    ggplot(data = allbar_data, aes(
      x = Building,
      group = type,
      fill = type
    )) +
      geom_bar(aes(weight = value)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("Proportions") +
      geom_hline(aes(yintercept=0.5880218), colour="#990000", linetype="dashed", size = 1)+
      geom_hline(aes(yintercept=0.6872354), colour="#990000", linetype="dashed", size = 1)+
      geom_hline(aes(yintercept=0.7985482), colour="#990000", linetype="dashed", size = 1)+
      geom_rect(aes(xmin = 6.5, xmax = 7.5, ymin = 0, ymax = 1), colour="orange", alpha = 0, size = 1.5)+
      guides(fill = guide_legend(title = "Ethnicity/Race")) +
      ggtitle("Proportions of Ethnic Groups across Campus")
  })
  
  output$raw_data <- DT::renderDataTable({
    datatable(raw_anon_data, 
              class = 'cell-border stripe', 
              filter = "top",
              rownames = FALSE,
              options = list(lengthMenu = c(5, 15, 50, 100, 1000)))
  })
  
  # output$ziptable <- DT::renderDataTable({
  #   df <- cleantable %>%
  #     filter(
  #       Score >= input$minScore,
  #       Score <= input$maxScore,
  #       is.null(input$states) | State %in% input$states,
  #       is.null(input$cities) | City %in% input$cities,
  #       is.null(input$zipcodes) | Zipcode %in% input$zipcodes
  #     ) %>%
  #     mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  #   action <- DT::dataTableAjax(session, df)
  #
  #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  # })
}