plot_leaflet_map <- function(map_object, variable_to_plot, name_of_region,
                             popup_text, end_text = "", popup_round = 2, frame_height = 500, legend_digits = 0){
    
    # NSE
    variable <- enquo(variable_to_plot)
    variable_str <- quo_name(variable)
    name_of_region <- enquo(name_of_region)
    
    # Find breaks
    brks <- classIntervals(map_object@data[, variable_str], n = 7, style = "jenks")
    brks <- brks$brks
    # "YlGnBu" "YlOrRd"
    pal <- colorBin("YlGnBu", domain = map_object@data[, variable_str], bins = brks)
    
    
    map_object@data <- map_object@data %>%
        mutate(popup = stringr::str_c("<strong>", !!name_of_region, "</strong>",
                                      "<br/>",
                                      popup_text, round(!!variable, popup_round), end_text) %>%
                   purrr::map(htmltools::HTML))
    
    leaflet(data = map_object) %>%
        addPolygons(label = ~popup,
                    fillColor = ~pal(map_object@data[, variable_str]),
                    color = "#444444",
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = 0.7,
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
        addLegend(pal = pal,
                  values = map_object@data[, variable_str],
                  opacity = 0.7,
                  title = NULL,
                  position = "bottomright",
                  labFormat = labelFormat(digits = legend_digits)) %>%
        frameWidget(height = frame_height)
}

r2_decomposition <- function(model){
    variables <- colnames(model$model)
    coefs <- model$coefficients[variables]
    data <- model$model
    # DF_normalized <- lapply(model$model, function(x) (x - mean(x)) / sd(x))
    # x_y_hat_cor <- lapply(data, function(x) cov(x, model$fitted.values))
    x_y_hat_cor <- lapply(data, function(x) cov(x, model$model[, 1]))
    r2_decomposed <- list()
    for (coef in names(x_y_hat_cor)){
        r2_decomposed[coef] <- x_y_hat_cor[[coef]] * coefs[coef]
    }
    return(r2_decomposed)
}

normalize_DF <- function(data){
    data <- lapply(data, function(x) (x - mean(x)) / sd(x))
    return(data)
}

RF_variables_importance <- function(dataset, model_formula, variables_DF,
                                    col_fill = "#2c7fb8", ntree = 1000, mtry = 3, 
                                    nodesize = 5, popup_round = 2, 
                                    plot_title = "Waga zmiennych w wyjaśnianiu zjawiska",
                                    y_title = "Procentowy wzrost MSE po permutacji danej zmiennej"){
    
    #' This function creates plot of variables importance in Random Forest model
    #' 
    #' https://explained.ai/rf-importance/
    #' https://www.displayr.com/how-is-variable-importance-calculated-for-a-random-forest/
    
    set.seed(1)
    model_rf <- randomForest::randomForest(model_formula, data = dataset, ntree = ntree, mtry = mtry, 
                                           nodesize = nodesize, importance = T)
    v_importance <- randomForest::importance(model_rf, type = 1, scale = T) %>% as.data.frame()
    v_importance <- v_importance %>% round(popup_round)
    colnames(v_importance) <- c("Importance")
    v_importance$variable <- rownames(v_importance)
    v_importance <- v_importance %>%
        left_join(variables_DF, by = "variable")
    
    p <- ggplot(v_importance)+
        geom_col(fill = col_fill, aes(x = reorder(variable_name, -Importance), y = Importance))+
        ylab(y_title)+
        xlab("")+
        ggtitle(plot_title)+
        coord_flip()+
        theme_bw()
    
    plotly::ggplotly(p, tooltip = c("y"))
}

# Funkcja do wylapywania statystyki morana
return_moran <- function(moran){
    stat <- moran$estimate[1] %>% round(3) %>% as.character()
    p <- moran$p.value
    if (p > 0.1){
        return(stat)
    } else if (p > 0.05){
        return(paste0(stat, "*"))
    } else if (p > 0.01){
        return(paste0(stat, "**"))
    } else {
        return(paste0(stat, "***"))
    }
}
return_moran_p_val <- function(moran_test, round_val = 3){
    p <- moran_test$p.value
    return(as.character(round(p, round_val)))
}
