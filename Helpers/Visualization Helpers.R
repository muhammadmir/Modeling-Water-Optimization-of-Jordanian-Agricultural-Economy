library(webr)
library(networkD3)

#' An internal function utilized to generate complex graphics.
#'
#' This function converts production, import, and export tibble into a single
#' "long format" tibble for sankey network and pie donut visuals. It is not
#' limited to quantity or revenue versions of these tibbles. If sum_quantity
#' is true, it sums the Quantity column for each tibble.
#'
#' Note: The variable countries (all unique countries involved in trade), and
#' TARGET (the name of the target country that is being optimized) should
#' already be declared to use this function.
#'
#' @param prod_df A dataframe.
#' @param import_df A dataframe.
#' @param export_df A dataframe.
#' @param sum_quantity A logical. Defaults to false.
#'
#' @return A "long format" tibble of the argument tibbles.
#'
long_format <- function(prod_df, import_df, export_df, sum_quantity=FALSE) {
    temp_country <- rep(
        countries,
        times = ifelse(sum_quantity, 1, ncol(import_df))
    )

    a <- prod_df %>%
        gather(key = "Item", value = "Quantity") %>%
        add_column(Type = "Production", .before = "Item") %>%
        as_tibble()

    b <- import_df %>%
        mutate(Type = "Import") %>%
        gather(key = "Item", value = "Quantity", -Type) %>%
        as_tibble()

    c <- export_df %>%
        mutate(Type = "Export") %>%
        gather(key = "Item", value = "Quantity", -Type) %>%
        as_tibble()

    if (sum_quantity) {
        b <- b %>%
            group_by(Type, Item) %>%
            summarize(Quantity = sum(Quantity))

        c <- c %>%
            group_by(Type, Item) %>%
            summarize(Quantity = sum(Quantity))

        return(bind_rows(a, b, c))
    }

    a <- a %>% add_column(Country = TARGET, .before = "Item")
    b <- b %>% add_column(Country = temp_country, .before = "Item")
    c <- c %>% add_column(Country = temp_country, .before = "Item")

    return(bind_rows(a, b, c))
}

#' Creates objective function space plot.
#'
#' This function plots the objective function space of out_df, as created in
#' Analysis.R. Whether or not out_df is the Pareo Fronter can be specified by
#' the optimal argument. The graph axes are labeled by xlab and ylab.
#' Optionally, the baseline water use and revenue can be plotted on the graph.
#' An accompanying legend based on specifications above is placed in the top
#' right.
#'
#' @param out_df A dataframe.
#' @param xlab A character.
#' @param ylab A character.
#' @param max_water_use A numeric. Defaults to false.
#' @param min_revenue A numeric. Defaults to false.
#' @param optimal A logical. Defaults to false.
#'
#' @return Prints plot.
#'
print_objective_space_diagram <- function(out_df, xlab, ylab, max_water_use=FALSE, min_revenue=FALSE, optimal=TRUE) {
    labels <- c()
    values <- c()

    g1 <- ggplot(out_df, aes(x = f_1, y = f_2)) + labs(x = xlab, y = ylab)

    if (max_water_use && min_revenue) {
        # Get shaded region below for gap between out_df and baseline input
        toprightmost <- out_df[out_df$f_2 == max(out_df$f_2), ]
        new_df <- tibble(
            f_1 = c(toprightmost$f_1 - 0.1, max_water_use),
            f_2 = c(toprightmost$f_2, toprightmost$f_2)
        )

        g1 <- g1 + geom_ribbon(
            data = new_df,
            aes(ymin = min(out_df$f_2), ymax = f_2),
            fill = "#D3D3D3"
        )

        # Graph baseline point
        g1 <- g1 +
        geom_point(
            aes(x = max_water_use, y = min_revenue, color = "blue"),
            size = 2
        )

        # For Legend
        labels <- c(labels, "Baseline")
        values <- c(values, "blue" = "blue")
    }

    if (optimal) {
        g1 <- g1 +
        geom_ribbon(aes(ymin = min(f_2), ymax = f_2), fill = "#D3D3D3") +
        geom_line(color = "red", linewidth = 2) +
        geom_point(aes(color = "green"), size = 1)

        # For Legend
        labels <- c(labels, "Pareto Optimal")
        values <- c(values, "green" = "green")
    } else {
        g1 <- g1 + geom_point(aes(color = "black"), size = 2)

        # For Legend
        labels <- c(labels, "Dominated")
        values <- c(values, "black" = "black")
    }

    g1 <- g1 +
    scale_color_manual(
        name = "Solution Type",
        labels = labels,
        values = values
    ) +
    scale_x_continuous(labels = function(x) {
        return(prettyNum(x, big.mark = ",", scientific = FALSE))
    }) +
    scale_y_continuous(labels = function(y) {
        return(prettyNum(y, big.mark = ",", scientific = FALSE))
    }, expand = c(0, 0)) +
    theme_classic(base_size = 16) +
    theme(legend.position = c(0.1, 0.9))

    print(g1)
}

#' Creates sankey diagram (not completely developed).
#'
#' Plots a sankey diagram showing quantity flow of commodities from target and
#' trading countries, grouped by type (Production, Import, Export). This
#' function needs more work so it is more visually appealing.
#'
#' Note: The variable countries (all unique countries involved in trade), and
#' TARGET (the name of the target country that is being optimized) should
#' already be declared to use this function.
#'
#' @param prod_df A dataframe.
#' @param import_df A dataframe.
#' @param export_df A dataframe.
#'
#' @return Prints plot.
#'
print_sankey_diagram <- function(prod_df, import_df, export_df) {
    lf_df <- long_format(prod_df, import_df, export_df, sum_quantity = FALSE)
    # Links
    links <- lf_df %>% as.data.frame()

    # Nodes
    nodes <- data.frame(
        "NodeID" = c(
            unique(as.character(links$Country)),
            unique(as.character(links$Item))
        ),
        "Group" = as.factor(c("Node"))
    )

    # Link IDs
    links$ID_Item <- match(links$Item, nodes$NodeID) - 1
    links$ID_Country <- match(links$Country, nodes$NodeID) - 1

    # Coloring
    domains <- '["Production", "Import", "Export", "Node"]'
    colors <- '["orange", "pink", "yellow", "grey"]'
    js <- paste("d3.scaleOrdinal().domain(", domains, ").range(", colors, ")")

    p <- sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "ID_Item",
        Target = "ID_Country",
        Value = "Quantity",
        NodeID = "NodeID",
        LinkGroup = "Type",
        NodeGroup = "Group",
        colourScale = js,
        fontSize = 12,
        nodeWidth = 25
    )

    qty_disply_js <- 'function(el, x){
        d3.select(el).selectAll(".node text")
        .text(d => d.name + " (" + d3.format("(.0f")(d.value) + ")");
    }'

    legend_js <- 'function(el, x, data) {
        var svg = d3.select("svg");
        
        let domains = ["Production", "Import", "Export", "Node"];
        let colors = ["orange", "pink", "yellow", "grey"];
        
        colors.forEach((color) => {
            svg.append("circle").attr("cx",25).attr("cy",10).
            attr("r", 6).style("fill", color)
        });

        domains.forEach((element) => {
            svg.append("text").attr("x", 35).attr("y", 10)
            .text(element).style("font-size", "15px")
            .attr("alignment-baseline","middle")
        });
    }'

    #htmlwidgets::onRender(x = p, jsCode = qty_disply_js)
    #htmlwidgets::onRender(x = p, jsCode = legend_js)

    print(p)
}

#' Creates pie donut plot.
#'
#' This function plots the pie donut graph of the production, import and export
#' tibbles It is not limited to quantity or revenue versions of these tibbles.
#' The inner pie plots the different aggregate proportions of the crops while
#' the outter donut plots the proportions OF THAT SAME CROP in terms of
#' Production, Import, and Export.
#'
#' Note: The variable countries (all unique countries involved in trade), and
#' TARGET (the name of the target country that is being optimized) should
#' already be declared to use this function.
#' 
#' @param prod_df A dataframe.
#' @param import_df A dataframe.
#' @param export_df A dataframe.
#'
#' @return Prints plot.
#'
print_pie_donut_diagram <- function(prod_df, import_df, export_df) {
    lf_sum_df <- long_format(prod_df, import_df, export_df, sum_quantity = TRUE)

    PieDonut(
        lf_sum_df %>% group_by(Type, Item),
        aes(Item, Type, count = Quantity),
        r0 = 0.2,
        color = "black",
        showRatioThreshold = 0.02,
        labelposition = 1,
        labelpositionThreshold = 0.1,
        donutLabelSize = 4
    )
}
