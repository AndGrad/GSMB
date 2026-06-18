## load environment
source("scripts/load_environment.R")

data_demographics <- 
  all_data %>% 
  dplyr::select(IDself, 
                gender,
                age,
                classNr
                ) %>% 
  distinct() %>%  
  mutate(gender_dummy = ifelse(gender == "male", 1, 0))

  ## load data
  folder <- 'data_experiments/networks/'
  full_edge_list <- list()
  plots = list()
  
  ## Load the networks
  ## go through different datasets and load classes
  
  list_edge_files <- list.files(path = folder)
  
  for (t in 1: length(list_edge_files)){
    this_list <- list_edge_files[t]
    edge_list <- read.table(paste0(folder, this_list), header = TRUE)
    edge_list$classNr <-  as.numeric(substr(as.character(edge_list$IDself[1]), 1, 3))

  
  #### STEP 1: IMPORT DATA ####  
  
  # Assign class number to a variable 
  classNumber <- edge_list$classNr[1]  
  edgesDF <- read.table(paste(folder, this_list, sep=''), header =  TRUE)
  demographics <- data_demographics %>% 
    dplyr::filter(classNr == classNumber & !is.na(gender) ) 
    
  #how many people in the class?
  totClassNr <- nrow(demographics)
  
  #create undirected and directed networks 
  DFNetwork0 <- graph_from_data_frame(d = edgesDF, directed = "T")  #directed 
  
  indeg <- degree(DFNetwork0) 
  indeg_plot <- degree(DFNetwork0) *1.2
  
  # remove nodes of people that did not participate to estimate undirected network measures
  IDs <- as.numeric(V(DFNetwork0)$name)
  edgesDF$IDself <- as.character(edgesDF$IDself)
  
  ###PLOT DIRECTED NETWORK
  gender_missing = 'unknown'
  
  missing_ppts <- NULL
  
  if (length(IDs) > length(unique(demographics$IDself))){
    
  missing_ppts <- data.frame( IDself = setdiff(IDs, unique(demographics$IDself)),
                              gender = gender_missing,
                              gender_dummy = 2,
                              age = NA,
                              classNr = classNumber)
  
  demographics <- rbind(demographics, missing_ppts)
  
  } 
  
  if (length(IDs) < length(unique(demographics$IDself))){
    
    missing_ppts <- setdiff(unique(demographics$IDself), IDs)
    
    if(length(missing_ppts) > 0){
      
    demographics <- demographics %>% 
      filter(IDself != missing_ppts)
}
    }
                     
  #demographics[is.na(demographics)] <- 0
  
  #remove self nominations
  V(DFNetwork0)$gender <- demographics$gender
  V(DFNetwork0)$indeg <- indeg
  gender_colors <- c( "female" = "#ba543d", "male" = "#56ae6c", "unknown" ="#FFFFFF")
  V(DFNetwork0)$gender_colors <- gender_colors[V(DFNetwork0)$gender]
  
  DFNetwork0 <- igraph::simplify(DFNetwork0)
  
  median_indegree <- median(V(DFNetwork0)$indeg)
  
  # ggsave("light_theme_network.png", plot = gephi_style_plot, width = 12, height = 10, dpi = 300)
  
  layout <- create_layout(DFNetwork0, layout = 'dh')
  
  plot_graph <- ggraph(layout) +
    geom_edge_fan(
      color = "gray60",
      alpha = 0.6,
      width = 0.3,
      arrow = arrow(length = unit(2, 'mm')),
      end_cap = circle(3, 'mm')
    ) +
      geom_node_point(
        aes(size = indeg, fill = gender),
        shape = 21,
        color = "black",
        stroke = 1,
        alpha = 0.8
    ) +
    scale_size_continuous(range = c(1, 8)) +
    scale_fill_manual(values = gender_colors) +
    guides(size = "none") +
    geom_node_text(
      aes(label = indeg),
      #point.padding = unit(0.2, "lines"),
      data = . %>% filter(indeg > median_indegree),
      color = "black",
      size = 3.5
    ) +
    theme_graph(
      base_family = 'sans',
      background = 'white',
      plot_margin = margin(10, 10, 10, 10)
    ) +
    labs(
      color = "Gender",
      title = paste0(
        "Class ID = ",classNumber
    ),
    subtitle = 
  paste0('Median age = ',
    round(median(na.omit(demographics$age)), 0), ", N = ", length(V(DFNetwork0))
)) +
    theme(
      #legend.position = "right",
      legend.position = "none",
      legend.background = element_rect(fill = "white"),
      #  legend.key = element_rect(fill = "white"),
      legend.text = element_text(color = "black"),
      legend.title = element_text(color = "black", face = "bold"),
      plot.title = element_text(color = "black", size = 16, face = "bold"),
      plot.subtitle = element_text(color = "black", size = 12),
      plot.background = element_rect(fill = "white", color = "lightgrey")
    )
  
  plot_graph
  
  node_data <- igraph::as_data_frame(DFNetwork0, what = 'vertices')

  indegree_hist <- ggplot(node_data, aes(x = indeg, fill = gender)) +
    geom_bar(position = "stack", alpha = 0.8, color="black", size=0.2,width = 1) +
    scale_fill_manual(values = gender_colors) +
    labs(x = "Indegree", y = "Count") +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = "white")
    )
  
  indegree_hist
  # Combine the main plot and the inset histogram
  final_plot <- plot_graph + 
    patchwork::inset_element(indegree_hist, left = 0.6, bottom = 0.85, right = 0.98, top = 0.98, align_to = 'full', 
                             on_top  = FALSE)
  final_plot
  plots[[t]] <- final_plot
  
  ggsave(paste0("plots/networks/network",classNumber, ".png"), plot = final_plot, width = 6, height = 6, dpi = 300)

  write_graph(file = paste0('data_experiments/networks_gephi/',classNumber,'gephi.graphml'), DFNetwork0, format = "graphml")
  
 # # pdf(paste("plots/networks/network",classNumber,".pdf", sep = ""))
 #  png(paste("plots/networks/network",classNumber,".png", sep = ""), width = 1200, height = 1200, res = 220)
 #  
 # 
 #  plot(
 #    DFNetwork0,
 #  layout = layout_with_dh,
 #  font.label.family = "Helvetica",
 #  font.label.style = "bold",
 #  vertex.label = NA,
 #  vertex.label.cex =0,
 #  vertex.label.color = "blue",
 #   vertex.label.style = "bold",
 #  vertex.label.dist = 0.5,
 #  edge.color = "grey",
 #    edge.width = 1,
 #    edge.arrow.size  = .2,
 #   vertex.size = V(DFNetwork0)$indeg*0.6,
 #   vertex.label = V(DFNetwork0)$indeg,
 #   vertex.frame.color = V(DFNetwork0)$gender_colors,
 #   vertex.color = V(DFNetwork0)$gender_colors,
 #   vertex.color.alpha = .6,
 #    main = paste(
 #      "Class ID = ",
 #      classNumber,
 #      ', Median age = ',
 #      round(median(na.omit(demographics$age)), 0),
 #      sep = ''
 #    ),
 #  )
 #  
 #  
 #  
 #  
 #  #legend("bottomleft", c("female", "male", "no consent"), pch = 21,col= "#777777", pt.bg = c( "#ba543d", "#56ae6c", "#FFFFFF") , pt.cex = 2, cex=1, bty = "n")
 #  
 #  dev.off()
 #  plot.new()
 #  
}
  ## Make combined plots
  
  plot1 <-
    (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]) + plot_layout(guides = "collect")
  plot2 <-
    (plots[[5]] + plots[[6]]) / (plots[[7]] + plots[[8]]) + plot_layout(guides = "collect")
  plot3 <-
    (plots[[9]] + plots[[10]]) / (plots[[11]] + plots[[12]]) +  plot_layout(guides = "collect")
  plot4 <-
    (plots[[13]] + plots[[14]]) / (plots[[15]] + plots[[16]]) + plot_layout(guides = "collect")
  plot5 <-
    (plots[[17]] + plots[[19]]) / (plots[[20]] + plots[[21]]) + plot_layout(guides = "collect")
  plot6 <-
    (plots[[22]] + plots[[23]]) / (plots[[24]]) + plot_layout(guides = "collect")
  
  plot1
  ggsave(("plots/networks/combined1.png"), plot = plot1, width = 10, height = 12, dpi = 300)
  ggsave(("plots/networks/combined2.png"), plot = plot2, width = 10, height = 12, dpi = 300)
  ggsave(("plots/networks/combined3.png"), plot = plot3, width = 10, height = 12, dpi = 300)
  ggsave(("plots/networks/combined4.png"), plot = plot4, width = 10, height = 12, dpi = 300)
  ggsave(("plots/networks/combined5.png"), plot = plot5, width = 10, height = 12, dpi = 300)
  ggsave(("plots/networks/combined6.png"), plot = plot6, width = 10, height = 12, dpi = 300)
  
  plot2
  plot3  
  plot4
  plot5    
  plot6  
  
  