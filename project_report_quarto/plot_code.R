# Create a visualisation of the cyclically transformed time variable:
working_df %>% 
  select(uke, sin, cos) %>% 
  arrange(uke) %>% 
  {
    # Specify where to save the finished plot:
    pdf("figures/cyclic_time.pdf",
        width  = 8,
        height = 5)
    
    # Adjust padding:
    par(mar = c(4, 4, 2, 2))
    
    attach(.)  # Lets me select columns by name without indexing.
    
    # Plot the sin curve:
    plot(sin~uke, 
         type = "l", 
         lwd = 2, 
         cex.lab = 1.2,
         col = "orange",
         ylab = "Yearly oscillation", 
         xlab = "Week number")
    
    # Plot the cos curve:
    lines(cos~uke, 
          lwd = 2, 
          col = "dimgray")
    
    # Add a legend:
    legend("topleft", 
           inset = c(0.05,0.08),
           legend = c("sin -1", "cos -1"), 
           col = c("orange", "dimgray"), 
           lwd = 2, 
           cex = 1.2)
    
    detach(.)  # Undo the previous attach() command.
    
    # Save the plot to PDF:
    dev.off()
  }
