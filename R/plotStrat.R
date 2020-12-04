#' Plot Length Distributions with Aged and All Fish
#' 
#' todo: document
#' 
#' @param data A data frame
#' @param dir The directory you want to print the plots
#' @param ylimperc The percent of the maximum value in the data 
#' set that you want displayed on the y axis.
#' @param npages The number of pages you want the plots split over.
#' @param dolbins The length bins
#' 
#' @import ggplot2
plotStrat <- function(data, dir, ylimperc = 0.65, npages = 11, dolbins) {

for (ii_f in c("fleet", "fleet + state")) 
{
  withage <- data.frame(writeComps(doSexRatio(getComps(
    data[!is.na(data$lengthcm) & data$age != -1, ], 
    strat = switch(ii_f, fleet = NULL, c("state")), 
    Comps = "LEN"), Rvector = 0.50), 
    fname = file.path(dir, "ignoreme.csv"), sum1 = TRUE,
    lbins = dolbins, returns = "Uout", partition = 2), "wo" = "only aged")

  plotme <- data.frame(writeComps(doSexRatio(getComps(data[!is.na(data$lengthcm), ],
    strat = switch(ii_f, fleet = NULL, c("state")), 
    Comps = "LEN"), Rvector = 0.50), 
    fname = file.path(dir, "ignoreme.csv"), sum1 = TRUE,
    lbins = dolbins, returns = "Uout", partition = 2), "wo" = "all")

  plotme <- rbind(plotme, withage)

  colnames(plotme) <- ifelse(duplicated(gsub("\\.1", "", colnames(plotme))),
    gsub("L", "M", gsub("\\.1", "", colnames(plotme))), gsub("L", "F", colnames(plotme)))

  plotme <- stats::reshape(plotme, direction = "long", sep = "", 
    varying = grep("^[LAFM][0-9]+", colnames(plotme), value = TRUE),
    idvar = grepl("^[^LAFM]", colnames(plotme)))

  row.names(plotme) <- NULL
  plotme[, "fleet"] <- factor(plotme[, "fleet"], labels = unique(data$fleet))

  splits <- split(unique(plotme$fishyr)[order(unique(plotme$fishyr))], 
    ggplot2::cut_number(unique(plotme$fishyr)[order(unique(plotme$fishyr))], npages))

  for (ii_g in c(formula("state ~ fishyr"), formula("fishyr ~ state"))) 
  {
    pdf(file = file.path(dir, paste0("lengthedages_", 
      gsub("\\s+\\+\\s+", "X", ii_f), "_", gsub("~", "", paste(ii_g, collapse = "")), ".pdf")))

    for(ii in seq_along(splits)) {
      plotmea <- plotme[plotme$fishyr %in% splits[[ii]], , drop = TRUE]

      if ("state" %in% colnames(plotmea)) {
        plotmea[, "state"] <- factor(plotmea[, "state"], levels = c("CA", "OR", "WA"))
      }

      if (nrow(plotmea) == 0) next
      plotmea <- aggregate(formula(paste("F ~ time + fishyr + wo +", ii_f)), 
      data = plotmea, sum, na.rm = TRUE)

      if (!"state" %in% colnames(plotmea)) plotmea[, "state"] <- "all"
      
      g <- ggplot(data = plotmea,
                  aes(x = time, y = F, linetype = wo, col = interaction(state, fleet, sep = "-", lex.order = TRUE))) +   
                  geom_line(lwd = 0.7) + 
                  facet_grid(ii_g, drop = FALSE) + 
                  guides(col= guide_legend(title = "Fleet"), linetype = guide_legend(title = "")) + 
                  ylab("") + 
                  coord_cartesian(ylim=c(0, max(plotme$F)*ylimperc)) + 
                  theme_bw() + theme(strip.background = element_rect(colour="black", fill="white"))
      print(g)
    }

    dev.off()
  }
}
unlink(file.path(dir, "ignoreme.csv"))
}
