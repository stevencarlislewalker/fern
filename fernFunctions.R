makeLatex <- function(file, content, height = 0.5) {
    require(tikzDevice)
    require(grid)
    fileTex <- paste0(file, ".tex")
    tikz(fileTex, standAlone = TRUE,
         width = 6, height)
    pushViewport(dataViewport(1:10, 1:10))
    grid.tikzNode(5, 5, content = content)
    dev.off()
    system(paste0("pdflatex ", fileTex))
    system(paste0("convert ", file, ".pdf", " ", file, ".png"))
}

makeIntervalPlot <- function(dat) {
    dat %>%
        filter(!is.na(interval)) %>%
        ggplot() + theme_bw() + 
        geom_line(aes(contractionID, as.numeric(interval) / 60)) +
        geom_ribbon(aes(x = contractionID, ymin = 3, ymax = 4),
                    alpha = 0.4) +
            scale_x_continuous("Contraction ID")
}
