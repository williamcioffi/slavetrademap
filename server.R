#shiny server

library(shiny)
library(scales)
library(ggplot2)
library(ggmap)

dat <- read.table("newdata2.csv", header = TRUE, sep = ',')
# who <- dat[, 1:5]73:7
# dat <- dat[, 6:ncol(dat)]
# who[, 'sum'] <- apply(dat, 1, sum, na.rm = TRUE)
mapworld <- borders("world", colour = "gray50", fill = "gray50", xlim = c(-180, -25), ylim = c(-60, 70))

shinyServer(function(input, output) {
	slidervalues <- reactive({
		yy <- input$years
		seq(yy[1], yy[2], by = 5)
	})
	
	sv1 <- reactive({
		yy <- input$y1
		seq(yy[1], yy[2], by = 5)
	})
	
	sv2 <- reactive({
		yy <- input$y2
		out <- seq(yy[1], yy[2], by = 5)
	})
	
	output$plot <- renderPlot({
		dese <- 1:nrow(dat)
		theseregions <- input$regioninput
		
		if(is.null(theseregions)) return(NULL)

		dese <- which(dat$Region %in% theseregions)
		dd <- dat[dese, ]
		
		p <- ggplot(data = dd, aes(x = StartYear, y = Count, color = Region)) + geom_line() + geom_point() + xlab("5 year period starting in") + ylab("Disembarked Slaves")
		p
	})
	
	output$regions <- renderUI({
		listofregions <- unique(dat$Region)
		selectInput("regioninput", "regions", listofregions, multiple = TRUE, selected = "Saint-Domingue")
	})
	
	output$map <- renderPlot({
		dd <- dat
		makeplot(dd, slidervalues())
	})
	
	output$maptable <- renderTable({
		dd <- dat
		maketable(dd, slidervalues())
	})
	
	output$m1 <- renderPlot({
		dd <- dat
		makeplot(dd, sv1())
	})
	
	output$mt1 <- renderTable({
		dd <- dat
		maketable(dd, sv1(), cols = c(1, 4))
	})
	
	output$m2 <- renderPlot({
		dd <- dat
		makeplot(dd, sv2())
	})
	
	output$mt2 <- renderTable({
		dd <- dat
		maketable(dd, sv2(), cols = c(1, 4))
	})
})

makeplot <- function(dd, yy) {
	dese <- which(dd$StartYear %in% yy)
	dd2 <- dd[dese, ]
	dd3 <- data.frame(
		Region = tapply(dd2$Region, dd2$RefSite, function(x)as.character(x[1])),
		RefSite = sort(unique(dd2$RefSite)),
		Contributor = tapply(dd2$Contributor, dd2$RefSite, function(x) as.character(x[1])),
		Total_Disembarked_Slaves = tapply(dd2$Count, dd2$RefSite, sum, na.rm = TRUE),
		Lat = tapply(dd2$Lat, dd2$RefSite, mean, na.rm = TRUE),
		Lon = tapply(dd2$Lon, dd2$RefSite, mean, na.rm = TRUE)
	)
	
	unspecs <- c("U.S.A. unspecified", "French Caribbean unspecified", "Brazil unspecified", "Other British Caribbean", "Other Spanish Americas", "Spanish Circum-Caribbean")

	dese <- which(dd3$Region %in% unspecs)
	dd4 <- dd3[-dese, ]
	
	dd4$Total_Disembarked_Slaves[which(dd4$Total_Disembarked_Slaves == 0)] <- NA
	
	mp <- ggplot() + mapworld
	mp <- mp + geom_point(aes(x = dd4$Lon, y = dd4$Lat, color = dd4$Contributor, size = dd4$Total_Disembarked_Slaves)) + scale_size_continuous(range = c(3, 18), label = comma) 
	# mp <- mp + geom_text(aes(x = dd3$Lon[dese], y = dd3$Lat[dese], label = dd3$Region[dese]), nudge_y = 4)
	mp + guides(color = guide_legend(title = "Contributor"), size = guide_legend(title = "Disembarked Slaves"))
}

maketable <- function(dd, yy, cols = 1:4) {
	dese <- which(dd$StartYear %in% yy)
	dd2 <- dd[dese, ]
	dd3 <- data.frame(
		Region = tapply(dd2$Region, dd2$RefSite, function(x)as.character(x[1])),
		RefSite = sort(unique(dd2$RefSite)),
		Contributor = tapply(dd2$Contributor, dd2$RefSite, function(x) as.character(x[1])),
		Disembarked_Slaves = tapply(dd2$Count, dd2$RefSite, sum, na.rm = TRUE),
		Lat = tapply(dd2$Lat, dd2$RefSite, mean, na.rm = TRUE),
		Lon = tapply(dd2$Lon, dd2$RefSite, mean, na.rm = TRUE)
	)
	dd3[, cols]
}