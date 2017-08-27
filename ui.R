#shiny UI nathanmaps

library(shiny)

shinyUI(fluidPage(
	mainPanel(
		tabsetPanel(
		tabPanel("map",
			sliderInput("years", label = "", min = 1651, max = 1866,value = c(1651, 1866), step = 5, sep = ""),
			plotOutput("map", height = "650px", width = "800px"),
			tableOutput("maptable")),
		tabPanel("comparison",
			fluidRow(
				column(6, sliderInput("y1", label = "", min = 1651, max = 1866,value = c(1651, 1866), step = 5, sep = ""),
				plotOutput("m1", height = "650px"),
				tableOutput("mt1")),
				column(6, sliderInput("y2", label = "", min = 1651, max = 1866,value = c(1651, 1866), step = 5, sep = ""),
				plotOutput("m2", height = "650px"),
				tableOutput("mt2"))
			)),
		tabPanel("graphs",
			sidebarLayout(
			sidebarPanel(uiOutput("regions")),
			mainPanel(
				plotOutput("plot", height = "600px", width = "800px")
			))
		))
		)))