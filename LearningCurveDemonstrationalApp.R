#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### Libraries ####
library(shiny)
library(rhandsontable)
library(ggplot2)
library(tidyr)
library(stats)
library(rmarkdown)



#### Shiny UI #### 
ui <- 
  fluidPage(
   # Application title
   titlePanel("Lego Learning Curve"),
   
   sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel(
            "Introduction",
            htmlOutput("intro")
          ),
          
          tabPanel(
            "Analysis",
            htmlOutput("BlankSpace"),
            uiOutput("selectCol1"),
            uiOutput("selectCol2"),
            uiOutput("selectCol3"),
            uiOutput("selectCol4")

            
          )
        )
        
        
        
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          
          tabPanel(
            
            "Data Entry", 
            rHandsontableOutput("HandTable")

          ),
          
          tabPanel(
            
            "Individual Plot",
            uiOutput("plotselect"),
            plotOutput("Individual"),
            htmlOutput("equationind")

            ),
          
          tabPanel(
            
            "Combined Plot", 
            selectInput(inputId = "selectCombined", #name of input
                        label = "Select a Plot:", #label displayed in ui
                        choices = c("Data Only","Fitted Curves","Data and Curves"),
                        # calls unique values from the State column in the previously created table
                        selected = c("Fitted Curves")), #default choice (not required)
            plotOutput("Combined"),
            htmlOutput("equation1"),
            htmlOutput("equation2"),
            htmlOutput("equation3"),
            htmlOutput("equation4")
            
          )
        )
      )
   )
)

#### Shiny Server ####
server <- function(input, output) {
  ##### Defining Data Tables and Select Values #####  

  output$BlankSpace<-renderText({
    " <br> "
  })
  
  LegoSetColor=data.frame(Red = c("Windmill (1 Brick)","Car (2 Brick)","Crab (2 Brick)"),
                          Blue = c("House (1 Brick)","Whale (2 Brick)","Train (3 Brick)"),
                          Orange=c("Shop (1 Brick)","Lion (2 Brick)","Plane (2 Brick)"),
                          Green=c("Alligator (1 Brick)","Castle (1 Brick)","Tractor (3 Brick)"))
  

  DF = data.frame(Trial = 1:3,
                  Set1 = 0,
                  Set2 = 0,
                  Set3 = 0,
                  Set4 = 0)

  output$selectCol1 = renderUI({ #creates select box object called in ui
    ColorSel<-input$selected
    selectInput(inputId = "selectedCol1", #name of input
                label = "Select a Lego Set for the first column:", #label displayed in ui
                choices = LegoSetColor
                # calls unique values from the State column in the previously created table
                 #default choice (not required)
    )
    })
  output$selectCol2 = renderUI({ #creates select box object called in ui

    selectInput(inputId = "selectedCol2", #name of input
                label = "Select a Lego Set for the second column:", #label displayed in ui
                choices = LegoSetColor
                # calls unique values from the State column in the previously created table
                
    )
  })
  output$selectCol3 = renderUI({ #creates select box object called in ui

    selectInput(inputId = "selectedCol3", #name of input
                label = "Select a Lego Set for the third column:", #label displayed in ui
                choices = LegoSetColor
                # calls unique values from the State column in the previously created table
                
    )
  })
  output$selectCol4 = renderUI({ #creates select box object called in ui

    selectInput(inputId = "selectedCol4", #name of input
                label = "Select a Lego Set for the fourth column:", #label displayed in ui
                choices = LegoSetColor
                # calls unique values from the State column in the previously created table
    )
  })

 output$HandTable <- 
   renderRHandsontable({ 
     
    names(DF)[2]<-paste("(1)",as.character(input$selectedCol1))
    names(DF)[3]<-paste("(2)",as.character(input$selectedCol2))
    names(DF)[4]<-paste("(3)",as.character(input$selectedCol3))
    names(DF)[5]<-paste("(4)",as.character(input$selectedCol4))
    rhandsontable(DF, width = 700, height = 100)
   
    })
 
 indat <- reactiveValues(data=DF)
 
 output$plotselect <- renderUI({
   SpecificSet<-names(indat$data)[2:5]
   
   selectInput(inputId = "setselect", #name of input
               label = "Select a Plot:", #label displayed in ui
               choices = SpecificSet,
               # calls unique values from the State column in the previously created table
               selected = c(SpecificSet[1])) #default choice (not required)
 })
 
 observe({
   if(!is.null(input$HandTable))
     indat$data <- hot_to_r(input$HandTable)
   
 })  
 
 datalong<-reactive({
   
   gather(data = indat$data,key = "Lego",value = "Duration",2:5)
   
 })
 
 output$intro<-renderText({"<br>
                          A learning curve demonstrates how operaters improve the speed at which they can complete a task through training.<br>
                          <br>
                          In this application, operators will build a Lego set 3 times and track the duration it takes to complete it. From this data, learning curves will be generated. <br>
                          On the <b>Analysis</b> tab, an operator selects the Lego set that they will build from the dropdown menu for each of the table columns.<br>
                          <br>
                          Data can be entered in the <b>Data Entry</b> tab. 
                          The data is then automatically plotted on the <b>Individual Plots</b> and <b>Combined Plots</b> tabs.
                          Several graphs are available from the dropdown menu on each page. <br>
                          <br>"
 })

 ##### Equations #####
 
 DF2<-reactive({
   data.frame(x=indat$data[,1],
              y1=indat$data[,2],
              y2=indat$data[,3],
              y3=indat$data[,4],
              y4=indat$data[,5])
 })

 SumCol1<-reactive({sum(indat$data[,2])})
 SumCol2<-reactive({sum(indat$data[,3])})
 SumCol3<-reactive({sum(indat$data[,4])})
 SumCol4<-reactive({sum(indat$data[,5])})
 
 output$equation1<-renderText({
   

   if(SumCol1()==0){
     paste(as.character(names(indat$data)[2]),
           ": Enter Data  (Red) <br>")
   } else{
     paste(as.character(names(indat$data)[2]),
           ": y = ",
           round((indat$data[3,2])/(3^((log10(indat$data[3,2])-log10(indat$data[1,2]))/(log10(3)-log10(1)))),4),
           "x^(",
           round(((log10(indat$data[3,2])-log10(indat$data[1,2]))/(log10(3)-log10(1))),4),
           ")   (Red) <br>"
     )
   }
   
   })
 
 output$equation2<-renderText({
   if(SumCol2()==0){
     paste(as.character(names(indat$data)[3]),
           ": Enter Data   (Green) <br>")
   } else{
     paste(as.character(names(indat$data)[3]),
           ": y = ",
           round((indat$data[3,3])/(3^((log10(indat$data[3,3])-log10(indat$data[1,3]))/(log10(3)-log10(1)))),4),
           "x^(",
           round(((log10(indat$data[3,3])-log10(indat$data[1,3]))/(log10(3)-log10(1))),4),
           ")   (Green) <br>"
     )
   }
   
   
   })
 
 output$equation3<-renderText({

       if(SumCol3()==0){
       paste(as.character(names(indat$data)[4]),
             ": Enter Data     (Blue) <br>")
     } else{
       paste(as.character(names(indat$data)[4]),
             ": y = ",
             round((indat$data[3,4])/(3^((log10(indat$data[3,4])-log10(indat$data[1,4]))/(log10(3)-log10(1)))),4),
             "x^(",
             round(((log10(indat$data[3,4])-log10(indat$data[1,4]))/(log10(3)-log10(1))),4),
             ")   (Blue) <br>")
     }
    
   })
 
 output$equation4<-renderText({
   
   if(SumCol4()==0){
     paste(as.character(names(indat$data)[5]),
           ": Enter Data     (Blue) <br>")
   } else{
     paste(as.character(names(indat$data)[5]),
           ": y = ",
           round((indat$data[3,5])/(3^((log10(indat$data[3,5])-log10(indat$data[1,5]))/(log10(3)-log10(1)))),4),
           "x^(",
           round(((log10(indat$data[3,5])-log10(indat$data[1,5]))/(log10(3)-log10(1))),4),
           ")   (Purple) <br>")
   }
   
 })
 
 DF3<-reactive({
   data.frame(x=indat$data[,1],
              y=indat$data[,input$setselect])
 })
 
 output$equationind<-renderText({
   if(sum(DF3()[,2])==0){
     paste(as.character(input$setselect),
           ": Enter Data")
   } else{
     paste(as.character(input$setselect),
           ": y = ",
           round((indat$data[3,input$setselect])/(3^((log10(indat$data[3,input$setselect])-log10(indat$data[1,input$setselect]))/(log10(3)-log10(1)))),4),
           "x^(",
           round(((log10(indat$data[3,input$setselect])-log10(indat$data[1,input$setselect]))/(log10(3)-log10(1))),4),
           ")   (Magenta) <br>"
     )
   }
 })
 
##### Generate Plots #####
 output$Individual<- renderPlot({
   
   eqind<-function(x){(indat$data[3,input$setselect])/(3^((log10(indat$data[3,input$setselect])-log10(indat$data[1,input$setselect]))/(log10(3)-log10(1))))*x^((log10(indat$data[3,input$setselect])-log10(indat$data[1,input$setselect]))/(log10(3)-log10(1)))}
   
   ggplot(data =indat$data,aes(indat$data[,1],indat$data[,input$setselect]))+
     geom_point()+
     geom_line()+
     ggtitle(paste(input$setselect))+
     theme(plot.title = element_text(size=20, hjust = .5))+
     xlab("Trials")+
     ylab("Duration")+
     scale_y_continuous(expand=c(.1,.1),limits = c(min(indat$data[,input$setselect]),NA))+
     scale_x_continuous(breaks = seq(1,3,1))+
     stat_function(fun = eqind, color = "magenta")
 })
 
 eq1<-function(x){(indat$data[3,2])/(3^((log10(indat$data[3,2])-log10(indat$data[1,2]))/(log10(3)-log10(1))))*x^((log10(indat$data[3,2])-log10(indat$data[1,2]))/(log10(3)-log10(1)))}
 eq2<-function(x){(indat$data[3,3])/(3^((log10(indat$data[3,3])-log10(indat$data[1,3]))/(log10(3)-log10(1))))*x^((log10(indat$data[3,3])-log10(indat$data[1,3]))/(log10(3)-log10(1)))}
 eq3<-function(x){(indat$data[3,4])/(3^((log10(indat$data[3,4])-log10(indat$data[1,4]))/(log10(3)-log10(1))))*x^((log10(indat$data[3,4])-log10(indat$data[1,4]))/(log10(3)-log10(1)))}
 eq4<-function(x){(indat$data[3,5])/(3^((log10(indat$data[3,5])-log10(indat$data[1,5]))/(log10(3)-log10(1))))*x^((log10(indat$data[3,5])-log10(indat$data[1,5]))/(log10(3)-log10(1)))}
 
 output$Combined <-    renderPlot({ 
   
   if (input$selectCombined == "Data Only"){
   
     ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
       geom_point(aes(shape = Lego))+
       geom_line()+
       ggtitle(paste(input$selected," Set: Data"))+
       theme(plot.title = element_text(size=20, hjust = .5))+
       xlab("Trials")+
       scale_y_continuous(expand=c(.1,.1),limits = c(min(datalong()[,3]),NA))+
       scale_x_continuous(breaks = seq(1,3,1))

 } else if (input$selectCombined == "Fitted Curves"){
   if(SumCol1() == 0 || SumCol2() == 0 || SumCol3() == 0){
    
      ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
       ggtitle("Please Completely Fill the Data Table")+
       theme(plot.title = element_text(size=20, hjust = .5))+
       xlab("Trials")+
       scale_y_continuous(expand=c(.1,.1),limits = c(min(datalong()[,3]),NA))+
       scale_x_continuous(breaks = seq(1,3,1))
     
     } else {
  
     ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
       ggtitle(paste(input$setselect," Set: Power Curves"))+
       theme(plot.title = element_text(size=20, hjust = .5))+
       xlab("Trials")+
       scale_y_continuous(expand=c(.1,.1),limits = c(min(datalong()[,3]),NA))+
       scale_x_continuous(breaks = seq(1,3,1))+ 
         stat_function(fun = eq1, color = "#FF6666")+
         stat_function(fun = eq2, color = "#669900")+
         stat_function(fun = eq3, color = "#56B4E9")+
         stat_function(fun = eq4, color = "#9933FF")

   }
 } else {
   if(SumCol1() == 0 || SumCol2() == 0 || SumCol3() == 0){
     
     ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
       geom_point(aes(shape = Lego))+
       geom_line()+
       ggtitle("Fill Data Table for Fitted Curves")+
       theme(plot.title = element_text(size=20, hjust = .5))+
       xlab("Trials")+
       scale_y_continuous(expand=c(.1,.1),limits = c(min(datalong()[,3]),NA))+
       scale_x_continuous(breaks = seq(1,3,1))


   } else{

     LegSort<-unique(datalong()[,2])
     
     ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
       geom_point(aes(shape = Lego))+
       geom_line()+
       ggtitle(paste(input$setselect," Set: Data and Power Curves"))+
       theme(plot.title = element_text(size=20, hjust = .5))+
       xlab("Trials")+
       scale_y_continuous(expand=c(.1,.1),limits = c(min(datalong()[,3]),NA))+
       scale_x_continuous(breaks = seq(1,3,1))+
       stat_function(fun = eq1, color = "#FF6666")+
       stat_function(fun = eq2, color = "#669900")+
       stat_function(fun = eq3, color = "#56B4E9")+
       stat_function(fun = eq4, color = "#9933FF")

     }
 }
})

 
 
 outputOptions(output, "selectCol1", suspendWhenHidden = FALSE)
 outputOptions(output, "selectCol2", suspendWhenHidden = FALSE)
 outputOptions(output, "selectCol3", suspendWhenHidden = FALSE)
 outputOptions(output, "selectCol4", suspendWhenHidden = FALSE)
 }

##### Run the application #####
shinyApp(ui = ui, server = server)

