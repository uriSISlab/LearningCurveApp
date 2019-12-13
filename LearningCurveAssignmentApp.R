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
            htmlOutput("Space"),
            textInput("studname", 
                      "Enter Your Name: ", 
                      value = "", 
                      width = NULL,
                      placeholder = NULL),
            uiOutput("select"),
            textOutput("Blank"),
            downloadButton("Report.html","Generate Report")
            
            
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
            htmlOutput("equation3")
            
          )
        )
      )
   )
)

#### Shiny Server ####
server <- function(input, output) {
  ##### Defining Data Tables and Select Values #####  
  output$Blank<-renderText({
    "Enter Name Before Generating Report"
  })
  
  output$Space<-renderText({
    "<br>"
  })
  
  LegoSetColor=data.frame(Red = c("Windmill (1 Brick)","Car (2 Brick)","Crab (2 Brick)"),
                          Blue = c("House (1 Brick)","Whale (2 Brick)","Train (3 Brick)"),
                          Orange=c("Shop (1 Brick)","Lion (2 Brick)","Plane (2 Brick)"),
                          Green=c("Alligator (1 Brick)","Castle (1 Brick)","Tractor (3 Brick)"))
  
  DF = data.frame(Trial = 1:16,
                  Set1 = 0,
                  Set2 = 0,
                  Set3 = 0)

  output$select = renderUI({ #creates select box object called in ui
    LegoSets <- names(LegoSetColor)
    
    selectInput(inputId = "selected", #name of input
                label = "Select a Lego Set:", #label displayed in ui
                choices = LegoSets,
                # calls unique values from the State column in the previously created table
                selected = c(LegoSets[1]) #default choice (not required)
    )
    })

 output$HandTable <- 
   renderRHandsontable({ 
     
    names(DF)[2]<-as.character(LegoSetColor[1,input$selected])
    names(DF)[3]<-as.character(LegoSetColor[2,input$selected])
    names(DF)[4]<-as.character(LegoSetColor[3,input$selected])
    rhandsontable(DF, width = 600, height = 400)
   
    })
 
 indat <- reactiveValues(data=DF)
 
 output$plotselect <- renderUI({
   SpecificSet<-names(indat$data)[2:4]
   
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
   
   gather(data = indat$data,key = "Lego",value = "Duration",2:4)
   
 })
 
 output$intro<-renderText({"<br>
                          A learning curve demonstrates how operaters improve the speed at which they can complete a task through training.<br>
                          <br>
                          In this application, operators will be building 3 items from a Lego set. <br>
                          On the <b>Analysis</b> tab, an operator enters their name and selects the Lego set that has been provided to them from the dropdown menu.<br>
                          <br>
                          Data can be entered in the <b>Data Entry</b> tab. 
                          The data is then automatically plotted on the <b>Individual Plots</b> and <b>Combined Plots</b> tabs.
                          Several graphs are available from the dropdown menu on each page. <br>
                          <br>
                          Finally, a report can be generated in an html format with the <b>Generate Report</b> button on the <b>Analysis</b> tab. <br>
                          This report can then be sent to the instructor for assignment submission."})
 
 ##### Equations #####
 
 DF2<-reactive({
   data.frame(x=indat$data[,1],
              y1=indat$data[,2],
              y2=indat$data[,3],
              y3=indat$data[,4])
 })

 SumCol1<-reactive({sum(indat$data[,2])})
 SumCol2<-reactive({sum(indat$data[,3])})
 SumCol3<-reactive({sum(indat$data[,4])})
 
 output$equation1<-renderText({
   if(SumCol1()==0){
     paste(as.character(LegoSetColor[1,input$selected]),
           ": Enter Data   <br>")
   } else{
     paste(as.character(LegoSetColor[1,input$selected]),
           ": y = ",
           round(coef(nls(y1~a*x^b,data = DF2(),start = list(a=1,b=1)))[1],4),
           "x^(",
           round(coef(nls(y1~a*x^b,data = DF2(),start = list(a=1,b=1)))[2],4),
           ")  <br>"
     )
   }
   
   })
 
 output$equation2<-renderText({
   if(SumCol2()==0){
     paste(as.character(LegoSetColor[2,input$selected]),
           ": Enter Data   <br>")
   } else{
     paste(as.character(LegoSetColor[2,input$selected]),
           ": y = ",
           round(coef(nls(y2~a*x^b,data = DF2(),start = list(a=1,b=1)))[1],4),
           "x^(",
           round(coef(nls(y2~a*x^b,data = DF2(),start = list(a=1,b=1)))[2],4),
           ")   <br>"
     )
   }
   
   
   })
 
 output$equation3<-renderText({

       if(SumCol3()==0){
       paste(as.character(LegoSetColor[3,input$selected]),
             ": Enter Data     <br>")
     } else{
       paste(as.character(LegoSetColor[3,input$selected]),
             ": y = ",round(coef(nls(y3~a*x^b,data = DF2(),start = list(a=1,b=1)))[1],4),
             "x^(",
             round(coef(nls(y3~a*x^b,data = DF2(),start = list(a=1,b=1)))[2],4),
             ")   <br>")
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
           round(coef(nls(y~a*x^b,data = DF3(),start = list(a=1,b=1)))[1],4),
           "x^(",
           round(coef(nls(y~a*x^b,data = DF3(),start = list(a=1,b=1)))[2],4),
           ")   (Magenta) <br>"
     )
   }
 })
 
##### Generate Plots #####
 output$Individual<- renderPlot({
   
   ggplot(data =indat$data,aes(indat$data[,1],indat$data[,input$setselect]))+
     geom_point(color = as.character(input$selected))+
     geom_line()+
     ggtitle(paste(input$setselect))+
     theme(plot.title = element_text(size=20, hjust = .5))+
     xlab("Trials")+
     ylab("Duration")+
     scale_y_continuous(expand=c(.1,.1),limits = c(min(indat$data[,input$setselect]),NA))+
     scale_x_continuous(breaks = seq(1,16,1))+
     stat_smooth(method = 'nls',formula = 'y~a*x^b',method.args = list(start= c(a = 1,b=1)),se=FALSE,fullrange = TRUE,n=100,color="magenta")
 })
 
 output$Combined <-    renderPlot({ 
   if (input$selectCombined == "Data Only"){
   
     ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
       geom_point(aes(shape = Lego),color = as.character(input$selected))+
       geom_line()+
       ggtitle(paste(input$selected," Set: Data"))+
       theme(plot.title = element_text(size=20, hjust = .5))+
       xlab("Trials")+
       scale_y_continuous(expand=c(.1,.1),limits = c(min(datalong()[,3]),NA))+
       scale_x_continuous(breaks = seq(1,16,1))
   
 } else if (input$selectCombined == "Fitted Curves"){
   if(SumCol1() == 0 || SumCol2() == 0 || SumCol3() == 0){
    
      ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
       ggtitle("Please Completely Fill the Data Table")+
       theme(plot.title = element_text(size=20, hjust = .5))+
       xlab("Trials")+
       scale_y_continuous(expand=c(.1,.1),limits = c(min(datalong()[,3]),NA))+
       scale_x_continuous(breaks = seq(1,16,1))
   
     } else {
  
     ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
       ggtitle(paste(input$selected," Set: Power Curves"))+
       theme(plot.title = element_text(size=20, hjust = .5))+
       xlab("Trials")+
       scale_y_continuous(expand=c(.1,.1),limits = c(min(datalong()[,3]),NA))+
       scale_x_continuous(breaks = seq(1,16,1))+ 
       stat_smooth(method = 'nls',formula = 'y~a*x^b',method.args = list(start= c(a = 1,b=1)),se=FALSE,fullrange = TRUE,n=100)
   }
 } else {
   if(SumCol1() == 0 || SumCol2() == 0 || SumCol3() == 0){
     
     ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
       geom_point(aes(shape = Lego),color = as.character(input$selected))+
       geom_line()+
       ggtitle("Fill Data Table for Fitted Curves")+
       theme(plot.title = element_text(size=20, hjust = .5))+
       xlab("Trials")+
       scale_y_continuous(expand=c(.1,.1),limits = c(min(datalong()[,3]),NA))+
       scale_x_continuous(breaks = seq(1,16,1))+
       stat_smooth(method = 'nls',formula = 'y~a*x^b',method.args = list(start= c(a = 1,b=1)),se=FALSE,fullrange = TRUE,n=100)
     
   } else{

     ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
       geom_point(aes(shape = Lego),color = as.character(input$selected))+
       geom_line()+
       ggtitle(paste(input$selected," Set: Data and Power Curves"))+
       theme(plot.title = element_text(size=20, hjust = .5))+
       xlab("Trials")+
       scale_y_continuous(expand=c(.1,.1),limits = c(min(datalong()[,3]),NA))+
       scale_x_continuous(breaks = seq(1,16,1))+
       stat_smooth(method = 'nls',formula = 'y~a*x^b',method.args = list(start= c(a = 1,b=1)),se=FALSE,fullrange = TRUE,n=100)
   }
 }
})

 
 
 ##### Generate Report #####
 output$Report.html <- downloadHandler(
   # For PDF output, change this to "report.pdf"
   filename = "Report.html",
   content = function(file) {
     # Copy the report file to a temporary directory before processing it, in
     # case we don't have write permissions to the current working dir (which
     # can happen when deployed).
     tempReport <- file.path(tempdir(), "report.Rmd")
     file.copy("report.Rmd", tempReport, overwrite = TRUE)
     
     # Set up parameters to pass to Rmd document
     params <- list(n = isolate(input$selected),
                    Tbl = indat$data,
                    dtlong = datalong(),
                    SetTbl = LegoSetColor,
                    indColor = isolate(input$setselect),
                    StudentName = isolate(input$studname),
                    DF2 = DF2()
       )
     
     # Knit the document, passing in the `params` list, and eval it in a
     # child of the global environment (this isolates the code in the document
     # from the code in this app).
     rmarkdown::render(tempReport, output_file = file,
                       params = params,
                       envir = new.env(parent = globalenv())
     )
     })
 outputOptions(output, "select", suspendWhenHidden = FALSE)
 }

##### Run the application #####
shinyApp(ui = ui, server = server)

