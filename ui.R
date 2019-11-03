library(shiny)
library(shinydashboard)
library(rWordCloud)
library(htmlwidgets)
library(shinyjs)
library(rd3BarChart)
library(shinyBS)
library(rhandsontable)
library(DT)
library(rd3Cluster)

shinyUI(dashboardPage(
  
  skin = "green",
  dashboardHeader(title = "Text Analytics",
                  tags$li(class="dropdown messages-menu",
                          HTML('<a id="download_help" class="shiny-download-link" href="" 
                               target="_blank" download="" style="padding: 15px;float: left;">
                               <i class="fa fa-question" style="width:inherit;
                               font-size:inherit;margin-right:5px;margin-top:4px;"></i>help</a>')
                          ),
                  tags$li(
                    class="dropdown messages-menu",
                    HTML('<a href="#" class="dropdown-toggle" data-toggle="dropdown">
                         <i class="fa fa-cog" ></i>
                         Settings
                         </a>
                         <ul class="dropdown-menu">
                         <li>
                         <ul class="menu">
                         <li>
                         <a id="download_stpwds" class="shiny-download-link" href="" target="_blank" 
                         download><i class="fa fa-download" style="width:inherit;
                         font-size:inherit;margin-right:5px;margin-top:4px;">
                         </i>Stopwords</a></li>
                         <li>
                         <a id="download_Lexicon" class="shiny-download-link" href="" target="_blank" 
                         download><i class="fa fa-download" style="width:inherit;
                         font-size:inherit;margin-right:5px;margin-top:4px;">
                         </i>Lexicon</a></li>
                         <li>
                         <a id="download_emotions" class="shiny-download-link" href="" target="_blank" 
                         download><i class="fa fa-download" style="width:inherit;
                         font-size:inherit;margin-right:5px;margin-top:4px;">
                         </i>Emotions</a></li>
                         </ul>
                         </li>
                         </ul>')
                    )
                  
                  #,dropdownMenuOutput("notifyMenu"),dropdownMenuOutput("messages")
                    ),
  dashboardSidebar(
    #     sidebarSearchForm(
    #       textId = "searchText", buttonId = "searchBtn",
    #       label = "Search..."
    #       ),
    tags$head(
      tags$script('Shiny.addCustomMessageHandler("testmessage",
                  function(message) {
                  alert(JSON.stringify(message));
                  }
      );
                  '),
      tags$script(type = "text/javascript", src = "footer.js"),
      # tags$style(type="text/css",
      #            ".shiny-output-error { visibility: hidden; }",
      #            ".shiny-output-error:before { visibility: visible; content: 'An error occurred. Please contact the admin.'; }"
      # ),
      tags$link(rel = "stylesheet", type = "text/css", href = "jqueryui/1.10.4/jquery-ui.css"),
      tags$script(type = "text/javascript", src = "jqueryui/1.10.4/jquery-ui.js"),
      tags$script(type = "text/javascript", src="jquery.ui-contextmenu.min.js"),
      tags$script(type = "text/javascript", src="saveSvgAsPng.js")
      
    ),
    #                   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
    #
    #                            menuSubItem("Widgets", icon = icon("th"), tabName = "widgets"
    #                            )),
    fileInput(
      'file1', 'Choose CSV File',
      accept = c('text/csv',
                 'text/comma-separated-values,text/plain',
                 c('.csv','.xls,.xlsx'))
    ),
    tags$hr(),
    
    selectInput(
      'intCols',"No.of Cols",choices = NULL, multiple = F, selectize = T
    ),
    
    selectizeInput(
      'cols', label = NULL, multiple = TRUE, choices = NULL, options = list(placeholder = 'select column names')
    ),
    tags$br(),
    tags$hr(),
    fileInput(
      'swFile', 'Choose StopWords File',
      accept = c('text/plain',
                 '.txt') 
    ),
    conditionalPanel(condition = "input.tabBox1 == '1'",
                     #checkboxInput("rmNum", label = "RemoveNumbers", value = FALSE),
                     #checkboxInput("rmWrds", label = "RemoveWords", value = FALSE)
                     HTML('<div class="checkbox" style="margin-left:10px">
                          <label>
                          <input id="rmNum" type="checkbox"/>
                          <span>RemoveNumbers</span>
                          </label>
                          </div>
                          <div class="checkbox" style="margin-left:10px">
                          <label>
                          <input id="rmWrds" type="checkbox"/>
                          <span>RemoveWords</span>
                          </label>
                          </div>')
                     # HTML('<label for="">RemoveWords</label>')
                     #     #checkboxInput("rmWrds", label = NULL, value = FALSE),
                     #     #conditionalPanel(
                     #      # condition = "input.rmWrds == true",
                     # #       ,selectizeInput(
                     # #         'words', label = NULL, multiple = TRUE, choices = NULL, options = list(placeholder = 'select words'))
                     #       #)
                     ,HTML('<div id="rdrmWords" class="form-group shiny-input-radiogroup shiny-input-container">
                           
                           <div class="shiny-options-group">
                           <div class="radio">
                           <label>
                           <input type="radio" name="rdrmWords" value="All" checked="checked"/>
                           <span>All</span>
                           </label>
                           
                           <label>
                           <input type="radio" name="rdrmWords" value="n-gram Specific"/>
                           <span>n-gram Specific</span>
                           </label>
                           </div>
                           </div>
                           </div>
                           ')
                     , textInput(inputId='words',label = "",value="",  placeholder = 'select words'),
                     # selectizeInput('words', label = NULL, multiple = TRUE, choices = NULL, 
                     #                 options = list(placeholder = 'select words')),
                     
                     tags$br(),
                     tags$hr(),
                     #     radioButtons("tf", label = "TermFrequency",
                     #                  choices = list("TF" = "tf", "TF-IDF" = "tfidf"),selected = "tf"),
                     
                     
                     HTML('<div id="tf" class="form-group shiny-input-radiogroup shiny-input-container">
                          <label class="control-label" for="tf">TermFrequency</label>
                          <div class="shiny-options-group">
                          <div class="radio">
                          <label>
                          <input type="radio" name="tf" value="tf" checked="checked"/>
                          <span>TF</span>
                          </label>
                          
                          <label>
                          <input type="radio" name="tf" value="tfidf"/>
                          <span>TF-IDF</span>
                          </label>
                          </div>
                          </div>
                          </div>
                          ')),
    conditionalPanel(condition = "input.tabBox1 == '1' || input.tabBox1 == '3'",
                     sliderInput("ngram","N-Gram",min = 1,max = 4,value = 1)),
    #     radioButtons("rmWrds", label = "RemoveWords",
    #                  choices = list("None" = "none", "Words" = "words"),selected = "none"),
    
    
    # HTML('<div class=""> <div>
    #     <select id="words" class="form-control" multiple="multiple" ></select>
    #       <script type="application/json" data-for="words">{"placeholder":"select words"}</script>
    # 
    #       </div></div>'),
    # HTML('<div class="form-group shiny-input-container">
    #   <input id="txtFilter" type="text" class="form-control" placeholder="&#xF002;Search" 
    # style="font-family:Arial, FontAwesome" value=""/>
    #      </div> '),
    # HTML('
    #   <div class="input-group" style="
    #     margin: 10px;
    #      background-color: #374850;
    #      border-radius: 4px;
    #      border: 1px #374850 solid;">
    #     <input id="txtFilter" type="text" class="form-control" placeholder="Search..." style="
    #     background-color: #374850;
    #     border: 0px;color:#fff"/>
    #   <span class="input-group-btn">
    #   <button id="searchButton" type="button" class="btn btn-flat action-button" style="
    #     background-color: #374850;color:#999;">
    #   <i class="fa fa-search"></i>
    #   </button>
    #   </span>
    #   </div>'
    # ),
    conditionalPanel(condition = "input.tabBox1 == '1' ",
                     HTML('<button class="btn btn-primary action-button" id="submit" style="float:right;margin-right:20px;">Get WordCloud
                          </button> ')),
    conditionalPanel(
      condition = "input.tabBox1 == '2'",
      HTML('<button class="btn btn-primary action-button" id="btnGetSentiment" style="float:right;margin-right:20px;">Get Sentiment
           </button> ')),
    conditionalPanel(
      condition = "input.tabBox1 == '3'",
      sliderInput("freq_percent","Frequency %",min = 0,max = 100,value = 20,step = ,post = "%"),
      HTML('<button class="btn btn-primary action-button" id="getCluster" style="float:right;margin-right:20px;">Get Cluster
           </button> '))
    ####################################################################
    
    ,conditionalPanel(
      condition = "input.tabBox1 == '4'",
      HTML('<button class="btn btn-primary action-button" id="btnGetTags" style="float:right;margin-right:20px;">Get Tags
           </button> '))
    
    #######################################################################    
    #actionButton("submit", "Go")
    #,downloadButton('downloadData','Save my file!')
      ),
  dashboardBody(useShinyjs(),
                shinyjs::extendShinyjs(text = "shinyjs.errorPanel = function(parm){
                                       alert(parm);
                                       }"),
                # Boxes need to be put in a row (or column)
                tags$head(
                  HTML('<meta http-equiv="Access-Control-Allow-Origin" content="*"/>'),
                  
                  tags$style(
                    type = "text/css",".sidebar {
                    height: 90vh; overflow-y: auto;
                    }
                    .multi{float:left;}
                    .content-wrapper{
                    height: 90vh;overflow-y : auto;
                    }
                    svg text:hover{
                    font-family:FontAwesome !important;
                    
                    }
                    #tooltip {
                    position: absolute;
                    width: 200px;
                    height: auto;
                    padding: 10px;
                    background-color: white;
                    -webkit-border-radius: 10px;
                    -moz-border-radius: 10px;
                    border-radius: 10px;
                    -webkit-box-shadow: 4px 4px 10px rgba(0, 0, 0, 0.4);
                    -moz-box-shadow: 4px 4px 10px rgba(0, 0, 0, 0.4);
                    box-shadow: 4px 4px 10px rgba(0, 0, 0, 0.4);
                    pointer-events: none;
                    z-index : 1000;
                    }
                    
                    #tooltip.hidden {
                    display: none;
                    }
                    
                    #tooltip p {
                    margin: 0;
                    font-family: sans-serif;
                    font-size: 16px;
                    line-height: 20px;
                    }
                    #download-svg{
                    margin-bottom:5px;
                    }
                    /*#wcContainer{
                    text-align:center;
                    }*/
                    /* Only for the demo */
                    .hasmenu, .hasmenu2 {
                    /*border: 1px solid #008;*/
                    margin: 3px;
                    padding: 5px;
                    width: 30px;
                    }
                    
                    /* Optionally define a fixed width for menus */
                    .ui-menu {
                    width: 220px;
                    }
                    /* Allow to use <kbd> elements inside the title to define shortcut hints. */
                    .ui-menu kbd {
                    padding-left: 1em;
                    float: right;
                    }
                    
                    /* Define a custom icon */
                    .ui-icon.custom-icon-firefox {
                    background-image: url(application_firefox.gif);
                    background-position: 0 0;
                    }
                    table.dataTable {border-collapse : collapse ; 
                    background-color : #ccc;
                    color : #000}
                    table.dataTable td,
                    table.dataTable th {
                    position : relative;
                    
                    }
                    table.dataTable tbody tr:hover {
                    background-color: #ecf0f5 !important;/*#ffff99*/
                    }
                    /*table.dataTable tbody tr:nth-child(odd) {
                    
                    background-color: #fff;
                    }
                    .progress-message{
                    background-image : url('15-progress_bar.gif');
                    }*/
                    span.logo {
                    text-indent:25px;
                    background-color:transparent !important;
                    background: url(logo.png) 0 0 no-repeat;
                    background-position:5px center;
                    }
                    .skin-green .main-header .navbar .sidebar-toggle:hover:hover {
                    background-color:#003A60 !important;
                    }
                    .main-header {
                    
                    background:url(bg-header.png) repeat-x 0 0;
                    
                    } 
                    .navbar {
                    
                    background-color:transparent !important;
                    
                    }
                    .skin-green .left-side, .skin-green .main-sidebar, .skin-green .wrapper{
                    background-color:#003A60 !important;
                    }
                    #swFile_progress,#file1_progress{
                    height:auto;
                    }
                    .handsontable col{
                    width:200px !important;
                    }
                    .handsontable col.rowHeader{
                    width:50px !important;
                    }
                    .small-box .img-local {
                    position: absolute;
                    top: auto;
                    bottom: 5px;
                    right: 5px;
                    z-index: 0;
                    font-size: 70px;
                    color: rgba(0, 0, 0, 0.15);
                    }
                    .panel-group .panel-heading {
                    color: #fff !important;
                    background-color:#3c8dbc !important;
                    }
                    #textCluster{width:800px;}
                    table.dataTable.cell-border tbody td.distribution{
                    text-decoration:none;
                    color:blue;
                    cursor:pointer;
                    }
                    table.dataTable.cell-border tbody td.distribution:hover{
                    text-decoration:underline;
                    }
                    "
                  )# close tags$style
                  
                  ),
                # HTML('<button class="btn btn-primary action-button" id="remove" >Remove</button> '),
                #valueBoxOutput("progressBox"),
                tabBox(side = "left",width = "100%",height = "100%",id = "tabBox1",
                       tabPanel("Word Cloud",value=1,icon = shiny::icon("wordpress"),
                                HTML('<table id="tblWordcloud" style="height:100%;width:100%"><tr><td>'),
                                # HTML('
                                # <div id="accordion" class="col-lg-7">
                                #      <h3>WordCloud</h3><div style="padding:10px;height:auto;">'),
                                # 
                                # HTML('</div><h3>Section 2</h3>
                                #      <div>'),
                                #      #dataTableOutput("tab1"),
                                #      HTML('</div></div>')
                                HTML('<div class="panel-group" id="accordion1">
                                     <div class="panel panel-default">
                                     <div class="panel-heading">
                                     <h4 class="panel-title">
                                     <a class="accordion-toggle" data-toggle="collapse" data-parent="#accordion1" href="#collapseOne">
                                     Word Cloud 
                                     </a><i class="indicator glyphicon glyphicon-minus pull-right"></i>
                                     </h4>
                                     </div>
                                     <div id="collapseOne" class="panel-collapse collapse in">
                                     <div class="panel-body" style="margin-bottom:70px">'),
                                div(id="wcContainer",style="display:none",#actionButton('download-svg', label = "Download Wordcloud",icon = icon("download")),
                                    #sliderInput("wordLimit","Word Limit",min = 0,max = 100,value = 70,step = 1),
                                    #     HTML('<button class="btn btn-primary action-button" id="Map"  style="margin-right:20px;">Map
                                    #          </button> '),
                                    
                                    d3CloudOutput("plot",width="1000")),
                                bsModal("modalExample", "Data Table", "Map",
                                        footer = tags$button(class="btn btn-primary action-button", id="map","Map"),
                                        div(rHandsontableOutput(outputId = "grid"))
                                        
                                ),
                                #
                                
                                HTML('</div>
                                     </div>
                                     </div>
                                     <div class="panel panel-default">
                                     <div class="panel-heading">
                                     <h4 class="panel-title">
                                     <a class="accordion-toggle" data-toggle="collapse" data-parent="#accordion1" href="#collapseTwo">
                                     WordCloud for selected word: '),
                                #labelInput("lblSelWord"),
                                #textInput("txtSelWord",label = "",value = NULL),
                                textOutput("selWord",inline = T),
                                HTML('
                                     </a><i class="indicator glyphicon glyphicon-plus  pull-right"></i>
                                     </h4>
                                     </div>
                                     <div id="collapseTwo" class="panel-collapse collapse">
                                     <div class="panel-body">'),
                                #plotOutput("plot2"),
                                div(id="wc2Container",style="display:none;height:700px",
                                    #HTML('<ol class=\"breadcrumb\" style=\"margin-bottom: 5px;\"></ol>'), 
                                    #textOutput("errtext"),
                                    d3CloudOutput("plot2",width="800")),
                                HTML('</div>
                                     </div>
                                     </div>
                                     <div class="panel panel-default">
                                     <div class="panel-heading">
                                     <h4 class="panel-title">
                                     <a class="accordion-toggle" data-toggle="collapse" data-parent="#accordion1" href="#collapseThree">
                                     Data Table
                                     </a><i class="indicator glyphicon glyphicon-plus pull-right"></i>
                                     </h4>
                                     </div>
                                     <div id="collapseThree" class="panel-collapse collapse">
                                     <div class="panel-body">'),
                                #d3CloudOutput("plot2"),
                                div(id="corPlot",style="display:none",#downloadButton("downloadWA","Download"),
                                    plotOutput("plot1",width = 980,height=600),
                                    div(id="corPlot1",style="display:table;margin:0px auto;margin-bottom:20px",
                                        HTML('<button class="btn btn-primary action-button" id="getCor" 
                                             style="display:table-cell;position:relative;top:25px;
                                             margin-right:20px;">Plot
                                             </button> '),
                                        sliderInput("corLimit","Correlation Limit",min = 0,max = 1,value = 0.6,step = 0.01)
                                        )),
                                div(id="tbl",style="display:none",DT::dataTableOutput("tab1",width=980)),
                                HTML('</div>
                                     </div>
                                     </div>
                                     </div>')
                                #                            ,HTML('<div class="panel panel-default">
                                #                                 <div class="panel-heading">
                                #                                 <h4 class="panel-title">
                                #                                 <a class="accordion-toggle" data-toggle="collapse" data-parent="#accordion1" href="#collapseFour">
                                #                                 r-WordCloud
                                #                                 </a><i class="indicator glyphicon glyphicon-chevron-up pull-right"></i>
                                #                                 </h4>
                                #                                 </div>
                                #                                 <div id="collapseFour" class="panel-collapse collapse">
                                #                                 <div class="panel-body">'),
                                #                            #d3CloudOutput("plot2"),
                                #                            div(id="rWordCloudPlot",class="col-lg-12",style="",plotOutput("rWordCloudPlot",width = 1000,height = 1000)),
                                #                            HTML('</div>
                                # </div>
                                # </div>
                                # </div>')
                                ,HTML('<a id="link" href="#" download="image.png" style="display:none">download</a>
                                      <div id="dvWC" style="display:none"><img id="imgWC"  /></div>')
                                #d3TextCloudOutput("plot")
                                ,HTML('</td></tr></table>')
                                ),
                       tabPanel(title = "Sentiment Analysis",value = 2,icon = shiny::icon("smile-o"),
                                HTML('<table style="height:100%;width:100%">
                                     <tr><td>'),textOutput("print"),
                                # box(title = "Emotion Cards",solidHeader = T,status="primary",
                                # uiOutput("emotion"),
                                #  width=12,collapsible = T),
                                
                                box(title = "Sentiments",solidHeader = T,status="primary",
                                    div(id = "d3Bar",style="display:inline-table;",class="col-lg-12",
                                        # conditionalPanel(
                                        #   condition = "input.tabBox1 == 2",
                                        
                                        selectizeInput(
                                          'groupby', label = NULL, multiple = F, choices = NULL, options = list(placeholder = 'group by')
                                          ,width = 250
                                        ),
                                        d3BarChartOutput("barchart",width = "500",height = "600"), 
                                        d3CloudOutput("sentimentcloud",width="500", height = "400")
                                        #,hr()
                                    )
                                    ,width=12,collapsible = T)
                                #)
                                
                                ,
                                box(title = "Data Table",solidHeader = T,status="primary",
                                    #tableOutput("sentiment")
                                    #box(status="primary", width=12,
                                    #HTML('<div class="col-lg-12" style ="padding-top:50px;">'),#, style="overflow-x:auto;">'),
                                    DT::dataTableOutput("sentiment",width=980),#HTML('</div>'),
                                    width=12,collapsible = T)
                                #)
                                ,
                                HTML('</td></tr></table>')
                                ),
                       
                       tabPanel("Clustering Analysis",value=3,icon = shiny::icon("object-group"),
                                HTML('<table style="height:100%;width:100%"><tr><td>'),
                                radioButtons("rbcluster_method", "Method:",
                                             c("ward.D" = "ward.D", 
                                               "ward.D2" = "ward.D2",  
                                               "Single" = "single", 
                                               "Complete" = "complete", 
                                               "Average" = "ave"
                                             ),inline = T),
                                box(title = "Dendrogram",solidHeader = T,status="primary",plotOutput("cluster"),width = 12,collapsible = T),
                                box(title = "Cluster",solidHeader = T,status="primary",d3ClusterOutput("d3cluster"),width = 12,collapsible = T),
                                box(title = "MappedData",solidHeader = T,status="primary","Clustered Words",verbatimTextOutput("textCluster"),
                                    DT::dataTableOutput("MappedTable",width=980),width = 12,collapsible = T),
                                
                                
                                #box(title = "Data",solidHeader = T,status="primary",dataTableOutput("mytable"),width = 12,collapsible = T),
                                HTML("</td></tr></table>")
                       )
                       
                       ##############################################################################
                       
                       ,tabPanel(title = "Keyword Tagging",value = 4,icon = shiny::icon("tag"),
                                 HTML('<button class="btn btn-primary action-button" id="btnHidden" style="display:none;">hdbtn
                                      </button> <table style="height:100%;width:100%">
                                      <tr><td>'),#textOutput("print"),
                                 
                                 box(title = "Tagged Output",solidHeader = T,status="primary",
                                     DT::dataTableOutput("keyword",width=980),
                                     width=12,collapsible = T)
                                 ,
                                 
                                 #fluidRow(column(7,dataTableOutput('filterkeyword'))),
                                 box(title = "Selected Tagged Output",solidHeader = T,status="primary",
                                     HTML("<div style='width:100%'><div style='position: absolute;right: 50px;'>"),
                                     downloadButton('downloadTags', 'Download'), 
                                     downloadButton('downloadAllTags', 'Download All')
                                     ,HTML("</div><br><hr style='
                                           width: 95%;
                                           right: 10px;
                                           position: relative;
                                           '></div>"),
                                     DT::dataTableOutput("filterkeyword",width=980),
                                     width=12,collapsible = T
                                     #downloadButton('downloadTags', 'Download'), downloadButton('downloadAllTags', 'Download All'))
                                     ),
                                 
                                 HTML('</td></tr></table>')
                                 )
                       
                       ###################################################################### 
                       
                                 )
                ,HTML('<div id="tooltip" class="hidden">
                      <p><strong>Word:</strong><span id="value">100</span></p>
                      <p><strong>Freq:</strong><span id="freq">100</span></p>
                      </div>')
                ,

                footer =  HTML(
                  '
                  <footer class="footer" style="background: #e5e4e2;padding:5px 0;z-index:5px;">
                  <DIV id=footer class="container"><A title="Information Systems" class=logoIS href="#"></A>
                  <DIV id=copyright sizset="true" sizcache0104612681152913="3.0.0">Copyright Â©
                   </DIV></footer>
                  '
                )
                
                )
                ))
