#############
# Libraries #
#############
library(shiny)
require(shinyWidgets)

###############
# Initiate UI #
###############

ui = fluidPage(
  #########
  # Title #
  #########
  
  titlePanel(div(h1("Survival Model"))),
  
  sidebarLayout(
    #################
    # Sidebar panel #
    #################
    sidebarPanel(
      h4('Import Training Data'),
      # File input training
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Show number of input files
      h5('Number of Inputs:'),
      textOutput('trainRowCount'),
      hr(),
      
      h4('Import External Data'),
      # file input for external validation
      fileInput("file2", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Show number of input files
      h5('Number of Inputs:'),
      textOutput('extRowCount')),
    
    
    ##############
    # Main panel #
    ##############
    mainPanel(
      tabsetPanel(
        
        # view data - Panel
        tabPanel('Input file',
                 h3('Data Table'),
                 # select columns to remove
                 fluidRow(column(4,
                                 pickerInput("select", h4("Remove columns"), choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE)),
                          column(6,
                                 radioButtons('radioShowRawTable', 'Show Raw Data Table?', c('Yes', 'No'), selected = 'No'))),
                 # render data table
                 tableOutput("table"),
                 hr(),
                 # Render plot 1
                 plotOutput('pltMissing1'),
                 # Render plot 2
                 plotOutput('pltMissing2'),
                 # Download plots
                 downloadButton("download_Missing1", "Download plot1"),
                 downloadButton("download_Missing2", "Download plot2"),
                 hr(),
                 # rScript
                 h3('R Script: Data Import and Missing Data Plots'),
                 verbatimTextOutput('importData_print')),
        
        
        # Imputing data
        tabPanel('Imputing',
                 h3('Data Set Type'),
                 radioButtons("radioImpute", "Use Raw or Imputed Dataset",
                              c("Raw", "Imputed"), selected = 'Raw'),
                 hr(),
                 h3('Imputed Value Plots'),
                 plotOutput('plttrans'),
                 downloadButton("download_imputed", "Download plot"),
                 hr(),
                 h3('Imputation Assessment'),
                 h5('Determining reliability of imputed values by R2 value'),
                 verbatimTextOutput('transSummary_rsq'),
                 h5('Determining where imputed values are derived from using "xcoef"'),
                 verbatimTextOutput('transSummary_xcoef'),
                 hr(),
                 h3('Imputed Data Table'),
                 radioButtons('radioShowImputeTable', 'Show Imputed Data Table', c('Yes', 'No'), selected = 'No'),
                 downloadButton('download_newImputedTable'),
                 pickerInput("selectImpute", h4("Remove columns"), choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE),
                 tableOutput('newImputedDatatable'),
                 hr(),
                 # Imputed values
                 h3('Imputed columns'),
                 radioButtons('radioShowImputeVals', 'Show Imputed Data Table', c('Yes', 'No'), selected = 'No'),
                 downloadButton('download_impute'),
                 tableOutput('imputeVals'),
                 hr(),
                 # Describe data
                 h3('Describe Data'),
                 radioButtons('radioDescribe', 'Show Data Description', c('Yes', 'No'), selected = 'No'),
                 verbatimTextOutput('describedData'),
                 # print R script
                 hr(),
                 h3('R Script: imputing data'),
                 verbatimTextOutput('imputePrint')),
        
        tabPanel('Create Survival Object',
                 strong(textOutput('warning_output1'), style="font-size:150%;color:DarkSlateBlue"),
                 fluidRow(column(2,
                                 h5('Training model'))),
                 fluidRow(column(4,
                                 uiOutput('survDuration_input'),
                                 uiOutput('timeUnit_input')),
                          column(4,
                                 uiOutput("eventCol_input"),
                                 uiOutput("eventElement_input"))),
                 fluidRow(column(12,
                                 h5('Survival object formula'),
                                 h4(em(textOutput('survPrint'), style = "color:#001B85")))),
                 fluidRow(column(12,
                                 h5('Calculate Survival Object'))),
                 fluidRow(column(12,
                                 actionButton('calSurvObject', 'Calculate', class = 'btn btn-success'))),
                 br(),
                 fluidRow(column(12,
                                 verbatimTextOutput('S_output'))),
                 hr(),
                 h3('R script: Creating Survival object'),
                 verbatimTextOutput('RsurvPrint')),
        
        tabPanel('Lasso Regression',
                 h3('Lasso regression can be used to assist in selecting predictors when developing Cox Model'),
                 h5('Note that rows with event time 0 or lower have automatically been removed from lasso regression', style = "color:red"),
                 h3('Select Predictors'),
                 fluidRow(column(4,
                                 pickerInput('lassoPred', 'Select Numeric Predictors', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE),
                                 pickerInput('lassoCat', 'Select Categorical Predictors', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE),
                                 pickerInput('lassoLog', 'Log Predictors', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE)),
                          column(8,
                                 strong(textOutput('warning_output2'), style="font-size:150%;color:DarkSlateBlue"))),
                 h1(textOutput('warningPredLasso'), style = "color:red"),
                 fluidRow(column(4,
                                 actionButton('calcLasso', 'Perform Lasso Regression', class = 'btn btn-success')),
                          column(8,
                                 h4('Caution: Using Lasso regression can cause innaccurate confidence intervals in later models', style = "color:red"))),
                 plotOutput('plotLasso'),
                 verbatimTextOutput('printLassoFit'),
                 h4('Lasso calculated predictors and coefficients'),
                 uiOutput('lambda_input'),
                 verbatimTextOutput('pasteLassoCox'),
                 hr(),
                 h3('R Script for Lasso Regression'),
                 verbatimTextOutput('lasso_imputePrint')
        ),
        
        tabPanel('Create Full Cox Model',
                 strong(textOutput('warning_output3'), style="font-size:150%;color:DarkSlateBlue"),
                 # select predictors for model
                 h3('Creating Cox Model'),
                 h5('Estimated degrees of freedom available before overfitting is:'),
                 textOutput('predNum'),
                 hr(),
                 h3('Exploring Predictors'),
                 h4('Somers Rank'),
                 plotOutput('somerPred'),
                 h4('Redundancy Analysis'),
                 fluidRow(column(6, pickerInput('redunCoxSelectPred', 'Select Predictors for Redundancy analysis', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE)),
                          column(6, sliderInput('redunCoxR2', 'Redundancy R2 Value', min = 0, max = 1, step = 0.05, value = 0.7))),
                 h5('Given selected variables and R2 value, redundant variables are:'),
                 verbatimTextOutput('redunCox'),
                 hr(),
                 h3('Select Predictors for Model'),
                 fluidRow(column(6,
                                 pickerInput('selectPred', 'Select Numeric Predictors', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE),
                                 pickerInput('selectCat', 'Select Categorical Predictors', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE),
                                 pickerInput('selectOrdCat', 'Select Ordinal Categorical Predictors', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE),
                                 pickerInput('selectLog', 'Log Predictors', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE),
                                 pickerInput('selectCluster', 'Cluster Predictors', choices = c('No data entered'), multiple = TRUE)),
                          column(6,
                                 pickerInput('selectStrat', 'Stratified Predictors', choices = c('No data entered'), multiple = TRUE),
                                 pickerInput('selectRCS3', 'Restricted cubic splines = 3 Predictors', choices = c('No data entered'), multiple = TRUE),
                                 pickerInput('selectRCS4', 'Restricted cubic splines = 4 Predictors', choices = c('No data entered'), multiple = TRUE),
                                 pickerInput('selectRCS5', 'Restricted cubic splines = 5 Predictors', choices = c('No data entered'), multiple = TRUE))),
                 # print what model will calculate
                 h1(textOutput('warningPred'), style = "color:red"),
                 # Interactions
                 hr(),
                 h3('Interactions'),
                 fluidRow(column(6,
                                 pickerInput('mainPred', 'Main Predictor (Binary or continious varaiable)', choices = c('NULL'))),
                          column(6,
                                 pickerInput('intPred', 'Interacting Predictor (Continious variable)', choices = c('NULL')))),
                 fluidRow(column(12,
                                 hr(),
                                 h3('Cox Model Equation'),
                                 h4(em(textOutput('fullCoxPrint_text'), style = "color:#001B85")))),
                 hr(),
                 h3('Calculate Cox Model'),
                 # calculate cox model on click with user defined model parameters
                 fluidRow(column(4,
                                 pickerInput('coxElement', 'cox element', choices = c('Please Calculate Cox Model First'))),
                          column(8,
                                 actionButton('cal_coxModel', 'Calculate', class = "btn btn-success"),
                                 br(),br(),
                                 verbatimTextOutput('shrinkage'))),
                 fluidRow(column(12,
                                 verbatimTextOutput('fullCoxPrint2'),
                                 uiOutput('overPeramWarn'),
                                 uiOutput('overPeramVals'))),
                 hr(),
                 # visualise predicted variables
                 fluidRow(column(8,
                                 h3('Visualise predicted variables')),
                          column(4,
                                 actionButton('action_plotPredVar', 'Plot', class = "btn btn-success"),
                                 downloadButton('download_predictedVars', 'Download Plot'))),
                 plotOutput('plotPredVar'),
                 # Anova assessment
                 h3('Anova assessment'),
                 verbatimTextOutput('anova'),
                 hr(),
                 # Check Interactions by plotting
                 h3('Plotting Predictor Interactions'),
                 h5('Note this plot is NOT designed to exceed 3 knots (3 cubic splines)', style = 'color:#7F5217'),
                 actionButton('plotIntButton', 'Plot', class = "btn btn-success"),
                 sliderInput("minMax", "Select plot x-axis range (interacting variable)",
                             min = 0, max = 100, value = c(0,100)),
                 plotOutput('int_plot'),
                 # R script
                 h3('R Script for creating Cox model'),
                 verbatimTextOutput('RcoxPrint'),
                 h4('R Script: plotting interactions between variables'),
                 verbatimTextOutput('int_print')),
        
        tabPanel('Test Cox Assumptions',
                 h3('Examining Cox Proportionality'),
                 h5('The p-values can be used to determine whether a predictor is proportional. A p-values less than 0.05 is sometimes used to indicate non-proportionality'),
                 verbatimTextOutput('ph_text'),
                 pickerInput('selectPhPlot', 'Select Predictor to Examine Proportionality', choices = 'Please Create Cox Model'),
                 plotOutput('shoenPlot'),
                 hr(),
                 h3('Examine Influence of Individual Observations per Predictor'),
                 pickerInput('selectInfluencePred', 'Select Predictor to Examine Influence', choices = 'Please Create Cox Model'),
                 h4('Plot influence'),
                 plotOutput('influence_plot'),
                 h4('Overly Influential Observations'),
                 numericInput('inflCutoff', 'Influence Cut Off (0-1):', min = 0, max = 1, step = 0.05, value = 0.6),
                 tableOutput('overInfluenceTable'),
                 
                 hr(),
                 h3('R Script: Testing Proportionality'),
                 verbatimTextOutput('RcoxAssump')),
        
        tabPanel('Validate and Calibrate Model',
                 h3('Model Validation/Calibrating Settings'),
                 fluidRow(column(6,
                                 sliderInput('Validate_timeInc', 'Time Inclusive', value = 12, min = 0, max = 100)),
                          column(6,
                                 numericInput('Validate_bootstrap', 'Validate Bootstrap', value = 500, min = 500, max = 3000, step = 500))),
                 actionButton('action_validate', 'Validate/Calibrate Model', class = "btn btn-success"),
                 hr(),
                 h3('Validation using bootstrapping'),
                 textOutput('validate'),
                 hr(),
                 h3('Calibration using bootstrapping'),
                 plotOutput('cal_plot'),
                 hr(),
                 
                 h3('R Script'),
                 verbatimTextOutput('RcalVal')),
        
        tabPanel('Summarise Results',
                 h3('Summarising Cox Model - Predictor Hazard Ratios'),
                 actionButton('action_SumModel', 'Summarise Model', class = "btn btn-success"),
                 plotOutput('summariseModel'),
                 
                 h3('Nomogram'),
                 actionButton('action_SumModelNom', 'Produce Nomogram', class = "btn btn-success"),
                 fluidRow(column(6,
                                 sliderInput('survVar1', 'Survival at time:', value = 0, min = 0, max = 100)),
                          column(6,
                                 sliderInput('survVar2', 'Survival at time:', value = 0, min = 0, max = 100))),
                 fluidRow(column(6,
                                 sliderInput('survMedian', 'Median Survival at time range:', value = c(0,100), min = 0, max = 100)),
                          column(6,
                                 numericInput('survMedianStep', 'Steps in Median survival range every:', value = 2))),
                 plotOutput('summariseModel2'),
                 hr(),
                 
                 # Survival for imputed values
                 h3('Plotting Survival Estimates from Imputed values'),
                 h4('For numeric predictors, default values is given as the median'),
                 uiOutput("allInputs"),
                 actionButton('action_survEst', 'Plot survival estimates', class = "btn btn-success"),
                 h4('Inputted Values Table'),
                 tableOutput('survestTable'),
                 h4('Survival Estimate Plot'),
                 plotOutput('servEstimatePlot'),
                 hr(),
                 # Rscript
                 h3('R Script'),
                 verbatimTextOutput('RsummaryPrint')),
        
        
        tabPanel('Parsimony',
                 # Selecting predictors
                 h3('Somers Rank Correlation'),
                 plotOutput('plot_sDxy'),
                 hr(),
                 h3('Redundancy Analysis'),
                 sliderInput('redunR2', 'Select a Redundancy R2 Cut of Value', value = 0.6, min = 0, max = 1, step = 0.1),
                 verbatimTextOutput('redun_print'),
                 hr(),hr(),
                 # Creating Parsimonious model
                 h3('Select Predictors For New Reduced Model'),
                 fluidRow(column(6,
                                 pickerInput('selectPred_pars', 'Select Numeric and Categorical Predictors', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE),
                                 pickerInput('selectOrdCat_pars', 'Select Ordinal Categorical Predictors', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE),
                                 pickerInput('selectLog_pars', 'Log Predictors', choices = c('No data entered'), options = list(`actions-box` = TRUE), multiple = TRUE),
                                 pickerInput('selectCluster_pars', 'Cluster Predictors', choices = c('No data entered'), multiple = TRUE)),
                          column(6,
                                 pickerInput('selectStrat_pars', 'Stratified Predictors', choices = c('No data entered'), multiple = TRUE),
                                 pickerInput('selectRCS3_pars', 'Restricted cubic splines = 3 Predictors', choices = c('No data entered'), multiple = TRUE),
                                 pickerInput('selectRCS4_pars', 'Restricted cubic splines = 4 Predictors', choices = c('No data entered'), multiple = TRUE),
                                 pickerInput('selectRCS5_pars', 'Restricted cubic splines = 5 Predictors', choices = c('No data entered'), multiple = TRUE))),
                 h1(textOutput('warningPredPars'), style = "color:red"),
                 # print what model will calculate
                 h5('Full Model'),
                 h4(em(textOutput('oldModel'), style = "color:#001B85")),
                 h5('Reduced Model'),
                 h4(em(textOutput('redModel'), style = "color:#001B85")),
                 hr(),
                 # analysis of new model
                 actionButton('calPars', 'Compare Reduced Model', class = 'btn btn-success'),
                 h3('Anova Assessment of Reduced Model'),
                 verbatimTextOutput('anovRed'),
                 h3('Variation Between Full and Reduced Model'),
                 plotOutput('plotPars'),
                 h5('Quantifying Results'),
                 textOutput('Spearman'),
                 hr(),
                 # R script
                 h3('R Script'),
                 verbatimTextOutput('Rparsimony')),
        
        tabPanel('External Validatation and Calibration',
                 # Selecting predictors
                 h3('Add external dataset'),
                 
                 # List predictors in original cox model which are not present in external data set
                 h4('The following predictors from full model were not found in external validation dataset'),
                 h4(em(textOutput('excPred'), style = 'color:#7F5217')),
                 hr(),br(),
                 
                 h3('Create External Survival Object'),
                 fluidRow(column(4,
                                 uiOutput('extSurvDuration_input')),
                          column(4,
                                 uiOutput("extEventCol_input"),
                                 uiOutput("extEventElement_input"))),
                 actionButton('calExtSurv', 'Calculate', class = 'btn btn-success'),
                 br(),hr(),
                 h3('Survival object'),
                 verbatimTextOutput('ext_sObject_text'),
                 hr(),
                 
                 # Validation
                 h3('Validation'),
                 h4(em(textOutput('ext_con'), style = "color:#001B85")),
                 
                 # Explore linear model ouput
                 uiOutput('f.reduced_val_search'),
                 verbatimTextOutput('test'),
                 br(),hr(),
                 h3('External Calibrate'),
                 h5('Calibrate to external data'),
                 actionButton('calExCalibrate', 'calibrate', class = 'btn btn-success'),
                 br(),br(),
                 h3('Exploring External Calibration Model'),
                 uiOutput('f.tmp_search'),
                 verbatimTextOutput('testCalibrate'),
                 hr(),
                 h3('Model 1 Calibration: Calibration in the Large'),
                 textOutput('Model1Results'),
                 h3('Model 2 Calibration: Calibration Slope'),
                 textOutput('Model2Results'),
                 h3('Model 3 Calibration: Goodness of fit'),
                 h5('Risk groups are defined by deciles'),
                 fluidRow(column(6,
                                 numericInput('rgroupInput', 'Specify Number of Risk Groups', value = 5, step = 1)),
                          column(6,
                                 actionButton('calRiskGroup', 'Calculate Risk Groups', class = 'btn btn-success'))),
                 fluidRow(column(6,
                                 h3('Counts per Risks Group per Event Type ')),
                          column(6,
                                 h3('Goodness of Fit Table'))),
                 fluidRow(column(6,
                                 h5('Based on Global Goodness of Guidelines it is recommended 80% of groups should have 5+ events'))),
                 fluidRow(column(6,
                                 verbatimTextOutput('riskGroupTable')),
                          column(6,
                                 tableOutput('Model3Results'))),
                 hr(),
                 h3('External Calibration Data Table'),
                 h5(em('Methods used for external calibration are based on the paper: "Assessing calibration of prognostic risk scores" (CS Crowson et al., 2016) '), style= "color:#800000" ), 
                 radioButtons('radioShowExtCalTable', 'Show Data Table?', c('Yes', 'No'), selected = 'No'),
                 tableOutput('testnewdatatable'),
                 hr(),
                 # R script
                 h3('R Script'),
                 verbatimTextOutput('RextVal'),
                 h3('R Script: External Calibration'),
                 verbatimTextOutput('RextCal')),
      )
    )
  )
)
