#############
# Libraries #
#############

library(shiny)
require(tidyverse)
require(rms)
require(naniar)
library(glmnet) # lasso regression
require(shinyalert)
library(interactionRCS)



# Define server logic required to draw a histogram
function(input, output, session) {
  
  
  ####################
  # Import data file #
  ####################
  
  # training data 
  data_input = reactive({
    req(input$file1)
    # convert UI to CSV
    x = read_csv(input$file1$datapath)
    names(x) = str_replace_all(names(x), " ", "_")
    names(x) = str_replace_all(names(x), "-", "_")
    x
  })
  
  # count training rows
  output$trainRowCount = renderText({
    nrow(data_input())
  })
  
  # external Validation data
  extVal = reactive({
    req(input$file2)
    # convert UI to CSV
    x = read_csv(input$file2$datapath)
    names(x) = str_replace_all(names(x), " ", "_")
    x
  })
  
  # count training rows
  output$extRowCount = renderText({
    nrow(extVal())
  })
  
#   ##############
#   # Data table #
#   ##############
#   
#   # filtered table
#   data = reactive({
#     req(data_input())
#     # read in function for tidied table
#     
#     
#     data_input() %>%
#       select(-input$select)
#   })
#   
#   # call above variable
#   output$table <- renderTable({
#     req(data())
#     if (input$radioShowRawTable == 'Yes'){
#       # read in function for tidied table
#       data() 
#     } 
#   })
#   
#   # Dynamically identify column names from input file (used to choose cols to view in table)
#   #Select columns names
#   observeEvent(data_input(), {
#     updatePickerInput(session, "select", choices= as.character(colnames(data_input())))
#   })
#   
#   #####################
#   # plot missing data #
#   #####################
#   
#   # Create plot variable 1
#   Missing1 = reactive({
#     req(data_input())
#     # Create missing data object
#     na.patterns = naclus(select(data(), colnames(data())[apply(data(), 2, anyNA)]))
#     # set plot size
#     par(mar = c(5, 5, 5, 2))
#     par(cex=0.9)
#     # plot object
#     naplot(na.patterns, 'na per var')
#   })
#   
#   # plot 1
#   output$pltMissing1 = renderPlot({
#     if (is.null(data_input()))
#       return(NULL)
#     Missing1()
#   })
#   
#   # Create plot variable 1
#   Missing2 = reactive({
#     req(data_input())
#     # Create missing data object
#     na.patterns = naclus(select(data(), colnames(data())[apply(data(), 2, anyNA)]))
#     # set plot size
#     par(mar = c(5, 5, 5, 2))
#     par(cex=0.6)
#     # plot object
#     plot(na.patterns)
#   })
#   
#   # plot 2
#   output$pltMissing2 = renderPlot({
#     if (is.null(data_input()))
#       return(NULL)
#     Missing2()
#   })
#   
#   # Print import data and impute values
#   output$importData_print = renderPrint({
#     
#     excPreds = toString(input$select)
#     cat(sprintf(
#       '# Import required libaries
# require(tidyverse) # data wrangling
# require(rms) # Survival analysis
# require(naniar) # handling missing data
# require(glmnet) # lasso regression
# require(interactionRCS) # variable interactions
#     
# # Import data
# data = read_csv(data.csv)
# 
# # Remove white spaces and "-" from column names
# names(data) = str_replace_all(names(data), " ", "_")
# names(data) = str_replace_all(names(data), "-", "_")
# 
# 
# # Exclude predictors (if only single predictor add quotation marks)
# excPreds = %s
# 
# # Remove selected symbols columns from
# if (length(excPreds) > 0){
#   data = data %%>%%
#     select(-c(%s))
# }else{
#   data = data
# }
# 
# # Plot missing values
# na.patterns = naclus(select(data, colnames(data[apply(data, 2, anyNA)])))
# 
# # set plot size
# par(mar = c(5, 5, 5, 2))
# par(cex=0.9)
# 
# # plot object 1
# naplot(na.patterns, "na per var")
# 
# # plot object 2
# plot(na.patterns)
# 
# # set plot size to default
# dev.off()
# 
# ', paste(strsplit(excPreds, ', ')), excPreds))
#     
#     
#   })
#   
#   ############
#   # Transcan #
#   ############
#   
#   # variable
#   trans = reactive({
#     req(data_input())
#     if (input$radioImpute == 'Imputed'){
#       # get list of columns with missing data and convert to string with sep = '+'
#       w = paste(colnames(data()[apply(data(), 2, anyNA)]), collapse = '+')
#       # add string to transcan function (in string form)
#       w =  sprintf('transcan(~ %s,
#              imputed = TRUE, trantab=TRUE, data = data(), pl = FALSE, pr = FALSE)', w)
#       # convert string as variable
#       w = eval(parse(text=w))
#     } 
#   })
#   
#   # Text output Impute R2 value
#   output$transSummary_rsq = renderPrint({
#     if (input$radioImpute == 'Imputed'){
#       trans()$rsq
#     }
#   })
#   
#   # Text output Impute xCoef
#   output$transSummary_xcoef = renderPrint({
#     if (input$radioImpute == 'Imputed'){
#       trans()$xcoef
#     }
#   })
#   
#   # Plot missing and imputed values
#   output$plttrans = renderPlot({
#     if (input$radioImpute == 'Imputed'){
#       ggplot(trans())
#     }
#   })
#   
#   # Identify missing variables
#   observeEvent(data_input(), {
#     if (input$radioImpute == 'Imputed'){
#       updatePickerInput(session, "MissingVar", choices= as.character(colnames(data())[apply(data(), 2, anyNA)]))
#     }
#   })
#   
#   #########################
#   # Impute missing values #
#   #########################
#   
#   impute = reactive({
#     if (input$radioImpute == 'Imputed'){
#       # select columns that contain NAs and numeric
#       datax = data() %>%
#         select_if(~ any(is.na(.))) %>%
#         select_if(is.numeric)
#       
#       #impute missing values into columns with numeric values
#       for (i in 1:length(names(datax))){
#         missing_val = list(eval(parse(text=sprintf('trans()$imputed$%s', names(datax)[i]))))
#         missing_pos = list(eval(parse(text = sprintf('which(is.na(data()$%s))', names(datax)[i]))))
#         for (j in missing_pos){
#           datax[j,i] = missing_val
#         }
#       }
#       # select columns that contain NAs and are characters
#       datax2 = data() %>%
#         select_if(~ any(is.na(.))) %>%
#         select_if(is.character)
#       
#       # Impute missing values
#       for (i in 1:length(names(datax2))){
#         missing_val = list(eval(parse(text=sprintf('as.character(trans()$imputed$%s)', names(datax2)[i]))))
#         missing_pos = list(eval(parse(text = sprintf('which(is.na(data()$%s))', names(datax2)[i]))))
#         for (j in missing_pos){
#           datax2[j,i] = missing_val
#         }
#       }
#       
#       # combine both numeric and character imputed values
#       comImpute = cbind(datax2, datax)
#     }
#   })
#   
#   # render imputed columns only
#   output$imputeVals = renderTable({
#     if (input$radioImpute == 'Imputed' & input$radioShowImputeVals == 'Yes'){
#       impute()
#     }
#   })
#   
#   # Insert imputed values into data frame - variable
#   imputed_data = reactive({
#     req(data())
#     if (input$radioImpute == 'Imputed'){
#       # Remove all columns from dataframe with missing values
#       data_impute = data() %>%
#         select_if(~ !any(is.na(.)))
#       
#       # combine with imputed columns (previously calculated)
#       data_impute = cbind(data_impute,impute())
#       
#       # reorder columns to match original dataframe
#       data_impute %>%
#         select(names(data())) %>%
#         select(-input$selectImpute)
#     } else
#       data() 
#   })
#   
#   #render new data table with imputed values
#   output$newImputedDatatable = renderTable({
#     if (input$radioShowImputeTable == 'Yes'){
#       imputed_data()
#     }
#   })
#   
#   
#   # print variable for imputed data
#   imputePrint_var = reactive({
#     
#     selectImpute = toString(c(input$selectImpute))
#     
#     if (input$radioImpute == 'Imputed'){
#       w = paste(colnames(data()[apply(data(), 2, anyNA)]), collapse = "+")
#       selectImpute = toString(c(input$selectImpute))
#       cat(sprintf(
#         '
# #################
# # imputing data #
# #################
# 
# # Impute missing data using transcan
# trans =  transcan(~ %s,
#                   imputed = TRUE, trantab=TRUE, data = data, pl = FALSE, pr = FALSE)
#                   
# # Get imputed values R^2 and coefficient values
# trans$rsq
# trans$xcoef
# 
# # Plot missing values
# ggplot(trans)
# 
# # select columns that contain NAs and numeric
# datax = data %%>%%
#   select_if(~ any(is.na(.))) %%>%%
#   select_if(is.numeric)
# 
# #impute missing values into columns with numeric values
# for (i in 1:length(names(datax))){
#   missing_val = list(eval(parse(text=sprintf("trans$imputed$%%s", names(datax)[i]))))
#   missing_pos = list(eval(parse(text = sprintf("which(is.na(data$%%s))", names(datax)[i]))))
#   for (j in missing_pos){
#     datax[j,i] = missing_val
#   }
# }
# # select columns that contain NAs and are characters
# datax2 = data %%>%%
#   select_if(~ any(is.na(.))) %%>%%
#   select_if(is.character)
# 
# # Impute missing values
# for (i in 1:length(names(datax2))){
#   missing_val = list(eval(parse(text=sprintf("as.character(trans$imputed$%%s)", names(datax2)[i]))))
#   missing_pos = list(eval(parse(text = sprintf("which(is.na(data$%%s))", names(datax2)[i]))))
#   for (j in missing_pos){
#     datax2[j,i] = missing_val
#   }
# }
#   
# # combine both numeric and character imputed values
# comImpute = cbind(datax2, datax)
# 
# # Remove all columns from dataframe with missing values
# data_impute = data %%>%%
#   select_if(~ !any(is.na(.)))
# 
# # combine with imputed columns (previously calculated)
# data_impute = cbind(data_impute,comImpute)
# 
# # reorder columns to match original dataframe
# data_impute = data_impute %%>%%
#   select(names(data), -%s)
# 
# # Describe data
# imputed_data  = data_impute
# describe(imputed_data)', w, paste(strsplit(selectImpute, ', '))))
#     } else {
#       cat(sprintf(
#         '
# #################
# # imputing data #
# #################
# 
# # data
# imputed_data = data
#                 
# # Describe data
# describe(imputed_data)'))
#     }
#   })
#   
#   # print imputed data to UI
#   output$imputePrint = renderPrint({
#     imputePrint_var()
#   })
#   
#   #############################
#   # Describe new imputed data #
#   #############################
#   
#   # Create variable
#   describeData = reactive({
#     if(input$radioDescribe == 'Yes')
#       describe(imputed_data())
#   })
#   
#   # Print variable
#   output$describedData = renderPrint({
#     describeData()
#   })
#   
#   ##############################
#   # Developing Survival object #
#   ##############################
#   
#   # Create warnings describing which data type is being used imputed/raw
#   warnings_input = reactive({
#     req(imputed_data())
#     if (input$radioImpute == 'Raw'){
#       c('You are currently using the RAW data set to construct the survival object. If instead you would like to use the IMPUTED data, go to the "Imputed" tab and select IMPUTED',
#         'You are currently using the RAW data set to perform Lasso Regression. This can be sensitive to missing data and as such MISSING DATA HAS BEEN OMMITTED from the data set. If instead you would like to use the IMPUTED data, go to the "Imputed" tab and select Imputed',
#         'You are currently using the RAW data set to develop the Cox Model. If instead you would like to use IMPUTED data, go to the "Imputed" tab and select IMPUTED.')
#     } else {
#       c('You are currently using the IMPUTED data set to construct the survival object. If instead you would like to use the RAW data, go to the "Imputed" tab and select RAW dataset.',
#         'You are currently using the IMPUTED data set to perform Lasso Regression. If instead you would like to use the RAW data, go to the "Imputed" tab and select RAW dataset, although note missing data will be omitted as this can cause errors.',
#         'You are currently using the IMPUTED data set to develop the Cox Model. If instead you would like to use RAW data go to the "Imputed" tab and select IMPUTED')
#     }
#   })
#   
#   # notify user whether raw/imputed dataset being used
#   output$warning_output1 = renderText({
#     req(imputed_data())
#     print(warnings_input()[1])
#   })
#   
#   # select duration column for survival object
#   output$survDuration_input = renderUI({
#     req(data_input())
#     pickerInput("survDuration_output", "Select Event Time Column", names(imputed_data()))
#   })
#   
#   # select duration unit of time
#   output$timeUnit_input = renderUI({
#     req(data_input())
#     pickerInput('timeUnit_output', 'Select Unit of Time', choices = c('Seconds','Minutes', 'Hours', 'Days', 'Weeks', 'Months', 'Years', 'Decades'), selected = 'Days')
#   })
#   
#   # Dynamically identify columns in database for selecting survival event
#   output$eventCol_input <- renderUI({
#     req(data_input())
#     pickerInput("eventCol_output", "Select Event Column", names(imputed_data()))
#   })
#   
#   # Dynamically identify distinct elements in user specified column (above)
#   output$eventElement_input = renderUI({
#     req(data_input())
#     selectInput("eventElement_output", "Select Event(s) Type", 
#                 imputed_data()[,input$eventCol_output], multiple = TRUE)
#   })
#   
#   # Print surv object (variable)
#   survPrint_var = reactive({
#     req(input$eventElement_output)
#     x = c(input$eventElement_output)
#     x = paste(shQuote(x), collapse=", ")
#     sprintf('S = Surv(%s,%s %%in%% c(%s))',input$survDuration_output, input$eventCol_output, x)
#   })
#   
#   # Print survival object script
#   output$survPrint = renderText({
#     survPrint_var()
#   })
#   
#   # Create survival object
#   S = eventReactive(input$calSurvObject, {
#     req(input$eventElement_output)
#     attach(imputed_data())
#     # Need to create a global data dist object so later rms objects can access
#     #* Note shiny doesn't like global objects so to force it we use syntax '<<-'
#     dd <<- datadist(imputed_data()); options(datadist = 'dd') 
#     eval(parse(text = sprintf('units(%s) = input$timeUnit_output', input$survDuration_output)))
#     dd
#     # Survival object
#     x = c(input$eventElement_output)
#     x = paste(shQuote(x), collapse=", ")
#     S = sprintf('Surv(%s,%s %%in%% c(%s))',input$survDuration_output, input$eventCol_output, x)
#     S = eval(parse(text = S))
#     detach(imputed_data())
#     S
#   })
#   
#   # print survival object value
#   output$S_output = renderPrint({
#     req(S())
#     S()
#   })
#   
#   # Print R script
#   output$RsurvPrint = renderPrint({
#     req(imputed_data())
#     cat(sprintf(
#       '
# ##########################
# # create survival object #
# ##########################
# 
# # attach data to global environment
# attach(imputed_data)
# 
# # Create datadist object
# dd = datadist(imputed_data); options(datadist = "dd")
# units(%s) = "%s"
# 
# # Survival object
# %s', input$survDuration_output, input$timeUnit_output, toString(survPrint_var())))
#   })
#   
#   ####################
#   # Lasso Regression #
#   ####################
#   
#   # notify user whether raw/imputed dataset being used
#   output$warning_output2 = renderText({
#     req(imputed_data())
#     print(warnings_input()[2])
#   })
#   
#   # lasso imputed data - automatically remove rows where event time is 0
#   lasso_impute = reactive({
#     req(imputed_data())
#     imputed_data = na.omit(imputed_data())
#     eval(parse(text = sprintf('imputed_data %%>%% filter(%s > 0)', input$survDuration_output)))
#   })
#   
#   
#   # Create survival object
#   S_lasso = eventReactive(input$calSurvObject, {
#     req(input$eventElement_output)
#     attach(lasso_impute())
#     # Need to create a global data dist object so later rms objects can access
#     #* Note shiny doesn't like global objects so to force it we use syntax '<<-'
#     dd <<- datadist(lasso_impute()); options(datadist = 'dd') 
#     eval(parse(text = sprintf('units(%s) = input$timeUnit_output', input$survDuration_output)))
#     dd
#     # Survival object
#     x = c(input$eventElement_output)
#     x = paste(shQuote(x), collapse=", ")
#     S = sprintf('Surv(%s,%s %%in%% c(%s))',input$survDuration_output, input$eventCol_output, x)
#     S = eval(parse(text = S))
#     detach(lasso_impute())
#     S
#   })
#   
#   # print lasso impute
#   output$lasso_imputePrint = renderPrint({
#     x = c(input$eventElement_output)
#     x = paste(shQuote(x), collapse=", ")
#     lassopreds = toString(c(input$lassoPred, lassoPredCat(), lassoPredLog()))
#     
#     cat(sprintf(
#       '
# ####################
# # Lasso Regression #
# ####################
# 
# # Manually remove rows where time = 0  
#  lasso_impute = na.omit(imputed_data) %%>%% filter(%s > 0)
# 
# # Create datadist object 
# dd = datadist(lasso_impute); options(datadist = "dd")
# 
# # Create Survival object
# S_lasso = Surv(%s,%s %%in%% c(%s))
# 
# # Predictors selected for lasso
# lassoPred_comp = %s
# 
# # Perform lasso regression
# set.seed(1)
# df = lasso_impute %%>%% select(matches(lassoPred_comp))
# lassoFit = cv.glmnet(as.matrix(df), as.matrix(S_lasso), family = "cox", type.measure = "C")
# plot(lassoFit)
# 
# # Print coefficient values
# lassoFit
# 
# # fit model using lasso regression calculated as lambda min or lambda to 1 SE
# df2 = lasso_impute %%>%% select(matches(lassoPred_comp))
# f.glmnet = glmnet(df2, S_lasso, family = "cox", lambda = %s )
# 
# # get coefficients of optimised model
# Lassocoef = coef(f.glmnet, s = %s)
# Lassocoef = as.matrix(Lassocoef)
# Lassocoef = tibble(predictor = row.names(Lassocoef), Lassocoef)
# colnames(Lassocoef) = c("predictor", "coef")
# 
# # Show only included predictors
# Lassocoef %%>%%
#   filter(coef != 0)',input$survDuration_output, input$survDuration_output,
#       input$eventCol_output, x, paste(strsplit(lassopreds, ', ')),
#       input$lambda_output, input$lambda_output))
#   })
#   
#   
#   
#   
#   # Update drop down menus in lasso regression
#   observeEvent(S_lasso(),{
#     pred = lasso_impute() %>% select(where(is.numeric))
#     pred = names(pred) %>% discard(names(pred) %in% c(input$survDuration_output, input$eventCol_output))
#     predCat = lasso_impute() %>% select(where(is.character))
#     predCat = names(predCat) %>% discard(names(predCat) %in% c(input$survDuration_output, input$eventCol_output))
#     updatePickerInput(session, "lassoPred", choices= pred)
#     updatePickerInput(session, "lassoCat", choices= predCat)
#     updatePickerInput(session, "lassoLog", choices= pred)
#   })
#   
#   # Loop Categorical predictors
#   lassoPredCat = reactive({
#     pCat = c()
#     for (i in input$LassoCat){
#       pCat = append(pCat, sprintf('as.factor(%s)', i))
#     }
#     pCat
#   })
#   
#   # Loop log predictors
#   lassoPredLog = reactive({
#     logx = c()
#     for (i in input$lassoLog){
#       logx = append(logx, sprintf('log(%s + 0.0000000001)', i))
#     }
#     logx
#   })
#   
#   # Combine all predictors for lasso regression
#   LassoPred_comp = reactive({
#     req(lasso_impute())
#     c(input$lassoPred, lassoPredCat(), lassoPredLog())
#   })
#   
#   # Combine all predictors for warning
#   LassoPred_comp2 = reactive({
#     req(lasso_impute())
#     c(input$lassoPred, input$lassoCat, input$lassoLog)
#   })
#   
#   
#   # warning if  predictors are selected in multiple categories
#   output$warningPredLasso = renderText({
#     req(LassoPred_comp2)
#     if (length(LassoPred_comp2()[duplicated(LassoPred_comp2())]) > 0){
#       duplicated = paste(LassoPred_comp2()[duplicated(LassoPred_comp2())], collapse = ', ')
#       sprintf('Warning the following predictors are found in more than one category: %s. This will cause issues when developing the Model', duplicated)
#     }
#   })
#   
#   #Plot coefficient solutions with default settings/weightings
#   lassoFit = eventReactive(input$calcLasso, {
#     req(LassoPred_comp())
#     
#     set.seed(1)
#     df = lasso_impute() %>% select(matches(LassoPred_comp()))
#     cv.glmnet(as.matrix(df), as.matrix(S_lasso()), family = "cox", type.measure = "C")
#   })
#   
#   # plot lasso regression
#   output$plotLasso = renderPlot({
#     req(lassoFit())
#     plot(lassoFit())
#   })
#   
#   # print lasso lambda values
#   output$printLassoFit = renderPrint({
#     req(lassoFit())
#     lassoFit()
#   })
#   
#   output$lambda_input = renderUI({
#     req(lassoFit())
#     radioButtons("lambda_output", "Select lambda", c('Lamda at highest predictive value' = lassoFit()$lambda.min,
#                                                      'Lamda where model is most regularised within 1 standard deviation' = lassoFit()$lambda.1se))
#   })
#   
#   # fit cox model with selected lasso regression predictor cut off
#   lassoCox = reactive({
#     req(input$lambda_output)
#     # fit model using lasso regression calculated lambda to 1se
#     df = lasso_impute() %>% select(matches(LassoPred_comp()))
#     f.glmnet = glmnet(df, S_lasso(), family = 'cox', lambda = input$lambda_output) 
#     
#     # get coefficients of optimised model
#     coef(f.glmnet, s = input$lambda_output)
#   })
#   
#   # paste lasso predictors and cox coefficents
#   output$pasteLassoCox = renderPrint({
#     req(lassoCox())
#     lassoCox()
#     
#   })
#   
#   ######################
#   # Creating Cox Model #
#   ######################
#   
#   # notify user whether raw/imputed dataset being used
#   output$warning_output3 = renderText({
#     req(imputed_data())
#     print(warnings_input()[3])
#   })
#   
#   ### Estimate number of available predictors for model ###
#   output$predNum = renderText({
#     req(S())
#     x = c(input$eventElement_output)
#     x = paste(shQuote(x), collapse=", ")
#     events = with(imputed_data(), eval(parse(text =sprintf('sum(%s %%in%% c(%s))', input$eventCol_output, x))))
#     events/15
#   })
#   
#   ##### Create Somers rank
#   # Get predictors for somers
#   somerPredList = reactive({
#     req(S())
#     predsNum = c()
#     predsChar = c()
#     
#     # loop through numeric and character columns
#     for(i in names(imputed_data() %>% select_if(is.numeric))){
#       predsNum = append(predsNum, sprintf('`%s`', i))
#     }
#     for(i in names(imputed_data() %>% select_if(is.character))){
#       predsChar = append(predsChar, sprintf('factor(`%s`)', i))
#     }
#     # Combine and create list
#     predsAll = c(predsChar, predsNum)
#     paste(predsAll, collapse = ' + ')
#   })
#   
#   #perform Somers
#   predsSomer = reactive({
#     req(somerPredList())
#     eval(parse(text = sprintf('with(imputed_data(), rcorrcens(S() ~ %s))', somerPredList())))
#   })
#   
#   # Render Somers Rank plot
#   output$somerPred = renderPlot({
#     req(predsSomer())
#     plot(predsSomer())
#   })
#   
#   #### Creating Cox Model Predictors ####
#   
#   # Create list of predictors
#   observeEvent(S() , {
#     pred = imputed_data() %>% select(where(is.numeric))
#     pred = names(pred) %>% discard(names(pred) %in% c(input$survDuration_output, input$eventCol_output))
#     predCat = imputed_data() %>% select(where(is.character))
#     predCat = names(predCat) %>% discard(names(predCat) %in% c(input$survDuration_output, input$eventCol_output))
#     predall = names(imputed_data()) %>% discard(names(imputed_data()) %in% c(input$survDuration_output, input$eventCol_output))
#     updatePickerInput(session, "selectPred", choices= pred)
#     updatePickerInput(session, "selectCat", choices= predCat)
#     updatePickerInput(session, 'selectOrdCat', choices = pred)
#     updatePickerInput(session, "selectLog", choices= pred) 
#     updatePickerInput(session, "selectCluster", choices= predall) 
#     updatePickerInput(session, "selectStrat", choices= predall) 
#     updatePickerInput(session, "selectRCS3", choices= pred) 
#     updatePickerInput(session, "selectRCS4", choices= pred) 
#     updatePickerInput(session, "selectRCS5", choices= pred) 
#   })
#   
#   # Loop Categorical predictors - survival package
#   predCat = reactive({
#     pCat = c()
#     for (i in input$selectCat){
#       pCat = append(pCat, sprintf('as.factor(%s)', i))
#     }
#     pCat
#   })
#   
#   # Loop Categorical predictors - rms package
#   predCat2 = reactive({
#     pCat = c()
#     for (i in input$selectCat){
#       pCat = append(pCat, sprintf('catg(%s)', i))
#     }
#     pCat
#   })
#   
#   # Loop Ordinal categorical predictors - survival package
#   predOrdCat = reactive({
#     pOrCat = c()
#     for (i in input$selectOrdCat){
#       pOrCat = append(pOrCat, sprintf('factor(%s)', i))
#     }
#     pOrCat
#   })
#   
#   # Loop Ordinal Categorical predictors - rms package
#   predOrdCat2 = reactive({
#     pOrCat = c()
#     for (i in input$selectOrdCat){
#       pOrCat = append(pOrCat, sprintf('scored(%s)', i))
#     }
#     pOrCat
#   })
#   
#   # Loop log predictors
#   predLog = reactive({
#     logx = c()
#     for (i in input$selectLog){
#       logx = append(logx, sprintf('log(%s + 0.0000000001)', i))
#     }
#     logx
#   })
#   
#   # Loop Cluster predictors
#   predCluster = reactive({
#     cluster = c()
#     for (i in input$selectCluster){
#       cluster = append(cluster, sprintf('cluster(%s)', i))
#     }
#     cluster
#   })
#   
#   # Loop Stratified predictors
#   predStrat = reactive({
#     stratx = c()
#     for (i in input$selectStrat){
#       stratx = append(stratx, sprintf('strat(%s)', i))
#     }
#     stratx
#   })
#   
#   # Loop RCS3 predictors
#   predRCS3 = reactive({
#     RCS3 = c()
#     for (i in input$selectRCS3){
#       RCS3 = append(RCS3, sprintf('rcs(%s, 3)', i))
#     }
#     RCS3
#   })
#   
#   # Loop RCS4 predictors
#   predRCS4 = reactive({
#     RCS4 = c()
#     for (i in input$selectRCS4){
#       RCS4 = append(RCS4, sprintf('rcs(%s, 4)', i))
#     }
#     RCS4
#   })
#   
#   # Loop RCS5 predictors
#   predRCS5 = reactive({
#     RCS5 = c()
#     for (i in input$selectRCS5){
#       RCS5 = append(RCS5, sprintf('rcs(%s, 5)', i))
#     }
#     RCS5
#   })
#   
#   # Update interaction select columns
#   observeEvent(fullCoxPred(), {
#     updatePickerInput(session, "mainPred", choices = c('NULL', coxPred()))
#     updatePickerInput(session, "intPred", choices = c('NULL', coxPred()))
#   })
#   
#   # Interaction
#   IntPred = reactive({
#     req(imputed_data())
#     if(input$mainPred != 'NULL' & input$intPred != 'NULL'){
#       sprintf('%s*%s', input$mainPred, input$intPred)
#     }
#   })
#   
#   # Create object for all predictors used in full model 
#   fullCoxPred = reactive ({
#     # Identify all inputs for cox model
#     pred = c(input$selectPred)
#     pCat = c(input$selectCat)
#     pOrCat = c(input$selectOrdCat)
#     RCS3 = c(input$selectRCS3)
#     RCS4 = c(input$selectRCS4)
#     RCS5 = c(input$selectRCS5)
#     Log = c(input$selectLog)
#     Clust = c(input$selectCluster)
#     Strat = c(input$selectStrat)
#     # combine into list
#     c(pred, pCat, pOrCat, Log, RCS3, RCS4, RCS5, Clust, Strat)
#   })
#   
#   # Warning message if a predictor is selected in more than one category
#   output$warningPred = renderText({
#     req(fullCoxPred())
#     if (length(fullCoxPred()[duplicated(fullCoxPred())]) > 0){
#       duplicated = paste(fullCoxPred()[duplicated(fullCoxPred())], collapse = ', ')
#       sprintf('Warning the following predictors are found in more than one category: %s. This will cause issues when developing the Model', duplicated)
#     }
#   })
#   
#   # Create object listing all predictors excluding interaction
#   coxPred = reactive({
#     req(S())
#     pred = c(input$selectPred)
#     pCat = c(input$selectCat)
#     c(pred, predCat2(), predOrdCat2(), predLog(), predCluster(), predStrat(), predRCS3(), predRCS4(), predRCS5())
#   })
#   
#   # Create object for all predictors and Parameters compatible of Somers analysis 
#   coxPred_comp = reactive({
#     req(imputed_data())
#     pred = c(input$selectPred)
#     RCS3 = c(input$selectRCS3)
#     RCS4 = c(input$selectRCS4)
#     RCS5 = c(input$selectRCS5)
#     paste(c(pred, predCat(), predOrdCat(), predLog(), predCluster(), predStrat(), RCS3, RCS4, RCS5), collapse=" + ")
#   })
#   
#   # Create object for all predictors and Parameters compatible of redundancy analysis 
#   coxPred_redun = reactive({
#     req(imputed_data())
#     pred = c(input$selectPred)
#     RCS3 = c(input$selectRCS3)
#     RCS4 = c(input$selectRCS4)
#     RCS5 = c(input$selectRCS5)
#     paste(c(pred, predCat(), predOrdCat(), predLog(), predStrat(), RCS3, RCS4, RCS5), collapse=" + ")
#   })
#   
#   ####### Print cox model variable ######
#   
#   # list cox model predictors
#   fullCoxPrint_var = reactive({
#     req(imputed_data())
#     pred = c(input$selectPred)
#     pCat = c(input$selectCat)
#     paste(c(pred, predCat2(), predOrdCat2(), predLog(), predCluster(), predStrat(), predRCS3(),
#             predRCS4(), predRCS5(), IntPred()), collapse=" + ")
#     
#   })
#   
#   # Print Cox Script
#   output$fullCoxPrint_text = renderText({
#     req(fullCoxPrint_var())
#     sprintf('cox = cph(S ~ %s,  x = TRUE, y = TRUE, surv = TRUE)', toString(fullCoxPrint_var()))
#   })
#   
#   ######## Calculate Cox Model ######
#   
#   # identify elements within cox model object
#   observeEvent(cox(), {
#     updatePickerInput(session, "coxElement", choices= names(cox()))
#   })
#   
#   # Create dynamic Cox model variable - for rms package
#   cox_initiate = eventReactive(input$cal_coxModel, {
#     req(S())
#     req(imputed_data())
#     req(any(is.na(imputed_data() %>% select(input$selectCat))) != TRUE)
#     pred = c(input$selectPred)
#     pCat = c(input$selectCat)
#     pred = paste(c(pred, predCat2(), predOrdCat2(), predLog(), predCluster(), predStrat(), predRCS3(),
#                    predRCS4(), predRCS5(), IntPred()), collapse=" + ")
#     fullCox = sprintf('cph(S() ~ %s,  x = TRUE, y = TRUE, surv = TRUE, data = imputed_data(), iter.max = 100)', pred)
#     
#     eval(parse(text = fullCox))
#   })
#   
#   # Run rms cox model if hasnt failed
#   cox = reactive({
#     req(cox_initiate()[["fail"]] == FALSE)
#     cox_initiate()
#   })
#   
#   # Create warning message for failed cox
#   observeEvent(input$cal_coxModel, {
#     if(cox_initiate()[["fail"]] == TRUE){
#       shinyalert("Oops!", "Something went wrong\nThis is likely caused by a collinearity of predictors, where one predictor can perfectly predict another after rescaling.\n\nTo resolve this issue select a different arrangement of predictors and try again", type = "error")
#     }
#   })
#   # Create warning message for failed cox2
#   observeEvent(input$cal_coxModel, {
#     if (any(is.na(imputed_data() %>% select(c(input$selectCat)))) == TRUE){
#       shinyalert("Oops!", "Something went wrong\nThis has likely caused by missing values in categorical variables.\n\nTo resolve this either\n1) Impute missing values by selecting 'Imputed' in the Imputed tab \n2) Remove rows with missing data and try again\n3) Remove predictor from model", type = "error")
#     }
#   })
#   
#   # Create dynamic Cox model variable - using survival package
#   cox_Surv = eventReactive(input$cal_coxModel, {
#     req(cox())
#     x = c(input$eventElement_output)
#     x = paste(shQuote(x), collapse=", ")
#     pred = c(input$selectPred)
#     pCat = c(input$selectCat)
#     pred = paste(c(pred, predCat(), predOrdCat(), predLog(), predCluster(), predStrat(), predRCS3(),
#                    predRCS4(), predRCS5(), IntPred()), collapse=" + ")
#     pred = paste(pred, collapse=" + ")
#     fullCox = sprintf('coxph(Surv(%s,%s %%in%% c(%s)) ~ %s,  model=TRUE, data = imputed_data())', input$survDuration_output, input$eventCol_output, x, pred)
#     eval(parse(text = fullCox))
#   })
#   
#   # Print results from dynamic cox model
#   output$fullCoxPrint2 = renderPrint({
#     req(cox())
#     print(cox()[[input$coxElement]])
#   })
#   
#   #### Assess Model ####
#   
#   # Estimate over fitting
#   output$shrinkage = renderPrint({
#     req(cox())
#     df = cox()$stats[4] #degree of freedom
#     LR = cox()$stats[3] #liklihood ratio
#     shrinkage = (LR-df)/LR
#     (1-shrinkage)*100 # estimated percentage of fitted noise
#     cat(sprintf("This model is fitting ~%1.0f%% noise.\nFor a respectible predictive power this should ideally be less than 10%%", (1-shrinkage)*100))
#   })
#   
#   # anova
#   output$anova = renderPrint({
#     req(cox())
#     anova(cox())
#   })
#   
#   # Create variable for Plotting predicted variables
#   pred_var = eventReactive(input$action_plotPredVar, {
#     req(cox())
#     ggplot(Predict(cox()), sepdiscrete='vertical', nlevels=4, vnames='names')
#   })
#   
#   # Plot predicted variables
#   output$plotPredVar = renderPlot({
#     req(pred_var())
#     pred_var()
#   })
#   
#   # Create variable for plotting interactions
#   int_var = eventReactive(input$plotIntButton, {
#     min = min(imputed_data()[, names(imputed_data())[which(str_detect(input$intPred, names(imputed_data())) == TRUE)]])
#     max = max(imputed_data()[, names(imputed_data())[which(str_detect(input$intPred, names(imputed_data())) == TRUE)]])
#     MainPred = names(imputed_data())[which(str_detect(input$mainPred, names(imputed_data())) == TRUE)]
#     IntPred = names(imputed_data())[which(str_detect(input$intPred, names(imputed_data())) == TRUE)]
#     
#     intEST(var2values = c(input$minMax[1]:input$minMax[2]),
#            model = cox(), data = imputed_data(), var1 = MainPred, var2= IntPred,
#            ci.method = "delta")
#   })
#   
#   # update x-axis range for interaction plot
#   observeEvent(input$intPred, {
#     minRange = min(imputed_data()[, names(imputed_data())[which(str_detect(input$intPred, names(imputed_data())) == TRUE)]])
#     maxRange = max(imputed_data()[, names(imputed_data())[which(str_detect(input$intPred, names(imputed_data())) == TRUE)]])
#     updateSliderInput(session, 'minMax', min = minRange, max = maxRange, value = c(minRange,maxRange))
#   })
#   
#   # Plot interaction
#   output$int_plot = renderPlot({
#     req(cox())
#     plotINT(int_var(), xlab = input$intPred)
#   })
#   
#   # R script for creating cox model
#   output$RcoxPrint = renderPrint({
#     req(S())
#     x = c(input$eventElement_output)
#     x = paste(shQuote(x), collapse=", ")
#     cat(sprintf(
#       '######################
# # Creating Cox Model #
# ######################
# 
# # Reapply datadist object for imputed values incase removed by lasso regression
# dd = datadist(imputed_data); options(datadist = "dd")
# units(%s) = "%s"
# 
# #Estimate number of degrees of freedom before overfitting 
# events = sum(%s %%in%% c(%s))/15
# 
# # Plot Somers Rank to determine indvidual predictors association to event
# sDxy = rcorrcens(S ~ %s)
# plot(sDxy)
# 
# # Cox model using rms package
# cox = cph(S ~ %s,  x = TRUE, y = TRUE, surv = TRUE)
# 
# # Cox model using survival package
# cox2 = coxph(Surv(%s,%s %%in%% c(%s)) ~ %s,  model=TRUE, data = imputed_data)
# 
# # anova assessment of cox model
# anova(cox)
# 
# # plot predicted variables
# ggplot(Predict(cox), sepdiscrete="vertical", nlevels=4, vnames="names")
# 
# ## Estimate overfitting ##
# df = cox$stats[4] #degree of freedom
# LR = cox$stats[3] #liklihood ratio
# shrinkage = (LR-df)/LR
# noise = (1-shrinkage)*100 # estimated percentage of fitted noise',
#       input$survDuration_output, input$timeUnit_output, input$eventCol_output, x,
#       somerPredList(), fullCoxPrint_var(), input$survDuration_output, input$eventCol_output,
#       x, fullCoxPrint_var()))
#   })
#   
#   # R script for interaction plot
#   output$int_print = renderPrint({
#     req(int_var())
#     min = min(imputed_data()[, names(imputed_data())[which(str_detect(input$intPred, names(imputed_data())) == TRUE)]])
#     max = max(imputed_data()[, names(imputed_data())[which(str_detect(input$intPred, names(imputed_data())) == TRUE)]])
#     MainPred = names(imputed_data())[which(str_detect(input$mainPred, names(imputed_data())) == TRUE)]
#     IntPred = names(imputed_data())[which(str_detect(input$intPred, names(imputed_data())) == TRUE)]
#     cat(sprintf(
#       '# Plot interaction between selected variables with the "interactionRCS" package using delta method
# HR_rcs_delta = intEST(var2values = c(%s:%s), model = cox, data = imputed_data, var1 = "%s", var2= "%s", ci.method = "delta")
# plotINT(HR_rcs_delta, xlab = %s)',
#       input$minMax[1], input$minMax[2], MainPred, IntPred, input$intPred))
#   })
#   
#   ###########################
#   # Check Model Assumptions #
#   ###########################
#   
#   # Create predicted variables - Used for Shoenfelds residuals
#   f.short = reactive({
#     req(cox())
#     z = predict(cox(), type = 'terms')
#     f.short = cph(S() ~ z, x = TRUE, y= TRUE)
#     f.short
#   })
#   
#   # Create variable for shoenfields residuals - Test proportionality
#   phtest = reactive({
#     req(f.short())
#     phtest_var = cox.zph(f.short(), transform = 'identity', terms = FALSE)
#     phtest_var
#   })
#   
#   # Print predicted Coef
#   output$ph_text = renderPrint({
#     req(phtest())
#     phtest()
#   })
#   
#   # Create variable to Plot Shoenfelds residuals
#   shoenPlot_var = reactive ({
#     req(phtest())
#     zfit = cox.zph(cox())
#     zfit[as.numeric(input$selectPhPlot)]
#   })
#   
#   # Plot Shoenfeld residual - Need to turn dynamic
#   output$shoenPlot = renderPlot({
#     req(shoenPlot_var())
#     plot(shoenPlot_var()) + abline(h=coef(cox())[as.numeric(input$selectPhPlot)], lwd = 2, col = 'red', lty = 2 )
#   })
#   
#   # Dynamically identify number of predictors to examine for proportionality
#   observeEvent(phtest(), {
#     updatePickerInput(session,'selectPhPlot', choices = (2:(length(phtest()$table)/3)-1))
#   })
#   
#   # Get residuals to calculate predictor influence
#   influence_var = reactive({
#     req(cox())
#     resid(cox(), type = 'dfbeta')
#   })
#   
#   output$influence_plot = renderPlot({
#     plot(input$selectInfluencePred,influence_var()[,1],xlab='sex',ylab='influence')
#   })
#   
#   
#   # dynamically identify predictor names to explore influence
#   observeEvent(phtest(), {
#     updatePickerInput(session, 'selectInfluencePred', choices = str_replace(f.short()[["mmcolnames"]],'z',''))
#   })
#   
#   
#   # R script for testing cox assumptions
#   output$RcoxAssump = renderPrint({
#     cat(sprintf(
#       '## Test proportion assumption ##
# 
# # Create predicted variables - Used for Shoenfelds residuals
# z = predict(cox, type = "terms")
# f.short = cph(S ~ z, x = TRUE, y= TRUE)
# 
# # Shoenfields residuals
# phtest_var = cox.zph(f.short, transform = "identity", terms = FALSE)
# 
# # Print results 
# phtest_var
# 
# ## Plot hazard proptionality ##
# 
# # Select variable to plot
# zfit = cox.zph(cox)
# HzdPlot = zfit[as.numeric(%s)]
# 
# # Plot
# plot(HzdPlot) + abline(h=coef(cox)[%s], lwd = 2, col = "red", lty = 2 )'
#       , input$selectPhPlot, input$selectPhPlot))
#     
#   })
#   
#   ############
#   # Validate #
#   ############
#   
#   # # adjust time inclusion sliderinput range according to range in data
#   observeEvent(input$survDuration_output,{
#     req(apply(imputed_data()[which(input$survDuration_output == names(imputed_data()))], 2, is.numeric) == TRUE)
#     minRange = min(imputed_data()[which(input$survDuration_output == names(imputed_data()))])
#     maxRange = max(imputed_data()[which(input$survDuration_output == names(imputed_data()))])
#     # medRange = median(imputed_data()[which(input$survDuration_output == names(imputed_data()))])
#     updateSliderInput(session, 'Validate_timeInc', min = minRange, max = maxRange, value = maxRange/2)
#     updateSliderInput(session, 'survVar1', min = minRange, max = maxRange, value = maxRange/2)
#     updateSliderInput(session, 'survVar2', min = minRange, max = maxRange, value = maxRange/2)
#     updateSliderInput(session, 'survMedian', min = minRange, max = maxRange, value = c(maxRange/2.5, maxRange/1.5))
#   })
#   
#   # validate model using user defined time interval and number of bootstrapping
#   val = eventReactive(input$action_validate, {
#     req(cox())
#     withProgress( message = 'Validating', {
#       set.seed(1) # Set random seed
#       # updates model survival time (time inclusive) to user defined period 'input$Validate_timeInc'
#       cox = update(cox(), time.inc = input$Validate_timeInc)
#       rms::validate(cox, u = as.numeric(input$Validate_timeInc), B = as.numeric(input$Validate_bootstrap))
#     })
#   })
#   
#   # print validate results
#   output$validate = renderText({
#     req(val())
#     # calculate c.index from somers Rank
#     Dxy = val()[1,5]
#     c.index = Dxy*0.5+0.5
#     #Estimate overfitting by calculating shrinkage
#     shrinkage = val()[3,5]
#     # text describing validation
#     sprintf('Concordance index equals %f. Meaning that the model correctly ranks %f%% of the predicted outcome to observed data. Ideally this would want to be > 0.8. Furthernore the model is fitting %f%% noise. This should ideally be < 10%%', c.index, c.index*100, (1-shrinkage)*100)
#   })
#   
#   # calibrate - Create plot variable
#   cal = eventReactive(input$action_validate, {
#     req(cox())
#     withProgress(message = 'Calibrating', {
#       set.seed(1)
#       cox = update(cox(), time.inc = input$Validate_timeInc)
#       rms::calibrate(cox, B=as.numeric(input$Validate_bootstrap), u=as.numeric(input$Validate_timeInc), maxdim = 4)
#     })
#   })
#   
#   # Validating calibration with KM - Create plot variable
#   cal2 = eventReactive(input$action_validate, {
#     req(cox())
#     withProgress(message = 'Validating Calibration', {
#       set.seed(1)
#       cox = update(cox(), time.inc = input$Validate_timeInc)
#       rms::calibrate(cox, cmethod = 'KM', B=as.numeric(input$Validate_bootstrap), u=as.numeric(input$Validate_timeInc))
#     })
#   })
#   
#   # plot calibrated variable
#   output$cal_plot = renderPlot({
#     req(cal())
#     req(cal2())
#     plot(cal(), subtitles = FALSE)
#     legend('bottomright', legend = c('expected', 'observed'), col=c("black", "blue"), lty = 1, cex=0.8)
#     abline(h=0.5, col="grey", lty = 2, cex = 1.5)
#     plot(cal2(), add = TRUE)
#   })
#   
#   
#   # Print Rscript for validation and calibration
#   output$RcalVal = renderPrint({
#     cat(sprintf(
#       '#######################
# # Internal Validation #
# #######################
# 
# # Set Random Seed
# set.seed(1)
# 
# # Reaffirm time at which to validate
# cox = update(cox, time.inc = %s)
# 
# # Validation Using Bootstrapping
# val = validate(cox, u = %s, B = %s)
# 
# # Calculate concordance index from somers Rank
# Dxy = val[1,5]
# c.index = Dxy*0.5+0.5
# c.index
# 
# # Estimate overfitting by calculating shrinkage
# shrinkage = val[3,5]
# overfitting = (1-shrinkage)*100
# overfitting
# 
# ########################
# # Internal Calibration #
# ########################
# 
# # Set Random Seed
# set.seed(1)
# 
# # Update cox model time inclusion - match
# cox = update(cox, time.inc = %s)
# 
# # Calibration Using Bootstrapping
# cal = calibrate(cox, u = %s, B = %s, maxdim = 4)
# 
# # Validating calibration with Kaplier Meier
# set.seed(1)
# cal2 = calibrate(cox, cmethod = "KM", u = %s, B = %s)
# 
# # plot calibration
# plot(cal, subtitles = FALSE)
#   legend("bottomright", legend = c("expected", "observed"), col=c("black", "blue"), lty = 1, cex=0.8)
#   abline(h=0.5, col="grey", lty = 2, cex = 1.5)
#   plot(cal2, add = TRUE)',
#       input$Validate_timeInc, input$Validate_timeInc,
#       input$Validate_bootstrap, input$Validate_timeInc, input$Validate_timeInc, input$Validate_bootstrap,
#       input$Validate_timeInc, input$Validate_bootstrap))
#   })
#   
#   ###################
#   # Summarise Model #
#   ###################
#   
#   # Summarise Model1 - variable
#   summariseModel_var = eventReactive(input$action_SumModel, {
#     summary(cox())
#   })
#   
#   # Summarise Model1 - plot
#   output$summariseModel = renderPlot({
#     req(summariseModel_var())
#     plot(summariseModel_var(), log = TRUE, main = '')
#   })
#   
#   
#   # Summarise Model2 - variable
#   summariseModel_var2 = eventReactive(input$action_SumModelNom, {
#     # Add user specified survival times and median
#     surv = Survival(cox())
#     surv3 = function (x) surv(input$survVar1,lp=x)
#     surv5 = function (x) surv(input$survVar2,lp=x)
#     quan = Quantile(cox())
#     med = function (x) quan(lp=x)
#     ss = c(.05 ,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95)
#     
#     nomogram(cox(),
#              fun=list(surv3 , surv5 , med),
#              funlabel =c( sprintf(' %s-%s Survival ',input$survVar1, input$timeUnit_output) , sprintf(' %s-%s Survival ', input$survVar2, input$timeUnit_output),
#                           sprintf(' Median Survival Time (%s) ', input$timeUnit_output)),
#              fun.at=list(ss, ss, c(seq(input$survMedian[1],input$survMedian[2], length.out = input$survMedianStep))))
#   })
#   
#   # Summarise Model2 - variable
#   output$summariseModel2 = renderPlot({
#     req(summariseModel_var2())
#     plot(summariseModel_var2(), xfrac=.85, lmgp=.35)
#   })
#   
#   #################################
#   # Create survival estimate plot #
#   #################################
#   #* autopoulate user inputs for each predictor
#   
#   # Create empty list to store inputs ids for predictors
#   inputTagList <- tagList()
#   
#   # Create user inputs for each predictor
#   output$allInputs <- renderUI({
#     req(cox())
#     # get predictors used in model
#     pred = c(input$selectPred)
#     pCat = c(input$selectCat)
#     preds = c(pred, predOrdCat2(), predLog(), predCluster(), predStrat(), predRCS3(),
#               predRCS4(), predRCS5(), IntPred())
#     predsCat = pCat
#     
#     # Create inputs for numeric inputs and give median as default value
#     for (i in preds){
#       newInputId <- paste0("selecinput", i)
#       newInputLabel <- paste("Select Input for ", i)
#       medianValue = as.numeric(eval(parse(text = sprintf('median(imputed_data()$%s)', i))))
#       # Define new input
#       newInput <- numericInput(newInputId, newInputLabel, value = medianValue)
#       
#       # Append new input to list of existing inputs
#       inputTagList <<- tagAppendChild(inputTagList, newInput)
#     }
#     # create inputs for categorical data
#     for (i in predsCat){
#       newInputId <- paste0("selecinput", i)
#       newInputLabel <- paste("Select Input for ", i)
#       # Define new input and provide select input choice
#       newInput = eval(parse(text = sprintf('selectInput(newInputId, newInputLabel, choices = imputed_data()$%s, selected = imputed_data()$%s[1])',i, i)))
#       
#       # Append new input to list of existing inputs
#       inputTagList <<- tagAppendChild(inputTagList, newInput)
#     }
#     # Return updated list of inputs
#     inputTagList
#   })
#   
#   # Get Predictor names and inputed values for survival estimate
#   survEstimateVals = eventReactive(input$action_survEst,{
#     req(cox())
#     # Get list of predictors from cox model
#     pred = c(input$selectPred)
#     pCat = c(input$selectCat)
#     preds = c(pred, predOrdCat2(), predLog(), predCluster(), predStrat(), predRCS3(),
#               predRCS4(), predRCS5(), IntPred(), pCat)
#     
#     # Create datafrme of inputs
#     df = data.frame(matrix(nrow = 1))
#     for(i in preds){
#       value = eval(parse(text = sprintf('input$selecinput%s', i)))
#       newinput =  data.frame(value = value)
#       df = cbind(df, newinput)
#     }
#     df = df[,-1]
#     colnames(df) = preds 
#     df
#   })
#   
#   # Render table showing inputted values
#   output$survestTable = renderTable({
#     req(survEstimateVals())
#     survEstimateVals()
#   })
#   
#   # create servestimates with 95% confidence intervals
#   servEstimate = reactive({
#     req(survEstimateVals())
#     survest(cox(), survEstimateVals(), conf.int= 0.95)
#   })
#   
#   # ServEst median survival
#   medSurv = reactive({
#     df = data.frame(servEstimate()$surv,servEstimate()$time)
#     colnames(df) = c('Survival', 'Time')
#     df[which.min(abs(0.5-df$Survival)),]
#   })
#   
#   # Plot serv estimate
#   output$servEstimatePlot = renderPlot({
#     plot(servEstimate()$time, servEstimate()$surv,'s',ylim=c(0,1), xlab="Time", ylab="Survival")
#     lines(servEstimate()$time, servEstimate()$lower,'s', lty = 2)
#     lines(servEstimate()$time, servEstimate()$upper,'s', lty = 2)
#     # Median Survival
#     abline(h=medSurv()[1], col="blue", lty = 2)
#     abline(v = medSurv()[2], col = "blue", lty = 2)
#     # Add a legend
#     legend("topright", legend=c("Survival Estimate", "95%% Confidence Interval", "Closest Median Survival line"),
#            lty=c(1,2,2), col=c("black", "black", "blue"), cex=0.8)
#   })
#   
#   output$RsummaryPrint = renderPrint({
#     # Get inputted survest values as string to later convert into data frame
#     predVals = c()
#     PredNumeric = survEstimateVals() %>% select_if(is.numeric)
#     PredChar = survEstimateVals() %>% select_if(is.character)
#     # loop numeric values in df
#     for (i in 1:length(PredNumeric)){
#       newentry = paste(colnames(PredNumeric)[i],'=',PredNumeric[i])
#       predVals = append(predVals, newentry)
#     }
#     # Loop chararacter values in df
#     for (i in 1:length(PredChar)){
#       newentry = paste0(colnames(PredChar)[i],' = ',"'",PredChar[i],"'")
#       predVals = append(predVals, newentry)
#     }
#     # Create string
#     SurvEstValsString = paste(predVals, collapse = ', ')
#     
#     cat(sprintf(
#       '#####################
# # Summarising Model #
# #####################
# 
# # Summarise Model method 1
# sumMod1 = summary(cox)
# plot(sumMod1, log = TRUE, main = "")
# 
# # Summarise Model method 2
# #* Add user specified survival times and median
# surv = Survival(cox)
# surv3 = function (x) surv(%s,lp=x)
# surv5 = function (x) surv(%s,lp=x)
# quan = Quantile(cox)
# med = function (x) quan(lp=x)
# ss = c(.05 ,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95)
# 
# #* plot nonogram  
# sumMod2 = nomogram(cox,
#                    fun=list(surv3 , surv5 , med),
#                    funlabel =c( " %s-%s Survival " , " %s-%s Survival ",
#                                 " Median Survival Time (%s) "),
#                    fun.at=list(ss, ss, c(seq(%s,%s, length.out = %s))))
# plot(sumMod2)
#   
# # Survival Estimates
# #* predictor inputs
# df = data.frame(%s)
# 
# #* Create survival estimates for inputted values with 95%% confidence intervals
# survEstFit = survest(cox, df, conf.int= 0.95)
# 
# #* Find "closest" to median survival
# df2 = data.frame(survEstFit$surv,survEstFit$time)
# medSurv = df2[which.min(abs(0.5-df2$survEstFit.surv)),]
# 
# # Plot Survival Estimates
# plot(survEstFit$time, survEstFit$surv,"s",ylim=c(0,1), xlab="Time", ylab="Survival")
#   lines(survEstFit$time, survEstFit$lower,"s", lty = 2)
#   lines(survEstFit$time, survEstFit$upper,"s", lty = 2)
# # Add Median Survival Line
#   abline(h=medSurv[1], col="blue", lty = 2)
#   abline(v = medSurv[2], col = "blue", lty = 2)
# # Add Plot legend
#   legend("topright", legend=c("Survival Estimate", "95%% Confidence Interval", "Closest Median Survival line"),
#          lty=c(1,2,2), col=c("black", "black", "blue"), cex=0.8)', 
#       input$survVar1, input$survVar2, input$survVar1,
#       input$timeUnit_output, input$survVar2, input$timeUnit_output, input$timeUnit_output,
#       input$survMedian[1], input$survMedian[2], input$survMedianStep, SurvEstValsString))
#   })
#   
#   
#   #############
#   # Parsimony #
#   #############
#   
#   ###### Identify redundant variables ######
#   # Get Somers Rank of variables
#   sDxy = reactive({
#     req(cox())
#     pred = coxPred_comp()
#     eval(parse(text = sprintf('with(imputed_data(), rcorrcens(S() ~ %s))', pred)))
#   })
#   
#   # plot Somers
#   output$plot_sDxy = renderPlot({
#     req(sDxy())
#     plot(sDxy())
#   })
#   
#   # redundancy - variable
#   redun = reactive({
#     req(cox())
#     pred = coxPred_redun()
#     x = sprintf('with(imputed_data(), Hmisc::redun(~ %s, r2 = input$redunR2, type = "adjusted"))', pred)
#     eval(parse(text = x))
#   })
#   
#   # redundancy print
#   output$redun_print = renderPrint({
#     req(redun())
#     redun()$Out
#   })
#   
#   ##### Create new reduce model from linear predictors ######
#   
#   # Get linear predictors of full model
#   f.small = reactive({
#     req(cox())
#     f.small = predict(cox(), type= 'lp')
#     f.small
#   })
#   
#   #### Creating Reduced Cox Model Predictors ####
#   
#   # Create list of predictors
#   observeEvent(f.short() , {
#     pred = str_replace(f.short()[["mmcolnames"]],'z','')
#     pred = pred %>% discard(pred %in% c(input$survDuration_output, input$eventCol_output))
#     updatePickerInput(session, "selectPred_pars", choices= pred)
#     updatePickerInput(session, "selectOrdCat_pars", choices= pred)
#     updatePickerInput(session, "selectLog_pars", choices= pred)
#     updatePickerInput(session, "selectCluster_pars", choices= pred)
#     updatePickerInput(session, "selectStrat_pars", choices= pred)
#     updatePickerInput(session, "selectRCS3_pars", choices= pred)
#     updatePickerInput(session, "selectRCS4_pars", choices= pred)
#     updatePickerInput(session, "selectRCS5_pars", choices= pred)
#   })
#   
#   
#   # Loop Ordinal categorical predictors - survival package
#   predOrdCat_pars = reactive({
#     pOrCat = c()
#     for (i in input$selectOrdCat_pars){
#       pOrCat = append(pOrCat, sprintf('factor(%s)', i))
#     }
#     pOrCat
#   })
#   
#   # Loop Ordinal Categorical predictors - rms package
#   predOrdCat_pars2 = reactive({
#     pOrCat = c()
#     for (i in input$selectOrdCat_pars){
#       pOrCat = append(pOrCat, sprintf('scored(%s)', i))
#     }
#     pOrCat
#   })
#   
#   # Loop log predictors
#   predLog_pars = reactive({
#     logx = c()
#     for (i in input$selectLog_pars){
#       logx = append(logx, sprintf('log(%s + 0.0000000001)', i))
#     }
#     logx
#   })
#   
#   # Loop Cluster predictors
#   predCluster_pars = reactive({
#     cluster = c()
#     for (i in input$selectCluster_pars){
#       cluster = append(cluster, sprintf('cluster(%s)', i))
#     }
#     cluster
#   })
#   
#   # Loop Stratified predictors
#   predStrat_pars = reactive({
#     stratx = c()
#     for (i in input$selectStrat_pars){
#       stratx = append(stratx, sprintf('strat(%s)', i))
#     }
#     stratx
#   })
#   
#   # Loop RCS3 predictors
#   predRCS3_pars = reactive({
#     RCS3 = c()
#     for (i in input$selectRCS3_pars){
#       RCS3 = append(RCS3, sprintf('rcs(%s, 3)', i))
#     }
#     RCS3
#   })
#   
#   # Loop RCS4 predictors
#   predRCS4_pars = reactive({
#     RCS4 = c()
#     for (i in input$selectRCS4_pars){
#       RCS4 = append(RCS4, sprintf('rcs(%s, 4)', i))
#     }
#     RCS4
#   })
#   
#   # Loop RCS5 predictors
#   predRCS5_pars = reactive({
#     RCS5 = c()
#     for (i in input$selectRCS5_pars){
#       RCS5 = append(RCS5, sprintf('rcs(%s, 5)', i))
#     }
#     RCS5
#   })
#   
#   # Identify all predictors selected for parsimony
#   pars_preds = reactive({
#     req(cox())
#     c(input$selectPred_pars, input$selectOrdCat_pars,
#       input$selectCluster_pars, input$selectLog_pars, input$selectStrat_pars,
#       input$selectRCS3_pars, input$selectRCS4_pars, input$selectRCS5_pars)
#   })
#   
#   # warning if more predictor found in more then one category for parsinomy
#   output$warningPredPars = renderText({
#     if (length(pars_preds()[duplicated(pars_preds())]) > 0){
#       duplicated = paste(pars_preds()[duplicated(pars_preds())], collapse = ', ')
#       sprintf('Warning the following predictors are found in more than one category: %s. This will cause issues when developing the Model', duplicated)
#     }
#   })
#   
#   # Print old model
#   output$oldModel = renderText({
#     req(imputed_data())
#     sprintf('cox = cph(S ~ %s,  x = TRUE, y = TRUE, surv = TRUE)', toString(fullCoxPrint_var()))
#   })
#   
#   # Print new reduced model
#   redModel_var = reactive({
#     req(f.short())
#     pred = c(input$selectPred_pars)
#     pred = paste(c(pred, predOrdCat_pars(), predLog_pars(), predCluster_pars(), predStrat_pars(), predRCS3_pars(),
#                    predRCS4_pars(), predRCS5_pars()), collapse=" + ")
#     sprintf('ols(f.small ~ %s)', pred)
#   })
#   
#   # Print new reduced model
#   output$redModel = renderText({
#     req(f.short())
#     redModel_var()
#   })
#   
#   # Create reduced model - variable
#   f.reduce_var = eventReactive(input$calPars, {
#     req(f.short())
#     pred = c(input$selectPred_pars)
#     pCat = c(input$selectCat_pars)
#     pred = paste(c(pred, predOrdCat(), predCluster_pars(), predStrat_pars(), predRCS3_pars(),
#                    predRCS4_pars(), predRCS5_pars()), collapse=" + ")
#     eval(parse(text = sprintf('with(imputed_data(), ols(f.small() ~ %s))', pred)))
#   })
#   
#   #Anova assessment of reduced model
#   output$anovRed = renderPrint({
#     req(f.reduce_var())
#     anova(f.reduce_var())
#   })
#   
#   # Compare goodness of fit between full and reduced model - Var
#   plotpars_var = reactive({
#     req(f.reduce_var())
#     predict(f.reduce_var())
#   })
#   
#   # plot goodness of fit between full and reduced model
#   output$plotPars = renderPlot({
#     req(plotpars_var())
#     plot(plotpars_var(), f.small())
#   })
#   
#   # Get Spearmans Rho^2 value for goodness of fit
#   output$Spearman = renderText({
#     req(plotpars_var())
#     Rho = spearman2(f.small()~plotpars_var(), p=2)
#     sprintf('The Spearmans Rho^2 correlation between the full and reduced model equals %s. (where > 0.7 is a close fit)', Rho[1])
#   })
#   
#   # Rscript print parsimony
#   output$Rparsimony = renderPrint({
#     pred = c(input$selectPred_pars)
#     pCat = c(input$selectCat_pars)
#     pred = paste(c(pred, predOrdCat(), predCluster_pars(), predStrat_pars(), predRCS3_pars(),
#                    predRCS4_pars(), predRCS5_pars()), collapse=" + ")
#     cat(sprintf(
#       '# Plot Somers Rank with current model predictors
# sDxy_cox = rcorrcens(S ~ %s)
# plot(sDxy_cox)
# 
# # Get redundancy analysis
# redAnalysis = redun(~ %s, r2 = %s, type = "adjusted")
# redAnalysis$Out
# 
# # Get Linear predictors from full model
# f.small = predict(cox, type= "lp")
# 
# # Get reduced model
# f.reduced = ols(f.small ~ %s)
# 
# # Anova assessment of reduced model
# anova(f.reduced)
# 
# # Compare goodness of fit between full and reduced model - Var
# f.reducedPred = predict(f.reduced)
# 
# # plot goodness of fit between full and reduced model
# plot(f.reducedPred, f.small)
# 
# # Get Spearmans Rho^2 value for goodness of fit
# Rho = spearman2(f.small~f.reducedPred, p=2)
# Rho[1]', coxPred_comp(), coxPred_redun(),
#       input$redunR2, toString(pred)))
#   })
#   
#   #######################
#   # External Validation #
#   #######################
#   
#   # Print R Script for external validation
#   output$RextVal = renderPrint({
#     # list of predictors used in full model
#     coxPredictors = toString(fullCoxPred())
#     
#     # Event column for cox model
#     x = c(input$eventElement_output)
#     x = paste(shQuote(x), collapse=", ")
#     # Event column for external model
#     x2 = c(input$extEventElement_output)
#     x2 = paste(shQuote(x2), collapse=", ")
#     cat(sprintf(
#       '############################
# # Import external data set #
# ############################
# 
# extVal = read_csv("extVal.csv")
# 
# ###########################################################
# # Identify predictors in model not found in external data #
# ###########################################################
# 
# # Get all predictors names used from full model
# coxPred = %s
# 
# # Allow `not` in function
# `%%!in%%` = Negate(`%%in%%`)
# 
# # For loop to idenify predictors in full model not present in external dataset
# excludePred = c()
# for (i in coxPred){
#   if (i %%!in%% names(extVal)){
#     excludePred = append(excludePred, i)
#   }
# }
# 
# # Missing predictors
# excludePred
# 
# ###################################
# # Create external survival object #
# ###################################
# 
# # attach external data to global environment
# attach(extVal)
# 
# # Create datadist for external data
# dd = datadist(extVal); options(datadist = "dd")
# 
# # Remove previous time variable
# rm(%s)
# 
# # Create Survival object
# S_ext = Surv(%s,%s %%in%% c(%s))
# 
# 
# #######################################################
# # Estimate Descrimination accuracy from external data #
# #######################################################
# 
# # Calculate linear of external data from predictors from full model
# f.reduced_val = ols(f.small ~ %s, data=imputed_data, sigma=1, x=T, y=T)
# 
# # explore linear predictor output
# f.reduced_val[["%s"]]
# 
# 
# ### Create new data frame comprising of columns from external validation dataset which match cox model predictors ###
# 
# # Intialise dataframe with external validation time and event status values
# newdata = data.frame(%s = extVal$%s, %s = extVal$%s)
# 
# # Add additional columns to data frame used in Cox Model
# for(i in 1:length(coxPred)) {  # Loop if predictor found in external dataset
#   if (coxPred[i] %%in%% names(extVal)) { #only loop through columns found in external data
#     new = eval(parse(text = sprintf("extVal$%%s", coxPred[i]))) # Create new column
#     newdata[, ncol(newdata) + 1] = new                  # Append new column
#     colnames(newdata)[ncol(newdata)] = coxPred[i]  # Rename column name
#   }
# }
# 
# # Show new data table
# newdata
# 
# # Predict linear predictors based on external data
# f.reduced_pred = predict(f.reduced_val,newdata=newdata,type="lp")
# 
# # Calculate concordance
# Cidx = rcorr.cens(x = f.reduced_pred , S = S_ext)
# 1 - Cidx[1]
# 
# ########################
# # External Calibration #
# ########################
# 
# #* Using the cox model created using the survival package (cox2), need to substitute
# #* coefficients obtained from the model developed from the linear predictors only
# #* as well as adjust naming convention of `factor()` variables so that rms 
# #* and surv packages are compatible
# 
# # cox model from survival package
# f.tmp = cox2
# 
# # Alter categorical colnames to allow rms and surv cox model naming conventions compatible by removing "=" sign
# names(f.reduced_val$coefficients) = str_remove(names(f.reduced_val$coefficients), "=")
# 
# # Swap coefficients from reduced Cox model with linear model coefficients (excluding intercept)
# f.tmp$coefficients = f.reduced_val$coefficients[2:length(f.reduced_val$coefficients)]
# 
# # explore external calibration output
# f.tmp[["%s"]]
# 
# ### Create new variables and update newdata datatable to allow for calibration using 3 different models ###
# 
# # Predict the expected number of events in the external validation data
# p0 = predict(f.tmp, newdata = newdata, type = "expected")
# 
# #*If the expected number of events is approximately 0, calculation of the
# #* logarithm can produce "infinity", so we make them "small enough" to avoid it
# p0[abs(p0)<0.00001]<-0.00001
# p = log(p0)
# 
# # predict linear predictor
# lp = predict(f.tmp, newdata = newdata, type = "lp")
# 
# # logbase
# logbase = p-lp
# 
# # Append new variables to dataframe
# newdata$p = p
# newdata$lp = lp
# newdata$logbase = p-lp
# 
# #* Incase the `event` column is in a character format, convert to a 0/1 format so
# #* external calibration function works
# 
# newdata = newdata %%>%%
#     mutate(%s = if_else(str_detect(%s, c(%s)) == TRUE, 1, 0))
# 
# #####################################
# # Model 1: Calibration-in-the-large #
# #####################################
# 
# #* removing baseline hazard allows for poisson!
# # Fit model
# fit1 = glm(%s ~ offset(p), family = poisson, data = newdata)
# 
# # Standardised Incidence Ratio (SIR)
# s1 = summary(fit1)[[13]]
# SIR_mod1 = exp(s1[1])
# 
# # Confidence intervals
# SIRCI_mod1 = c(exp(s1[1]-1.96*s1[2]),exp(s1[1]+1.96*s1[2]))
# 
# ##############################
# # Model 2: Calibration slope #
# ##############################
# 
# # Fit model
# fit2 = glm(%s ~ lp + offset(logbase), family = poisson, data = newdata)
# 
# # Standardised Incidence Ratio (SIR)
# s2 = summary(fit2)[[13]]
# SIR_mod2 = exp(s2[2])
# 
# # Confidence Interval
# SIRCI_mod2 = c(exp(s2[2]-1.96*s2[4]),exp(s2[2]+1.96*s2[4]))
# 
# ############################
# # Model 3: Goodness of fit #
# ############################
# 
# # Create risk groups (user defined)
# rgroups = cut(newdata$lp, c(-Inf, quantile(newdata$lp,(1:(%s-1))/(%s),na.rm=TRUE), Inf))
# 
# # Render risk group table to ensure valid sample size per group
# #* Global Goodness of fit Guidelines recommends 80%% of groups should have >=5 events
# riskGroupTable = as.matrix(table(rgroups, newdata$%s))
# riskGroupTable
# 
# # Fit model
# fit3 = glm(%s ~ -1 + rgroups + offset(p), family = poisson, data = newdata)
# 
# # Calulate table variables
# s3 = summary(fit3)[[13]]
# riskGroups = 1:%s
# SIRgof = exp(s3[1:%s])
# SIRgofCI = paste("[",round(exp(s3[1:%s]-1.96*s3[(1+%s):(2*%s)]),3),"-", round(exp(s3[(1:%s)]+1.96*s3[(1+%s):(2*%s)]),3),"]")
# SIR_mod3 = cbind(riskGroups, SIRgof,SIRgofCI)
# colnames(SIR_mod3) = c("Risk Group", "SIRgof", "95%% CI")
# 
# # Show Table
# SIR_mod3
# '
# , paste(strsplit(coxPredictors, ', ')), input$extSurvDuration_output, input$extSurvDuration_output,
# input$extEventCol_output,x, toString(predCoxExt()), input$ols_selector, input$extSurvDuration_output,
# input$extSurvDuration_output, input$extEventCol_output, input$extEventCol_output,
# input$f.tmp_selector, input$eventCol_output, input$eventCol_output, x2, input$extEventCol_output,
# input$extEventCol_output, input$rgroupInput, input$rgroupInput, input$extEventCol_output,
# input$extEventCol_output, input$rgroupInput,input$rgroupInput,input$rgroupInput,
# input$rgroupInput, input$rgroupInput, input$rgroupInput, input$rgroupInput, input$rgroupInput
#     ))
#   })
#   
#   # Identify predictors in full model which are absent in external data
#   predMissing = reactive({
#     req(extVal())
#     # get all predictors names used from full model
#     pred = fullCoxPred()
#     
#     # allow not in function
#     `%!in%` = Negate(`%in%`)
#     
#     # For loop to idenify predictors in full model not present in external dataset
#     excludePred = c()
#     for (i in pred){
#       if (i %!in% names(extVal())){
#         excludePred = append(excludePred, i)
#       }
#     }
#     
#     # Return message if all predictors are present
#     if (length(excludePred > 0)){
#       excludePred
#     } else {
#       return('None')
#     }
#   })
#   
#   # Print predictors in full model not found in external data
#   output$excPred = renderPrint ({
#     req(predMissing())
#     predMissing()
#   })
#   
#   ###### dynamically identify columns for survival object #####
#   
#   # select duration column for survival object
#   output$extSurvDuration_input = renderUI({
#     req(extVal())
#     pickerInput("extSurvDuration_output", "Select Event Time Column", names(extVal()), selected = input$survDuration_output)
#   })
#   
#   # Dynamically identify columns in database for selecting survival event
#   output$extEventCol_input <- renderUI({
#     req(extVal())
#     pickerInput("extEventCol_output", "Select Event Column", names(extVal()), selected = input$eventCol_output)
#   })
#   
#   # Dynamically identify distinct elements in user specified column (above)
#   output$extEventElement_input = renderUI({
#     req(extVal())
#     selectInput("extEventElement_output", "Select Event(s) Type",
#                 extVal()[,input$extEventCol_output], multiple = TRUE, selected = input$eventElement_output)
#   })
#   
#   # Create survival object with external data
#   ext_S = eventReactive(input$calExtSurv, {
#     req(extVal())
#     attach(extVal())
#     # Need to create a global data dist object so later rms objects can access
#     #* Note shiny doesn't like global objects so to force it we use syntax '<<-'
#     dd <<- datadist(extVal()); options(datadist = 'dd')
#     dd
#     # Survival object
#     x = c(input$extEventElement_output)
#     x = paste(shQuote(x), collapse=", ")
#     S = sprintf('with(extVal(), Surv(%s,%s %%in%% c(%s)))',input$extSurvDuration_output, input$extEventCol_output, x)
#     S = eval(parse(text = S))
#     detach(extVal())
#     S
#   })
#   
#   # print external Survival object
#   output$ext_sObject_text = renderPrint({
#     req(ext_S())
#     ext_S()
#   })
#   
#   # Identify all predictors found in cox model and external data only
#   predCoxExt = reactive({
#     # Get all predictors from cox model
#     coxPred = fullCoxPred()
#     # Create empty list
#     predList = c()
#     # For loop to identify predictors in full model in external dataset
#     for (i in coxPred){
#       if (i %in% names(extVal())){
#         predList = append(predList, i)
#       }
#     }
#     # Turn into single string with a plus seperating elements
#     paste(predList, collapse=" + ")
#   })
#   
#   # Get linear predictors from full model and p
#   f.reduced_val = reactive({
#     # calculate linear predictors from full model
#     x = sprintf('ols(f.small() ~ %s, data=imputed_data(), sigma=1, x=T, y=T)', toString(predCoxExt()))
#     eval(parse(text = x))
#   })
#   
#   # Create new data frame comprising of columns from external validation dataset which match cox model predictors
#   newdata = reactive({
#     req(extVal())
#     # get all predictors names used from full model
#     pred = fullCoxPred()
#     
#     # Intialise dataframe with external validation time and event status values
#     Survtime = input$extSurvDuration_output
#     Survevent = input$extEventCol_output
#     
#     newdata = sprintf('data.frame(%s = extVal()$%s, %s = extVal()$%s)', Survtime, Survtime, Survevent, Survevent)
#     newdata = eval(parse(text = newdata))
#     
#     # Add additional columns to data frame used in Cox Model
#     # for loop add columns
#     for(i in 1:length(pred)) {  # Loop if predictor found in external dataset
#       if (pred[i] %in% names(extVal())) { #only loop through columns found in external data
#         new = eval(parse(text = sprintf('extVal()$%s', pred[i]))) # Create new column
#         newdata[ , ncol(newdata) + 1] <- new                  # Append new column
#         colnames(newdata)[ncol(newdata)] <- pred[i]  # Rename column name
#       }
#     }
#     newdata
#   })
#   
#   # Create drop down for exploring f.reduced model
#   output$f.reduced_val_search = renderUI({
#     req(f.reduced_val())
#     pickerInput("ols_selector", "Further information on Linear Model ", names(f.reduced_val()))
#   })
#   
#   # show ols selector
#   output$test = renderPrint({
#     req(f.reduced_val())
#     f.reduced_val()[[input$ols_selector]]
#   })
#   
#   output$testTable = renderTable({
#     req(newdata())
#     newdata()
#   })
#   
#   # Determine concordance index from external data
#   Cidx_val = reactive({
#     req(newdata())
#     req(ext_S())
#     # Predict linear predictors of based on external data
#     f.reduced_pred = predict(f.reduced_val(),newdata=newdata(),type='lp')
#     
#     # Calculate concordance
#     Cidx = rcorr.cens(x=f.reduced_pred,S= ext_S())
#     
#     if (Cidx[1] <= 1){
#       sprintf('When validating against external data the concordance index equals: %.3f', 1-Cidx[1])
#     } else {
#       sprintf('When validating against external data the concordance index equals: %.3f', Cidx[1])
#     }
#   })
#   
#   # Print concordnace
#   output$ext_con = renderText ({
#     req(Cidx_val())
#     Cidx_val()
#   })
#   
#   ########################
#   # External Calibration #
#   ########################
#   
#   # In full cox model from surv package substitute coefficients with linear predictor
#   #coefs and adjust naming convention of as.factor to be compatible between rms and surv packages
#   f.tmp = eventReactive(input$calExCalibrate, {
#     req(f.reduced_val())
#     f.reduced_valCon = f.reduced_val()
#     cox_Surv = cox_Surv()
#     # alter categorical colnames to allow rms and surv cox model naming conventions compatible by removing '=' sign
#     names(f.reduced_valCon$coefficients) = str_remove(names(f.reduced_valCon$coefficients), '=')
#     ## Swap coefficients from reduced Cox model with linear model coefficients (excluding intercept)
#     cox_Surv$coefficients = f.reduced_valCon$coefficients[2:length(f.reduced_valCon$coefficients)]
#     
#     cox_Surv
#   })
#   
#   # Create drop down for exploring external calibration data
#   output$f.tmp_search = renderUI({
#     req(f.tmp())
#     pickerInput("f.tmp_selector", "Explore External Calibration Model ", names(f.tmp()))
#   })
#   
#   # Explore external calibration data 
#   output$testCalibrate = renderPrint({
#     req(f.tmp())
#     f.tmp()[[input$f.tmp_selector]]
#   })
#   
#   # update newdata() datatable
#   newdata_update = eventReactive(input$calExCalibrate, {
#     req(f.tmp())
#     f.reduced_valCon = f.reduced_val()
#     cox_Surv = cox_Surv()
#     # alter categorical colnames to allow rms and surv cox model naming conventions compatible by removing '=' sign
#     names(f.reduced_valCon$coefficients) = str_remove(names(f.reduced_valCon$coefficients), '=')
#     ## Swap coefficients from reduced Cox model with linear model coefficients (excluding intercept)
#     cox_Surv$coefficients = f.reduced_valCon$coefficients[2:length(f.reduced_valCon$coefficients)]
#     
#     p0 = predict(cox_Surv, newdata = newdata(), type = 'expected')
#     
#     ### If the expected number of events is approximately 0, calculation of the
#     ### logarithm can produce "infinity", so we make them "small enough" to avoid it
#     p0[abs(p0)<0.00001]<-0.00001
#     p = log(p0)
#     lp = predict(cox_Surv, newdata = newdata(), type = 'lp')
#     logbase = p-lp
#     
#     # add created variables to newdata dataframe
#     newdata = newdata()
#     newdata$p = p
#     newdata$lp = lp
#     newdata$logbase = p-lp
#     
#     # convert status column from character to numeric vector (allows external calibration when status is not 0/1)
#     x = c(input$eventElement_output)
#     x = paste(shQuote(x), collapse=", ")
#     newdata2 =  sprintf('newdata = newdata %%>%%
#     mutate(%s = if_else(str_detect(%s, c(%s)) == TRUE, 1, 0))', input$eventCol_output, input$eventCol_output, x)
#     eval(parse(text = newdata2))
#   })
#   
#   # Show updated table for external calibration
#   output$testnewdatatable = renderTable({
#     req(newdata_update())
#     if(input$radioShowExtCalTable == 'Yes'){
#       newdata_update()
#     }
#   })
#   
#   
#   ###### Model 1: Calibration-in-the-large ######
#   Model1 = reactive ({
#     req(newdata_update())
#     x = sprintf('glm(%s ~ offset(p), family = poisson, data = newdata_update())', input$extEventCol_output)
#     eval(parse(text = x))
#   })
#   
#   Model1_SIR = reactive({
#     req(Model1())
#     s1<-summary(Model1())[[13]]
#     SIR<-exp(s1[1])
#     SIRCI<-c(exp(s1[1]-1.96*s1[2]),exp(s1[1]+1.96*s1[2]))
#     c(SIR, SIRCI)
#     
#   })
#   
#   output$Model1Results = renderText({
#     req(Model1())
#     
#     sprintf('Calibration Model 1:\n
#           This model calculates "Standardised Incidence Ratio" (SIR) which estimates the ratio between the observed events in the validation data against the number of events in the predicted risk model, which is calculated from the training data.\n\n
#           Based on the given inputs the SIR has been calculated at %.3f.
# 
#           This means the expected predicted risk is %.3f higher/lower (depending of if value is positive or negative) than the original data.\n\nThis has a 95%% confidence interval of [%.3f, %.3f]', Model1_SIR()[1], (Model1_SIR()[1] - 1)*100, Model1_SIR()[2], Model1_SIR()[3])
#   })
#   
#   ###### Model 2: Calibration slope ######
#   Model2 = reactive ({
#     req(newdata_update())
#     x = sprintf('glm(%s ~ lp + offset(logbase), family = poisson, data = newdata_update())', input$extEventCol_output)
#     eval(parse(text = x))
#   })
#   
#   Model2_SIR = reactive({
#     req(Model2())
#     s1<-summary(Model2())[[13]]
#     SIR<-exp(s1[2])
#     SIRCI<-c(exp(s1[2]-1.96*s1[4]),exp(s1[2]+1.96*s1[4]))
#     c(SIR, SIRCI)
#   })
#   
#   output$Model2Results = renderText({
#     req(Model2())
#     sprintf('For each unit increase in the risk score in the validation population data, there is a %.3fx increase/decrease risk (depending of positive or negative). This has a 95%% confidence interval of [%.3f, %.3f]', Model2_SIR()[1], Model2_SIR()[2], Model2_SIR()[3])
#   })
#   
#   
#   ###### Model 3: Goodness of fit ######
#   
#   # Create risk groups (user defined)
#   rgroups = eventReactive(input$calRiskGroup,{
#     cut(newdata_update()$lp, c(-Inf, quantile(newdata_update()$lp,(1:(input$rgroupInput-1))/(input$rgroupInput),na.rm=TRUE), Inf))
#   })
#   
#   #render risk group table to ensure valid sample size per group
#   output$riskGroupTable = renderPrint({
#     req(rgroups())
#     x = sprintf('as.matrix(table(rgroups(), newdata_update()$%s))', input$extEventCol_output)
#     eval(parse(text = x))
#   })
#   
#   # Create Model
#   Model3 = reactive ({
#     req(rgroups())
#     x = sprintf('glm(%s ~ -1 + rgroups() + offset(p), family = poisson, data = newdata_update())', input$extEventCol_output)
#     eval(parse(text = x))
#   })
#   
#   # Calulate table variable
#   Model3_SIR = eventReactive(input$calRiskGroup, {
#     s3 = summary(Model3())[[13]]
#     riskGroup = 1:input$rgroupInput
#     SIRgof = exp(s3[1:input$rgroupInput])
#     SIRgofCI = paste('[',round(exp(s3[1:input$rgroupInput]-1.96*s3[(1+input$rgroupInput):(2*input$rgroupInput)]),3),'-', round(exp(s3[(1:input$rgroupInput)]+1.96*s3[(1+input$rgroupInput):(2*input$rgroupInput)]),3),']')
#     summary = cbind(riskGroup, SIRgof,SIRgofCI)
#     colnames(summary) = c('Risk Group', 'SIRgof', '95% CI')
#     summary
#   })
#   
#   # render table
#   output$Model3Results = renderTable({
#     req(Model3_SIR())
#     Model3_SIR()
#   })
#   
#   
#   ##################
#   # observe events #
#   ##################
#   
#   # Dynamically identify column names from input file (used to choose cols to view in imputed table)
#   #Select columns names
#   observeEvent(data_input(), {
#     updatePickerInput(session, "selectImpute", choices= as.character(colnames(data_input())))
#   })
#   
#   
#   ####################
#   # Download buttons #
#   ####################
#   
#   # Missing Plot 1 - not working as not a ggplot
#   output$download_Missing1 = downloadHandler(
#     filename = function() {
#       paste("MissingFraction1-", Sys.Date(), ".png", sep="")
#     },
#     content = function(file) {
#       device = function(..., width, height) {
#         grDevices::png(..., width = width, height = height,
#                        res = 300, units = "in")
#       }
#       ggsave(file, plot = Missing1(), device = device)
#     })
#   
#   # Missing plot 2 - not working as not a ggplot
#   output$download_Missing2 = downloadHandler(
#     filename = function() {
#       paste("MissingFraction2-", Sys.Date(), ".png", sep="")
#     },
#     content = function(file) {
#       device = function(..., width, height) {
#         grDevices::png(..., width = width, height = height,
#                        res = 300, units = "in")
#       }
#       ggsave(file, plot = Missing2(), device = device)
#     })
#   
#   
#   # plot imputed values - not working as not a ggplot
#   output$download_imputed = downloadHandler(
#     filename = function() {
#       paste("imputedpred-", Sys.Date(), ".png", sep="")
#     },
#     content = function(file) {
#       device = function(..., width, height) {
#         grDevices::png(..., width = width, height = height,
#                        res = 300, units = "in")
#       }
#       ggsave(file, plot = ggplot(trans()), device = device)
#     })
#   
#   # Download imputed table
#   output$download_impute = downloadHandler(
#     filename = function(){
#       paste0('imputeValues-', Sys.Date(), '.csv')
#     },
#     content = function(file){
#       write_csv(impute(), file)
#     }
#   )
#   
#   # Download table of imputed values only
#   output$download_impute = downloadHandler(
#     filename = function(){
#       paste0('imputeValues-', Sys.Date(), '.csv')
#     },
#     content = function(file){
#       write_csv(impute(), file)
#     }
#   )
#   
#   # Download new data table with imputed values
#   output$download_newImputedTable = downloadHandler(
#     filename = function(){
#       paste0('imputedDataTable-', Sys.Date(), '.csv')
#     },
#     content = function(file){
#       write_csv(imputed_data(), file)
#     })
#   
#   # Download predicted variables plot
#   output$download_predictedVars = downloadHandler(
#     filename = function() {
#       paste("PredictedVars-", Sys.Date(), ".png", sep="")
#     },
#     content = function(file) {
#       device = function(..., width, height) {
#         grDevices::png(..., width = width, height = height,
#                        res = 300, units = "in")
#       }
#       ggsave(file, plot = pred_var(), device = device)
#     })
  
  
  
}