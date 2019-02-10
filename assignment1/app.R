#
# Basis Concept Mobile Web App.
#
# Requirements:
#
# 1. One question at the time.
#
# Necessary parts:
#
# 1. counter
# 2. dataframe with questions and specification of dependencies
# 3. counter + 1 button
# 4. dynamic UI

library('shiny')
library('tidyverse')

word_pairs <- sample(read_csv(file = "word_pairs.csv")[[1]])

cn <- str_replace(word_pairs, " vs. ", "_")

# Dataframe with questions
questions <- tibble(
  type = c(rep("text", 3), 
           rep("slider", 
               length(word_pairs))),
  id = c("info", "consent", "instruction", cn),
  label = c("In this experiment you will have to indicate how closely related pairs of words are. For example
            `flying` and `airplane` are closely related, while `pear` and `acceptance` are not.", 
            "Statement of informed consent: I understand that the responses I provide in this
            experiment will be stored anonymously. I furthermore understand that I am free to
            terminate my participation in this experiment at any time by closing the form. I am 18 years or older. 
            After the experiment is finished, it is possible to view and download all of the anonymous results. By
            pressing `Next` below I give my informed consent.", 
            "Please rate for the following word-pairs how closely they are related. 0 means that 
            they are not related at all. 10 means that they are very closely related.",
            word_pairs)
)

saveData <- function(columns, input) {
  name <- str_c(str_replace_all(Sys.time(), "\\s|\\:", "_"), ".csv")
  results <- list()
  for (column in columns)
    results[[column]] <- input[[column]]
    results <- as_tibble(results)
  
  #file.create(name, showWarnings = TRUE)
  write.csv(x = results, 
            file = file.path(str_c("results/", name)),
            row.names = F)
}

loadData <- function() {
  files <- list.files(file.path("./results"), full.names = TRUE)
  data <- do.call(rbind, lapply(files, read.csv)) %>%
    drop_na()
  as_tibble(data)
}

# Default values for all the arguments can already be put in the dataframe with the questions. 
# Here we only look at 'value', 'min' and 'max', but columns for other default values van be added.

# Define dependencies:
# - To refer to the input of an earlier question, you use 'input$' plus the id of the question that you want to use the input of.
# - Min and max of slider is dependent on input of minimum and maximum question.
# - The default value of the slider is dependent on its range.

# UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Word Similarity"),
  
  textOutput(outputId = "current_count"),
  
  # This is the dynamic UI that is rendered in the server
  uiOutput("Question"),
  
  # Button for counting
  actionButton(inputId = 'counter', label = 'Next')
  
)

server <- function(input, output) {
  # Counter, updated when going to the next question
  reactiveVals <- reactiveValues(
    count = 1
  )
  
  output$Question <- renderUI({
    dynamicUi()
  })
  
  output$current_count <- renderText(str_c(as.character(reactiveVals$count), "/", as.character(nrow(questions) + 1)))
  
  dynamicUi <- reactive({
    while (reactiveVals$count <= nrow(questions)) {
      question <- questions[reactiveVals$count,]
      type <- question$type
      id <- question$id
      label <- question$label
      
      # This questionnaire only has 'numeric' and 'slider' questions, more options can be added.
      if (type == 'numeric') 
        return(
          list(
            numericInput(inputId = id, 
                         label = label, 
                         value = 0)
          )
        )
      
      if (type == 'text') 
        return(
          list(
            h4(label)
          )
        )
      
      if (type == 'slider')
        return(
          list(
            # 'value', 'min' and 'max' are strings in our dataframe. They have to be parsed as variables and be evaluated.
            sliderInput(inputId = id, 
                        label = label, 
                        min = 0, 
                        max = 10, 
                        value = 5,
                        step = 0.1)
          )
        )
    }
    
    if (reactiveVals$count == nrow(questions) + 1)
      saveData(cn, input)
    return(
        list(
          h3("Thank you very much for your participation. 
             You can now close the form or view and download the results."),
          downloadButton(outputId = "download", label = "Download csv"),
          tabsetPanel(type = "tabs",
                    tabPanel("Spreadsheet", dataTableOutput(outputId = "table")),
                    tabPanel("Frequency plots", plotOutput("graphs"))
        )
      )
    )
  })
  
  output$table <- renderDataTable(
    loadData()
  )
  
  output$graphs <- renderPlot({
    loadData() %>%
    gather(key = word_pair, value = similarity) %>%
    ggplot(aes(similarity)) +
      geom_bar() +
      facet_wrap(~word_pair)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste(format(Sys.time(), "%Y%m%d-%H%M%OS"), "_results.csv")
    },
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE)
    }
  )
  
  
  observeEvent(input$counter, 
               reactiveVals$count <- reactiveVals$count + 1
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

