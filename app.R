library(shiny)
library(dplyr)
library(purrr)
library(stringr)
library(seqinr)
library(data.table)

#################################################### 
# Функция по преобразованию fasta-файла в дата-фрейм
####################################################
fasta_to_df <- function(fasta_path, fasta_name) {
  # fasta_data - путь к fasta-файлу  
  # fasta_name - название файла  
  
  # считываем fasta-файлу в лист
  fasta_data <- read.fasta(
    file = fasta_path,
    seqtype = "DNA",
    # whole.header = T,
    set.attributes = F,
    as.string = T
  )
  
  # собираем датафрейм из названия файла, имени последовательности и самой последовательности
  fasta_df <- tibble(file_name = fasta_name,    # столбец с названием файла
                     name = names(fasta_data),  # столбец с названием последовательности
                     dna = unlist(fasta_data)   # столбец с самой последовательностью
  )
  
  # возвращаем датафрейм из функции
  return(fasta_df)
}


##############################
# UI
##############################

ui <- fluidPage(
  titlePanel("Чтение fasta файлов"),
  sidebarLayout(
    sidebarPanel(
      # форма ввода пути файла(ов) для обработки
      fileInput(inputId = "file_fasta",
                label = "Выберете файл(ы) для обработки:",
                multiple = T, 
                # accept = c("fasta$"),
                buttonLabel = "Открыть...",
                placeholder = "Файл не выбран"),
      # кнопка скачивания
      downloadButton(outputId = "downloadData", 
                     label = "Скачать (.csv)")
    ),
    mainPanel(
      h3("Загруженные данные:"),
      tableOutput(outputId = "pivot")
    )
  )
)


server <- function(input, output) {
  # чудесная опция, позволяющая подгружать в shiny файла размером больше 5 мегабайт
  # в данном случае размер увеличен до 90 мегабайт
  options(shiny.maxRequestSize = 90*1024^2)
  
  # формируем дата-фрейм
  df_fasta <- eventReactive(input$file_fasta, {
    map_df(
      input$file_fasta$datapath, 
      fasta_to_df, 
      input$file_fasta$name
    )
  })

  # небольшой свод для вывода на экран
  output$pivot <- renderTable(
    {df_fasta()},
    striped = TRUE,
    rownames = TRUE,
    digits = 0
  )
  
  # сохраняем файл
  output$downloadData <- downloadHandler(
    filename = "fasta_file.csv",
    # filename = function() {
    #   paste("fasta_file_", date_save(), ".csv", sep = "")
    # },
    content = function(file) {
      fwrite(df_fasta(), file, quote = F, na = "", row.names = F, sep = ";")
    }
  )
}

shinyApp(ui = ui, server = server)