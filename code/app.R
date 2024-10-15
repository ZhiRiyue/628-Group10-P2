library(shiny)
library(MASS)

bodyfat_tables <- list(
  "20-29" = data.frame(
    Category = c("Low (Increased Health Risk)", "Excellent/Fit (Healthy)", "Good/Normal (Healthy)", "Fair/Average (Healthy)", "Poor (Increased Health Risk)", "High (Increased Health Risk)"),
    Percentage = c("<8%", "<=10.5%", "10.6-14.8%", "14.9-18.6%", "18.7-23.1%", ">=23.2%")
  ),
  "30-39" = data.frame(
    Category = c("Low (Increased Health Risk)", "Excellent/Fit (Healthy)", "Good/Normal (Healthy)", "Fair/Average (Healthy)", "Poor (Increased Health Risk)", "High (Increased Health Risk)"),
    Percentage = c("<8%", "<=14.5%", "14.6-18.2%", "18.3-21.3%", "21.4-24.9%", ">=25%")
  ),
  "40-49" = data.frame(
    Category = c("Low (Increased Health Risk)", "Excellent/Fit (Healthy)", "Good/Normal (Healthy)", "Fair/Average (Healthy)", "Poor (Increased Health Risk)", "High (Increased Health Risk)"),
    Percentage = c("<8%", "<=17.4%", "17.5-20.6%", "20.7-23.4%", "23.5-26.6%", ">=26.7%")
  ),
  "50-59" = data.frame(
    Category = c("Low (Increased Health Risk)", "Excellent/Fit (Healthy)", "Good/Normal (Healthy)", "Fair/Average (Healthy)", "Poor (Increased Health Risk)", "High (Increased Health Risk)"),
    Percentage = c("<8%", "<=19.1%", "19.2-22.1%", "22.2-24.6%", "24.7-27.8%", ">=27.9%")
  ),
  "60-69" = data.frame(
    Category = c("Low (Increased Health Risk)", "Excellent/Fit (Healthy)", "Good/Normal (Healthy)", "Fair/Average (Healthy)", "Poor (Increased Health Risk)", "High (Increased Health Risk)"),
    Percentage = c("<8%", "<=19.7%", "19.8-22.6%", "22.7-25.2%", "25.3-28.4%", ">=28.5%")
  )
)

ui <- fluidPage(
  tags$style(
    HTML("
      body { background-color: #FDF6E3; }
      h1 { text-align: center; margin-bottom: 19    px; color: #C68E3D; }
      h3 { text-align: center; font-size: 19px; color: #C68E3D; }
      .shiny-input-container { margin-bottom: 15px; color: #4F4F4F; }
      .sidebar { padding: 20px; background-color: #F3E5AB; border-radius: 8px; box-shadow: 0px 4px 12px rgba(0, 0, 0, 0.1); color: #4F4F4F; }
      .main { padding: 20px; background-color: #F3E5AB; border-radius: 8px; box-shadow: 0px 4px 12px rgba(0, 0, 0, 0.1); color: #4F4F4F; }  /* 主内容区域背景为米黄色，文字为深灰 */
      .container { display: flex; justify-content: center; }
      .btn { background-color: #D2691E; color: white; font-size: 16px; padding: 10px 20px; display: block; margin: 0 auto;}
      table { width: 100%; margin: 20px 0; border-collapse: collapse; color: #4F4F4F; }
      th { background-color: #C68E3D; color: white; padding: 10px; border-bottom: 2px solid #F3E5AB; }
      td { padding: 10px; border-bottom: 1px solid #F3E5AB; }
      #prediction { color: black; font-weight: bold; font-size: 18px; }
      .help-text { color: #4F4F4F; }
    ")
  ),
  
  
  titlePanel(h1("Body Fat Prediction Model — GROUP 10")),
  
  div(class = "container", 
      fluidRow(
        column(6,
               div(class = "sidebar",
                   h3("Input Parameters"),
                   numericInput("abdomen", "Abdomen Circumference (cm):", value = 100, min = 60, max = 150),
                   helpText(HTML("(Measure around your abdomen at navel level.)")),
                   numericInput("weight", "Weight (lbs):", value = 200, min = 50, max = 400),
                   helpText(HTML("(Kg to lbs: multiply kg by 2.20.)")),
                   actionButton("predict", "Predict Body Fat", class = "btn btn-primary btn-lg"),
                   h3("Predicted Body Fat Percentage"),
                   verbatimTextOutput("prediction")
               )
        ),
        
        column(6,
               div(class = "main",
                   h3("Select Age Group"),
                   selectInput("age_group", "", choices = c("20-29 years" = "20-29", "30-39 years" = "30-39", "40-49 years" = "40-49", "50-59 years" = "50-59", "60-69 years" = "60-69")),
                   
                   h3("BodyFat Percentage Table for Men"),
                   tableOutput("bodyfat_table")
               )
        ),
        
        fluidRow(
          column(12, 
                 br(), 
                 p("For any inquiries, contact us at: ",
                   a("zchen2353@wisc.edu, xdong95@wisc.edu, xtang254@wisc.edu, zwu535@wisc.edu", href="mailto:support@bodyfatapp.com"),
                   style = "text-align:center; color:#555;")
          )
        )
        
      )
  )
)

# Define server logic
server <- function(input, output) {
  
  # 样本均值与标准差
  mu_weight <- 177.69
  sigma_weight <- 26.48
  mu_abdomen <- 92.08
  sigma_abdomen <- 9.85
  mu_bodyfat <- 18.94
  sigma_bodyfat <- 7.75
  
  # 动态预测体脂百分比
  predict_bodyfat <- reactive({
    req(input$predict) # 等待用户点击“Predict”按钮
    
    # 输入合理性检查
    if (input$weight < 50 || input$weight > 400) {
      return("Error: Please enter a valid weight between 50 and 400 lbs.")
    }
    if (input$abdomen < 60 || input$abdomen > 150) {
      return("Error: Please enter a valid abdomen circumference between 60 and 150 cm.")
    }
    
    # 计算体脂百分比
    prediction <- ((1.18 * (input$abdomen - mu_abdomen) / sigma_abdomen) - 
                     (0.42 * (input$weight - mu_weight) / sigma_weight)) * sigma_bodyfat + mu_bodyfat
    
    # 预测结果合理性检查
    if (prediction < 0 || prediction > 100) {
      return("Error: The predicted body fat is invalid. Please check your input values.")
    }
    
    return(round(prediction, 2)) # 返回保留两位小数的预测结果
  })
  
  # 显示预测结果
  output$prediction <- renderText({
    paste(predict_bodyfat(), "%")
  })
  
  # 显示体脂百分比参考表格
  output$bodyfat_table <- renderTable({
    selected_age_group <- input$age_group
    bodyfat_tables[[selected_age_group]]
  })
  
  
}

# 启动应用
shinyApp(ui = ui, server = server)
