# Install and load required packages
if (!require(shiny)) install.packages("shiny", repos = "https://cran.rstudio.com/")
if (!require(DT)) install.packages("DT", repos = "https://cran.rstudio.com/")
if (!require(dplyr)) install.packages("dplyr", repos = "https://cran.rstudio.com/")
if (!require(shinythemes)) install.packages("shinythemes", repos = "https://cran.rstudio.com/")
if (!require(shinyWidgets)) install.packages("shinyWidgets", repos = "https://cran.rstudio.com/")

library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(shinyWidgets)

# Load the EnhancedPrediction data
EnhancedPrediction <- read.csv("EnhancedPrediction.csv", stringsAsFactors = FALSE)

# Round 'Batting_Avg' and 'Slugging_Pct' columns to three decimal places
EnhancedPrediction$Batting_Avg <- round(EnhancedPrediction$Batting_Avg, 3)
EnhancedPrediction$Slugging_Pct <- round(EnhancedPrediction$Slugging_Pct, 3)

# Get unique pitch types
pitch_types <- c("All", unique(EnhancedPrediction$pitch_type))


ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Helvetica+Neue&display=swap');
      body, .form-control, .selectize-input, .btn, table.dataTable, .dataTables_info, .paginate_button { 
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif !important; 
      }
      body { background-color: #f5f5f5; color: #333; }
      .container-fluid { max-width: 1200px; margin: 0 auto; }
      .navbar { background-color: #3474A2; border: none; }
      .navbar-default .navbar-brand { color: #ffffff; font-weight: bold; }
      .well { background-color: #ffffff; border: none; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .form-control, .selectize-input, .shiny-input-container input { 
        background-color: #ffffff !important; 
        border: 1px solid #cccccc !important; 
        border-radius: 4px !important;
        color: #333 !important;
      }
      .selectize-input { background-color: #ffffff !important; }
      .selectize-dropdown, .selectize-dropdown.form-control { background-color: #ffffff; }
      .selectize-dropdown-content { background-color: #ffffff; }
      .selectize-dropdown .active { background-color: #f0f0f0; }
      table.dataTable { 
        background-color: #ffffff;
        border: none;
        border-radius: 8px;
        overflow: hidden;
      }
      .dataTables_wrapper { 
        background-color: #ffffff; 
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .btn-default { background-color: #ffffff; color: #333; border: 1px solid #cccccc; }
      .btn-default:hover, .btn-default:focus { background-color: #f0f0f0; color: #333; }
      h4 { color: #3474A2; font-weight: bold; }
      .documentation-box {
        background-color: #ffffff;
        border: 1px solid #ddd;
        border-radius: 8px;
        padding: 20px;
        margin-top: 20px;
      }
      .documentation-box h5 {
        color: #3474A2;
        margin-bottom: 5px;
      }
      .documentation-box p {
        margin-bottom: 15px;
      }
      .filter-box {
        background-color: #ffffff;
        border: 1px solid #ddd;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 20px;
      }
      .range-input { display: flex; justify-content: space-between; }
      .range-input .form-group { width: 48%; }
      .model-doc img {
        max-width: 100%;
        height: auto;
        margin-bottom: 20px;
      }
      .graph-container {
        display: flex;
        flex-wrap: wrap;
        justify-content: space-between;
      }
      .graph-item {
        width: 48%;
        margin-bottom: 20px;
      }
      .graph-item img {
        width: 100%;
        height: auto;
        object-fit: contain;
      }
      .model-performance-box {
        background-color: #ffffff;
        border: 1px solid #ddd;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 20px;
      }
    "))
  ),
  navbarPage(
    "MLB STUFF PLUS",
    tabPanel("Results",
             fluidRow(
               column(12,
                      div(class = "model-performance-box",
                          h4("Model Performance and Stuff+ Metric Accuracy"),
                          p("Our analysis shows that the accuracy of the Stuff+ metric improves as the number of pitches increases. Here's a breakdown of how the model performs at different pitch count thresholds:"),
                          tags$ul(
                            tags$li("At 0 pitches (including all data), the correlation between predicted and actual outcomes is 0.829."),
                            tags$li("The correlation steadily improves as we increase the pitch count threshold."),
                            tags$li("At 50 pitches, we see the highest correlation of 0.944, which is our optimal threshold.")
                          ),
                          p("When looking at specific pitch count ranges:"),
                          tags$ul(
                            tags$li("For 0-15 pitches, the correlation is 0.796"),
                            tags$li("For 15-30 pitches, it improves to 0.847"),
                            tags$li("For 30-45 pitches, it reaches 0.868"),
                            tags$li("For 45-60 pitches, we see a strong correlation of 0.894")
                          ),
                          p("Based on these results, we can conclude that the Stuff+ metric is most accurate when evaluating pitchers with at least 50 pitches of a given type. However, even with lower pitch counts, the metric still provides valuable insights, albeit with slightly less reliability.")
                      )
               )
             ),
             fluidRow(
               column(12,
                      div(class = "filter-box",
                          fluidRow(
                            column(4, pickerInput("pitch_type", "Select Pitch Type:", choices = pitch_types, selected = "All", options = list(`live-search` = TRUE))),
                            column(4, numericInput("pitch_count_min", "Minimum Pitch Count:", value = NULL)),
                            column(4, textInput("player_name", "Search By Last Name, First Name:", ""))
                          ),
                          fluidRow(
                            column(4, div(class = "range-input",
                                          numericInput("velocity_min", "Velocity Min:", value = NULL),
                                          numericInput("velocity_max", "Velocity Max:", value = NULL)
                            )),
                            column(4, div(class = "range-input",
                                          numericInput("horizontal_movement_min", "Horizontal Movement Min:", value = NULL),
                                          numericInput("horizontal_movement_max", "Horizontal Movement Max:", value = NULL)
                            )),
                            column(4, div(class = "range-input",
                                          numericInput("vertical_movement_min", "Vertical Movement Min:", value = NULL),
                                          numericInput("vertical_movement_max", "Vertical Movement Max:", value = NULL)
                            ))
                          ),
                          fluidRow(
                            column(4, div(class = "range-input",
                                          numericInput("horizontal_release_min", "Horizontal Release Min:", value = NULL),
                                          numericInput("horizontal_release_max", "Horizontal Release Max:", value = NULL)
                            )),
                            column(4, div(class = "range-input",
                                          numericInput("vertical_release_min", "Vertical Release Min:", value = NULL),
                                          numericInput("vertical_release_max", "Vertical Release Max:", value = NULL)
                            )),
                            column(4, div(class = "range-input",
                                          numericInput("extension_min", "Extension Min:", value = NULL),
                                          numericInput("extension_max", "Extension Max:", value = NULL)
                            ))
                          )
                      )
               )
             ),
             fluidRow(
               column(12,
                      DTOutput("results_table"),
                      div(class = "documentation-box",
                          h4("Data Field Descriptions"),
                          h5("Pitch Type:"),
                          p("The type of pitch derived from Statcast."),
                          h5("Pitch Count:"),
                          p("The number of pitches thrown that matched the pitch type and resulted in either a foul, ball hit into play, or a swinging strike."),
                          h5("Velocity:"),
                          p("The average pitch velocity, adjusted to roughly the out-of-hand release point. All velocities from 2017 and beyond are reported by Statcast, reflecting the out-of-hand speed."),
                          h5("Horizontal Release:"),
                          p("The average horizontal release position of the ball, measured in feet from the catcher's perspective."),
                          h5("Vertical Release:"),
                          p("The average vertical release position of the ball, measured in feet from the catcher's perspective."),
                          h5("Horizontal Movement:"),
                          p("The average horizontal movement of the pitch, measured in feet from the catcher's perspective."),
                          h5("Vertical Movement:"),
                          p("The average vertical movement of the pitch, measured in feet from the catcher's perspective."),
                          h5("Release Spin:"),
                          p("The average spin rate of the pitch, as tracked by Statcast."),
                          h5("Extension:"),
                          p("The average release extension of the pitch, measured in feet, as tracked by Statcast."),
                          h5("Arm Slot:"),
                          p("The average angle of the pitcher's arm relative to the ground at ball release, calculated using release point coordinates and extension. Lower angles indicate a high arm slot, while higher angles indicate a lower arm slot. This is not a true arm slot in relation to the pitcher's shoulder."),
                          h5("Exit Velocity (Opposing):"),
                          p("The average launch speed of the ball after contact with the pitch, as recorded by Statcast."),
                          h5("Whiff Rate:"),
                          p("The percentage of swings and misses that the pitch generates."),
                          h5("Batting Average (Opposing):"),
                          p("The opposing batting average against the pitch, calculated by dividing the total number of hits by the pitch count. This metric is lower than expected because it considers every pitch, not just those resulting in the end of an at-bat. An influx of swings and misses early in the count will lead to a much lower batting average than expected."),
                          h5("Batting Average (Standard):"),
                          p("The standard batting average against the pitch. Like the opposing batting average, the slugging percentage will also be lower than expected because every pitch is considered, not just those that end an at-bat. This means an influx of swings and misses early in the count will result in a lower-than-expected slugging percentage.")
                      )
               )
             )
    ),
    tabPanel("Model Documentation",
             fluidRow(
               column(12,
                      div(class = "documentation-box model-doc",
                          h2("Explanation of Stuff+ Model for Pitch Quality Evaluation"),
                          h3("GitHub"),
                          p("Code for the xgboost model: ", 
                            tags$a(href = "https://github.com/aasmith1788/StuffPlussProject-ShinyApp/blob/main/StuffPlusModelR.R", 
                                   "StuffPlusModelR.R", 
                                   target = "_blank")
                          ),
                          p("Code for the Shiny App: ", 
                            tags$a(href = "https://github.com/aasmith1788/StuffPlussProject-ShinyApp/blob/main/app.R", 
                                   "app.R", 
                                   target = "_blank")
                          ),
                          h3("Introduction"),
                          p("This project develops a comprehensive model for evaluating pitch quality in baseball using a metric called \"Stuff+\". The model aims to quantify the effectiveness of pitches based on various physical characteristics, providing a nuanced understanding of pitcher performance that goes beyond traditional statistics."),
                          
                          h3("Detailed Methodology"),
                          h4("Data Preparation and Feature Engineering"),
                          h5("Dataset Usage:"),
                          p("StuffPlusTrain: Pitch by pitch data from 2020, 2021, 2022, and 2023. Used for model development"),
                          p("StuffPlusTest: Pitch by pitch data from 2024. Used for model evaluation and final analysis"),
                          h5("Advanced Feature Engineering:"),
                          p("Calculated release_magnitude using 3D coordinates (release_pos_x, release_extension, release_pos_z)."),
                          p("Computed arm_angle and arm_slot to capture the pitcher's release point in a more interpretable format."),
                          p("Developed an Effective Exit Velocity (EEV) metric:"),
                          tags$ul(
                            tags$li("EEV = launch_speed - (28 - launch_angle) if launch_angle < 28"),
                            tags$li("EEV = launch_speed - 3 * (launch_angle - 28) if launch_angle >= 28"),
                            p("This EEV metric aims to balance the effects of exit velocity and launch angle on pitch outcome.")
                          ),
                          p("Final Feature Set: release_speed, pfx_x, pfx_z, release_spin_rate, release_extension, release_pos_z, release_pos_x, arm_slot, EEV"),
                          
                          h3("Model Development"),
                          h5("Model Choice: XGBoost"),
                          p("Selected for its ability to handle complex non-linear relationships and its interpretability. Well-suited for capturing the intricate interactions between pitch characteristics."),
                          h5("Hyperparameter Tuning:"),
                          p("Implemented random search with cross-validation to optimize model performance."),
                          p("Tuned parameters: number of trees, maximum tree depth, learning rate, L1 regularization term, subsampling ratio, column sampling ratio."),
                          p("Objective: Maximize correlation between predicted and actual EEV values."),
                          h5("Model Training:"),
                          p("Trained on pitch-by-pitch data to capture the nuanced relationships between pitch characteristics and outcomes."),
                          p("Used EEV as the target variable, allowing the model to learn the complex interactions that contribute to pitch effectiveness."),
                          p("EEV was chosen over Run Value as the target variable due to several key advantages:"),
                          p("1. Independence from External Factors: Run Value can be influenced by external variables that are beyond the pitcher's control, such as weather conditions, fielding quality, and lucky bounces. These factors can skew the evaluation of pitch effectiveness by introducing noise into the data. In contrast, Effective Exit Velocity (EEV) is a raw measure that purely reflects the quality of contact made between the bat and the ball. By focusing on EEV, the model evaluates the pitch itself without being affected by these external forces."),
                          p("2. Purity of Measurement: Run Value incorporates the outcome of the play, which can be heavily dependent on what happens after the ball is in play (e.g., whether a fielder makes a good or bad play). EEV, on the other hand, directly measures the speed and angle at which the ball leaves the bat. This makes it a more direct and reliable indicator of how effective a pitch was in inducing weak contact or limiting hard contact. By using EEV, the model can capture the intrinsic qualities of the pitch, such as its velocity, movement, and spin, which are the factors directly controlled by the pitcher."),
                          p("3. Consistency and Reliability: EEV provides a more consistent and reliable metric for evaluating pitch effectiveness because it is not influenced by the randomness inherent in the game of baseball. By removing the noise associated with play outcomes and focusing on the quality of contact, the model can better generalize to different contexts and provide more stable predictions."),
                          p("4. Improved Predictive Power: Using EEV as the target variable allows the model to learn from more stable and less noisy data. This enhances the model's ability to accurately predict pitch effectiveness in future situations, as it is not confounded by external factors that could introduce variance in the results."),
                          p("In summary, EEV was selected as the target variable because it offers a more direct, consistent, and unbiased measure of pitch effectiveness, allowing the model to better understand the complex interactions that contribute to a pitcher's success."),
                          h3("Evaluation Approach"),
                          h5("Rationale for Averaging:"),
                          p("The xgboost model maximize correlation between predicted and actual EEV values. After prediction, results were averaged by pitcher and pitch type. Correlations between the average values of xEEV and EEV were chosen as the primary evaluation metric."),
                          p("By averaging xEEV and EEV for each pitcher-pitch type combination, the overall performance of that pitch type was captured. Instead of focusing on individual pitches, this approach averages the effectiveness over many pitches, giving a better representation of how that pitch type performs overall."),
                          p("Pitch-by-pitch predictions can be noisy due to the inherent variability in baseball. Aggregating by pitcher and pitch type provides a more stable and meaningful assessment of a pitcher's overall 'stuff'. This approach allows for easier interpretation and application in player evaluation and strategy development."),
                          p("Through a comprehensive analysis of various pitch count thresholds, it became evident that the correlation between Expected Exit Velocity (xEEV) and Effective Exit Velocity (EEV) consistently improved as the minimum number of pitches per group increased. Starting with a threshold of 0 pitches, the correlation was already reasonably strong at 0.829. However, as the threshold was gradually raised, the correlation strengthened significantly, reaching 0.944 at 50 pitches. This pattern suggests that larger sample sizes enhance the reliability and precision of the relationship between xEEV and EEV. Setting a minimum threshold filters the data to include only those pitcher-pitch type combinations that meet or exceed the specified pitch count. This method progressively excludes combinations with fewer pitches, ensuring that the analysis focuses on larger sample sizes, which typically yield more stable and accurate correlations."),
                          p("Similarly, when evaluating ranges of pitch counts, a clear trend emerged. For instance, in the range of 0-15 pitches, the correlation was 0.796. As the range shifted to 45-60 pitches, the correlation rose to 0.894. This further reinforces the idea that higher pitch counts yield more accurate assessments of pitch effectiveness. Using ranges, on the other hand, allows for an exploration of how performance metrics vary across different segments of pitch counts. This approach provides insights into how correlations behave at different stages, offering a more nuanced understanding of the data across specific intervals, rather than progressively filtering it as in the minimum threshold approach."),
                          p("Based on this analysis, a pitch count threshold of 50 pitches was identified as the optimal point where the correlation between xEEV and EEV is strongest. This threshold will be utilized when applying the stuff+ metric to evaluate a pitcher's arsenal, ensuring that the evaluation is both robust and dependable. While setting a minimum threshold offers a clear baseline for analysis, understanding the behavior of correlations within specific pitch count ranges adds valuable context, making the evaluation more comprehensive."),
                          p("In practical terms, this means that when evaluating a pitcher's arsenal, it is crucial to ensure that each pitch type has accumulated enough data (at least 50 pitches) to provide a reliable assessment. By doing so, coaches and analysts can make more informed decisions about a pitcher's strengths and weaknesses. Additionally, this model can be applied during bullpens to predict pitch outcomes in real-time. By inputting data from a bullpen session, the model can provide immediate feedback on how a pitch is expected to perform in a game scenario, based on the xEEV-EEV relationship. This allows pitchers to adjust their mechanics or pitch selection on the fly, optimizing their performance before they step into a live game."),
                          h3("Stuff+ Metric Explanation"),
                          h5("Nature of Stuff+:"),
                          p("Stuff+ is a composite metric derived from the model's predictions. It represents the overall quality of a pitch or pitch type, taking into account various physical characteristics."),
                          h5("Calculation:"),
                          p("The model predicts the Expected Effective Exit Velocity (xEEV) for each pitch. These predictions are then averaged by pitcher and pitch type."),
                          p("The averaged xEEV is then scaled and centered to create the Stuff+ metric:"),
                          p("Stuff+ = 100 + (league_average_xEEV - pitcher_xEEV) / league_standard_deviation_xEEV * 15"),
                          p("This scaling ensures that the average Stuff+ across all pitches is 100, with higher values indicating better 'stuff'."),
                          h5("Interpretation:"),
                          p("A Stuff+ value of 100 represents average pitch quality. Values above 100 indicate above-average pitch quality, while values below 100 indicate below-average pitch quality."),
                          
                          h3("Model Application"),
                          h5("Single Model Approach:"),
                          p("One unified model was developed to evaluate all pitch types. This approach allows the model to learn the relative effectiveness of different pitch characteristics across all types of pitches."),
                          h5("Pitch Type Differentiation:"),
                          p("While a single model is used, the inclusion of pitch type as a feature allows the model to capture type-specific relationships. The aggregation by pitch type in the final analysis ensures that each pitch type is evaluated separately, providing insights into a pitcher's effectiveness with different pitches."),
                          h5("Evaluation Process:"),
                          p("The model makes predictions on a pitch-by-pitch basis. These predictions are then averaged to provide Stuff+ values for each pitcher-pitch type combination. This allows for both detailed analysis of individual pitches and broader evaluation of a pitcher's arsenal."),
                          
                          h3("Results and Insights"),
                          h5("Model Performance:"),
                          p("The final correlation between predicted and actual Effective Exit Velocity (EEV) after aggregation with a minimum pitch count of 50 is 0.94, demonstrating a strong relationship between the model's predictions and the actual outcomes. Moreover, the Mean Squared Error (MSE) of 987.89, consistent across both the training and test sets, indicates that the model generalizes well, minimizing the risk of overfitting. This suggests that the model is robust and can be relied upon for making predictions across different data samples."),
                          p("When comparing the original test MSE of 987.89 with the slightly higher MSE of 988.31 under high regularization, we observe that the increase in regularization did not significantly degrade the model's performance. This indicates that the model is relatively stable and resilient to adjustments in regularization parameters, further reinforcing its reliability."),
                          p("The cross-validation results provide additional insights into the model's performance across multiple folds. The test RMSE steadily decreased across iterations, reaching a minimum at iteration 8 with a test RMSE of 47.03. This early stopping point reflects the optimal balance between training and validation performance, ensuring that the model does not overfit to the training data. The minimal variance between the train and test RMSE values throughout cross-validation further supports the model's ability to generalize effectively."),
                          
                          h5("Feature Importance:"),
                          tags$ul(
                            tags$li("Arm slot (21.29%)"),
                            tags$li("Release speed (15.48%)"),
                            tags$li("Horizontal release position (15.41%)"),
                            tags$li("Horizontal movement (14.59%)"),
                            tags$li("Vertical movement (14.35%)"),
                            tags$li("Vertical release position (12.78%)"),
                            tags$li("Extension (0.06%)")
                          ),
                          p("This hierarchy provides insights into which physical characteristics most influence pitch quality."),
                          
                          h3("Correlation Analysis"),
                          h5("Exit Velocity:"),
                          p("Strong negative correlation (-0.893, R-squared: 0.797)."),
                          h5("Whiff Rate:"),
                          p("Strong positive correlation (0.864, R-squared: 0.746)."),
                          h5("Batting Average:"),
                          p("Moderate negative correlation (-0.591, R-squared: 0.349)."),
                          h5("Slugging Percentage:"),
                          p("Moderate negative correlation (-0.489, R-squared: 0.239)."),
                          p("These correlations validate the Stuff+ metric by showing its strong relationships with key performance indicators."),
                        
                              
                              
                          h3("Applications in Bullpen Evaluation and Pitcher Development"),
                          h5("Pitch Type Optimization:"),
                          p("Pitchers can experiment with different pitch types and variations during bullpen sessions. The Stuff+ model can help identify which pitch types are most effective for each individual pitcher, potentially uncovering hidden strengths or areas for improvement in their repertoire."),
                          h5("Refinement of Pitch Characteristics:"),
                          p("Given the model's insights into feature importance, pitchers can focus on optimizing key characteristics:"),
                          tags$ul(
                            tags$li("Arm slot: Experimenting with slight adjustments to find the most effective release point."),
                            tags$li("Velocity: Velocity is not everything, but often times it can be the determining factor."),
                            tags$li("Movement: Adjusting grip or mechanics to enhance horizontal and vertical movement.")
                          ),
                          
                
                          p("Over multiple bullpen sessions, pitchers can track their Stuff+ scores to monitor improvement. This data-driven approach allows for objective measurement of progress, complementing subjective assessments from coaches."),
                          h5("MLB-Level Benchmarking:"),
                          p("Since the model is trained on MLB data, it provides a benchmark for how a pitch might perform at the highest level. This is particularly valuable for minor league pitchers or prospects, offering insights into how their current 'stuff' compares to MLB standards."),
                          h5("Customized Development Plans:"),
                          p("Based on Stuff+ evaluations, coaches and players can create personalized development plans. These plans can focus on enhancing strengths or addressing weaknesses identified by the model."),
                          
                          h3("Conclusion"),
                          p("The Stuff+ model provides a comprehensive and data-driven approach to evaluating pitch quality in baseball. By leveraging advanced statistical techniques and a rich set of pitch characteristics, it offers valuable insights for player development, scouting, and in-game strategy. As with any model, it's important to use Stuff+ in conjunction with other evaluation methods and expert knowledge for a holistic understanding of pitcher performance.")
                      )
               )
             )
    )
  )
)


                          
                          
# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    data <- EnhancedPrediction
    
    if (input$pitch_type != "All") {
      data <- data %>% filter(pitch_type == input$pitch_type)
    }
    
    if (!is.null(input$pitch_count_min) && !is.na(input$pitch_count_min)) {
      data <- data %>% filter(Pitch_Count >= input$pitch_count_min)
    }
    
    if (input$player_name != "") {
      data <- data %>% filter(grepl(input$player_name, player_name, ignore.case = TRUE))
    }
    
    if (!is.null(input$velocity_min) && !is.na(input$velocity_min)) {
      data <- data %>% filter(Velocity >= input$velocity_min)
    }
    if (!is.null(input$velocity_max) && !is.na(input$velocity_max)) {
      data <- data %>% filter(Velocity <= input$velocity_max)
    }
    
    if (!is.null(input$horizontal_movement_min) && !is.na(input$horizontal_movement_min)) {
      data <- data %>% filter(Horizantle_Movement >= input$horizontal_movement_min)
    }
    if (!is.null(input$horizontal_movement_max) && !is.na(input$horizontal_movement_max)) {
      data <- data %>% filter(Horizantle_Movement <= input$horizontal_movement_max)
    }
    
    if (!is.null(input$vertical_movement_min) && !is.na(input$vertical_movement_min)) {
      data <- data %>% filter(Vertical_Movement >= input$vertical_movement_min)
    }
    if (!is.null(input$vertical_movement_max) && !is.na(input$vertical_movement_max)) {
      data <- data %>% filter(Vertical_Movement <= input$vertical_movement_max)
    }
    
    if (!is.null(input$horizontal_release_min) && !is.na(input$horizontal_release_min)) {
      data <- data %>% filter(Horizantle_Release >= input$horizontal_release_min)
    }
    if (!is.null(input$horizontal_release_max) && !is.na(input$horizontal_release_max)) {
      data <- data %>% filter(Horizantle_Release <= input$horizontal_release_max)
    }
    
    if (!is.null(input$vertical_release_min) && !is.na(input$vertical_release_min)) {
      data <- data %>% filter(Verticle_Release >= input$vertical_release_min)
    }
    if (!is.null(input$vertical_release_max) && !is.na(input$vertical_release_max)) {
      data <- data %>% filter(Verticle_Release <= input$vertical_release_max)
    }
    
    if (!is.null(input$extension_min) && !is.na(input$extension_min)) {
      data <- data %>% filter(Extension >= input$extension_min)
    }
    if (!is.null(input$extension_max) && !is.na(input$extension_max)) {
      data <- data %>% filter(Extension <= input$extension_max)
    }
    
    data %>% arrange(desc(Stuff.))
  })
  
  # Render the datatable
  output$results_table <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 15, 
                             scrollX = TRUE,
                             dom = 'rtipB',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
              rownames = FALSE,
              class = 'cell-border stripe hover') %>%
      formatStyle(columns = names(filtered_data()),
                  backgroundColor = 'white',
                  color = '#333',
                  fontWeight = 'normal') %>%
      formatRound(columns = c("Stuff.", "Velocity", "Horizantle_Movement", "Vertical_Movement",
                              "Horizantle_Release", "Verticle_Release", "Extension", "Spin_Rate",
                              "Arm_Slot", "Exit_Velo", "Whiff_Rate"), digits = 2)
  })
  
  # Render images for Model Documentation tab
  output$batting_avg_plot <- renderImage({
    list(src = "Stuff+_vs_Batting_Avg_50plus_EnhancedTufte.png",
         width = "100%",
         height = "auto")
  }, deleteFile = FALSE)
  
  output$exit_velo_plot <- renderImage({
    list(src = "Stuff+_vs_Exit_Velo_50plus_EnhancedTufte.png",
         width = "100%",
         height = "auto")
  }, deleteFile = FALSE)
  
  output$slugging_pct_plot <- renderImage({
    list(src = "Stuff+_vs_Slugging_Pct_50plus_EnhancedTufte.png",
         width = "100%",
         height = "auto")
  }, deleteFile = FALSE)
  
  output$whiff_rate_plot <- renderImage({
    list(src = "Stuff+_vs_Whiff_Rate_50plus_EnhancedTufte.png",
         width = "100%",
         height = "auto")
  }, deleteFile = FALSE)
}







# Run the application 
shinyApp(ui = ui, server = server)
app <- shinyApp(ui = ui, server = server)
app  # Explicitly return the app object


