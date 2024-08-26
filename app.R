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

# Define pitch types with full names
pitch_types <- list(
  "All" = "All",
  "Fastballs" = "Fastballs",
  "Offspeed" = "Offspeed",
  "Curveballs" = "Curveballs",
  "Sliders" = "Sliders",
  "Fastball" = list(
    "FF - Fastball (4-seam)" = "FF",
    "SI - Sinker (2-Seam)" = "SI",
    "FC - Cutter" = "FC"
  ),
  "Offspeed" = list(
    "CH - Changeup" = "CH",
    "FS - Split-finger" = "FS",
    "FO - Forkball" = "FO",
    "SC - Screwball" = "SC"
  ),
  "Curveball Group" = list(
    "CU - Curveball" = "CU",
    "KC - Knuckle Curve" = "KC",
    "CS - Slow Curve" = "CS"
  ),
  "Slider Group" = list(
    "SL - Slider" = "SL",
    "ST - Sweeper" = "ST",
    "SV - Slurve" = "SV"
  ),
  "KN - Knuckleball" = "KN"
)

# Reorganize the EnhancedPrediction data frame
EnhancedPrediction <- EnhancedPrediction %>%
  select(
    player_name,
    pitch_type,
    pitch_usage,
    Pitch_Count_M,
    pitch,
    Stuff.,
    release_speed,
    pfx_z,
    pfx_x,
    release_spin_rate,
    release_pos_z,
    release_pos_x,
    release_extension,
    Arm_Slot,
    in_zone_rate,
    chase_rate,
    whiff_rate,
    xwoba,
    Batting_Avg,
    Exit_Velo
  ) %>%
  rename(
    "Player Name" = player_name,
    "Type" = pitch_type,
    "Usage" = pitch_usage,
    "MCount" = Pitch_Count_M,
    "Count" = pitch,
    "Stuff+" = Stuff.,
    "Velo" = release_speed,
    "iVB" = pfx_z,
    "HB" = pfx_x,
    "Spin Rate" = release_spin_rate,
    "vRel" = release_pos_z,
    "hRel" = release_pos_x,
    "Arm Slot" = Arm_Slot,
    "Ext" = release_extension,
    "Zone%" = in_zone_rate,
    "Chase%" = chase_rate,
    "Whiff%" = whiff_rate,
    "xwOBA" = xwoba,
    "Opp BA" = Batting_Avg,
    "Exit Velo" = Exit_Velo
  )

EnhancedPrediction <- EnhancedPrediction %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(`Spin Rate` = round(`Spin Rate`),
         MCount = round(MCount),
         `Stuff+` = round(`Stuff+`))

# Ensure that 'Stuff+', 'Spin Rate', and 'MCount' are read as whole numbers
EnhancedPrediction <- EnhancedPrediction %>%
  mutate(`Stuff+` = as.integer(`Stuff+`),
         `Spin Rate` = as.integer(`Spin Rate`),
         MCount = as.integer(MCount))

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Times+New+Roman&display=swap');
      body, .form-control, .selectize-input, .btn, .dataTables_info, .paginate_button { 
        font-family: 'Times New Roman', Times, serif !important; 
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
        font-size: 11px;
      }
      .dataTables_wrapper { 
        background-color: #ffffff; 
        padding: 10px;
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
      /* Updated styles for smaller data cells */
      table.dataTable thead th, table.dataTable tbody td {
        font-size: 11px !important;
        padding: 5px 8px !important;
      }
      table.dataTable thead th {
        font-weight: bold;
      }
    "))
  ),
  tags$script('
    $(document).on("shiny:connected", function(e) {
      var setTableHeight = function() {
        var windowHeight = $(window).height();
        var tableHeight = windowHeight - 200; // Adjust this offset as needed
        $(".dataTables_scrollBody").css("height", tableHeight + "px");
      };
      setTableHeight();
      $(window).resize(setTableHeight);
    });
  '),
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
                          h5("Type:"),
                          p("The type of pitch derived from Statcast."),
                          h5("Usage:"),
                          p("The frequency or percentage of times a pitcher throws a specific type of pitch."),
                          h5("MCount:"),
                          p("The number of pitches thrown that resulted in a swinging strike, foul ball, or a ball hit-into-play."),
                          h5("Count:"),
                          p("The number of pitches thrown that matched the pitch type and resulted in either a foul, ball hit into play, or a swinging strike."),
                          h5("Velo:"),
                          p("The average pitch velocity, adjusted to roughly the out-of-hand release point. All velocities from 2017 and beyond are reported by Statcast, reflecting the out-of-hand speed."),
                          h5("hRel:"),
                          p("The average horizontal release position of the ball, measured in feet from the catcher's perspective."),
                          h5("vRel:"),
                          p("The average vertical release position of the ball, measured in feet from the catcher's perspective."),
                          h5("HB:"),
                          p("The average horizontal movement of the pitch, measured in feet from the catcher's perspective."),
                          h5("iVB:"),
                          p("The average vertical movement of the pitch, measured in feet from the catcher's perspective."),
                          h5("Spin Rate:"),
                          p("The average spin rate of the pitch, as tracked by Statcast."),
                          h5("Ext:"),
                          p("The average release extension of the pitch, measured in feet, as tracked by Statcast."),
                          h5("Arm Slot:"),
                          p("The average angle of the pitcher's arm relative to the ground at ball release, calculated using release point coordinates and extension. Lower angles indicate a high arm slot, while higher angles indicate a lower arm slot. This is not a true arm slot in relation to the pitcher's shoulder."),
                          h5("Exit Velocity (Opposing):"),
                          p("The average launch speed of the ball after contact with the pitch, as recorded by Statcast."),
                          h5("Whiff%:"),
                          p("The percentage of swings and misses that the pitch generates."),
                          h5("Opp BA:"),
                          p("The opposing batting average against the pitch, calculated by dividing the total number of hits by the pitch count. This metric is lower than expected because it considers every pitch, not just those resulting in the end of an at-bat. An influx of swings and misses early in the count will lead to a much lower batting average than expected."),
                          h5("Zone%"),
                          p("Percentage of pitches thrown in the strikezone."),
                          h5("Chase%"),
                          p("The percentage of pitches thrown outside the strike zone that batters attempted to swing at."),
                          h5("xwOBA"),
                          p("Average xwOBA is a statistic that estimates a player's expected on-base performance based on the quality of contact and, in some cases, strikeouts and walks."),
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
                      p("1. Independence from External Factors: Run Value can be influenced by external variables that are beyond the pitcher's control, such as weather conditions, fielding quality, and lucky bounces. These factors can skew the evaluation of pitch effectiveness by introducing noise into the data. In contrast, Effective Exit Velocity (EEV) is a raw measure that purely reflects the
                          purely reflects the quality of contact made between the bat and the ball."),
                      p("2. Purity of Measurement: Run Value incorporates the outcome of the play, which can be heavily dependent on what happens after the ball is in play (e.g., whether a fielder makes a good or bad play). EEV, on the other hand, directly measures the speed and angle at which the ball leaves the bat. This makes it a more direct and reliable indicator of how effective a pitch was in inducing weak contact or limiting hard contact."),
                      p("3. Consistency and Reliability: EEV provides a more consistent and reliable metric for evaluating pitch effectiveness because it is not influenced by the randomness inherent in the game of baseball. By removing the noise associated with play outcomes and focusing on the quality of contact, the model can better generalize to different contexts and provide more stable predictions."),
                      p("4. Improved Predictive Power: Using EEV as the target variable allows the model to learn from more stable and less noisy data. This enhances the model's ability to accurately predict pitch effectiveness in future situations, as it is not confounded by external factors that could introduce variance in the results."),
                      
                      h3("Evaluation Approach"),
                      h5("Rationale for Averaging:"),
                      p("The xgboost model maximize correlation between predicted and actual EEV values. After prediction, results were averaged by pitcher and pitch type. Correlations between the average values of xEEV and EEV were chosen as the primary evaluation metric."),
                      p("By averaging xEEV and EEV for each pitcher-pitch type combination, the overall performance of that pitch type was captured. Instead of focusing on individual pitches, this approach averages the effectiveness over many pitches, giving a better representation of how that pitch type performs overall."),
                      p("Pitch-by-pitch predictions can be noisy due to the inherent variability in baseball. Aggregating by pitcher and pitch type provides a more stable and meaningful assessment of a pitcher's overall 'stuff'. This approach allows for easier interpretation and application in player evaluation and strategy development."),
                      p("Through a comprehensive analysis of various pitch count thresholds, it became evident that the correlation between Expected Exit Velocity (xEEV) and Effective Exit Velocity (EEV) consistently improved as the minimum number of pitches per group increased. Starting with a threshold of 0 pitches, the correlation was already reasonably strong at 0.829. However, as the threshold was gradually raised, the correlation strengthened significantly, reaching 0.944 at 50 pitches. This pattern suggests that larger sample sizes enhance the reliability and precision of the relationship between xEEV and EEV."),
                      p("Similarly, when evaluating ranges of pitch counts, a clear trend emerged. For instance, in the range of 0-15 pitches, the correlation was 0.796. As the range shifted to 45-60 pitches, the correlation rose to 0.894. This further reinforces the idea that higher pitch counts yield more accurate assessments of pitch effectiveness."),
                      p("Based on this analysis, a pitch count threshold of 50 pitches was identified as the optimal point where the correlation between xEEV and EEV is strongest. This threshold will be utilized when applying the stuff+ metric to evaluate a pitcher's arsenal, ensuring that the evaluation is both robust and dependable."),
                      
                      h3("Results and Insights"),
                      h5("Model Performance:"),
                      p("The final correlation between predicted and actual Effective Exit Velocity (EEV) after aggregation with a minimum pitch count of 50 is 0.94, demonstrating a strong relationship between the model's predictions and the actual outcomes. Moreover, the Mean Squared Error (MSE) of 987.89, consistent across both the training and test sets, indicates that the model generalizes well, minimizing the risk of overfitting."),
                      p("When comparing the original test MSE of 987.89 with the slightly higher MSE of 988.31 under high regularization, we observe that the increase in regularization did not significantly degrade the model's performance. This indicates that the model is relatively stable and resilient to adjustments in regularization parameters, further reinforcing its reliability."),
                      p("The cross-validation results provide additional insights into the model's performance across multiple folds. The test RMSE steadily decreased across iterations, reaching a minimum at iteration 8 with a test RMSE of 47.03. This early stopping point reflects the optimal balance between training and validation performance, ensuring that the model does not overfit to the training data."),
                      
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
                      h5("xwOBA:"),
                      p("Moderate negative correlation (-.621, R-squared: 0.349)."),
                      h5("Batting Average:"),
                      p("Moderate negative correlation (-0.591, R-squared: 0.349)."),
                      h5("Chase Rate:"),
                      p("Moderate negative correlation (-0.49, R-squared: 0.24)."),
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

server <- function(input, output, session) {
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    data <- EnhancedPrediction
    
    if (input$pitch_type != "All") {
      if (input$pitch_type == "Fastballs") {
        data <- data %>% filter(`Type` %in% c("FF", "SI", "FC"))
      } else if (input$pitch_type == "Offspeed") {
        data <- data %>% filter(`Type` %in% c("CH", "FS", "FO", "SC"))
      } else if (input$pitch_type == "Curveballs") {
        data <- data %>% filter(`Type` %in% c("CU", "KC", "CS"))
      } else if (input$pitch_type == "Sliders") {
        data <- data %>% filter(`Type` %in% c("SL", "ST", "SV"))
      } else {
        data <- data %>% filter(`Type` == input$pitch_type)
      }
    }
    
    if (!is.null(input$pitch_count_min) && !is.na(input$pitch_count_min)) {
      data <- data %>% filter(`MCount` >= input$pitch_count_min)
    }
    
    if (!is.null(input$velocity_min) && !is.na(input$velocity_min)) {
      data <- data %>% filter(Velo >= input$velocity_min)
    }
    if (!is.null(input$velocity_max) && !is.na(input$velocity_max)) {
      data <- data %>% filter(Velo <= input$velocity_max)
    }
    
    if (!is.null(input$horizontal_movement_min) && !is.na(input$horizontal_movement_min)) {
      data <- data %>% filter(HB >= input$horizontal_movement_min)
    }
    if (!is.null(input$horizontal_movement_max) && !is.na(input$horizontal_movement_max)) {
      data <- data %>% filter(HB <= input$horizontal_movement_max)
    }
    
    if (!is.null(input$vertical_movement_min) && !is.na(input$vertical_movement_min)) {
      data <- data %>% filter(iVB >= input$vertical_movement_min)
    }
    if (!is.null(input$vertical_movement_max) && !is.na(input$vertical_movement_max)) {
      data <- data %>% filter(iVB <= input$vertical_movement_max)
    }
    
    if (!is.null(input$horizontal_release_min) && !is.na(input$horizontal_release_min)) {
      data <- data %>% filter(hRel >= input$horizontal_release_min)
    }
    if (!is.null(input$horizontal_release_max) && !is.na(input$horizontal_release_max)) {
      data <- data %>% filter(hRel <= input$horizontal_release_max)
    }
    
    if (!is.null(input$vertical_release_min) && !is.na(input$vertical_release_min)) {
      data <- data %>% filter(vRel >= input$vertical_release_min)
    }
    if (!is.null(input$vertical_release_max) && !is.na(input$vertical_release_max)) {
      data <- data %>% filter(vRel <= input$vertical_release_max)
    }
    
    if (!is.null(input$extension_min) && !is.na(input$extension_min)) {
      data <- data %>% filter(Ext >= input$extension_min)
    }
    if (!is.null(input$extension_max) && !is.na(input$extension_max)) {
      data <- data %>% filter(Ext <= input$extension_max)
    }
    
    data %>% 
      arrange(desc(`Stuff+`)) %>%
      select(!!!names(EnhancedPrediction))  # This preserves the original column order
  })
  
  # Render the datatable
  output$results_table <- renderDT({
    data <- filtered_data()
    
    # Apply player name search here
    if (input$player_name != "") {
      data <- data %>% filter(grepl(input$player_name, `Player Name`, ignore.case = TRUE))
    }
    
    datatable(data,
              options = list(
                pageLength = 15, 
                scrollX = TRUE,
                scrollY = "600px",  # Set a fixed height for the table body
                scrollCollapse = TRUE,  # Always show the vertical scrollbar
                dom = 'rtipB',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                autoWidth = FALSE,  # Disable automatic column width adjustments
                fixedColumns = TRUE,  # Ensure columns remain fixed
                columnDefs = list(
                  list(targets = "_all", className = "dt-center", width = "150px"),  # Set fixed width for all columns
                  list(targets = 0, width = "150px")  # Adjust the width for the Player Name column
                ),
                search = list(regex = TRUE, caseInsensitive = TRUE)
              ),
              rownames = FALSE,
              class = 'cell-border stripe hover',
              filter = 'none'  # Disable built-in filtering
    ) %>%
      formatStyle(columns = names(data),
                  backgroundColor = 'white',
                  color = '#333',
                  fontWeight = 'normal') %>%
      formatRound(columns = intersect(names(data), 
                                      c( "Velo", "HB", "iVB",
                                         "hRel", "vRel", "Ext",
                                         "Arm Slot", "Exit Velo", "Whiff%", "Zone%", "Chase%", 
                                         "xwOBA", "Opp BA", "Usage")), 
                  digits = 2)
  }, server = TRUE)  # Enable server-side processing
  
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






