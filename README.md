# StuffPlussProject-ShinyApp

Explanation of Stuff+ Model for Pitch Quality Evaluation

aaronsmith1788.shinyapps.io/MLBStuffPlus/ 

Introduction

This project develops a comprehensive model for evaluating pitch quality in baseball using a metric called "Stuff+". The model aims to quantify the effectiveness of pitches based on various physical characteristics, providing a nuanced understanding of pitcher performance that goes beyond traditional statistics.

Detailed Methodology
Data Preparation and Feature Engineering
Dataset Usage:
StuffPlusTrain: Pitch by pitch data from 2020, 2021, 2022, and 2023. Used for model development

StuffPlusTest: Pitch by pitch data from 2024. Used for model evaluation and final analysis

Advanced Feature Engineering:
Calculated release_magnitude using 3D coordinates (release_pos_x, release_extension, release_pos_z).

Computed arm_angle and arm_slot to capture the pitcher's release point in a more interpretable format.

Developed an Effective Exit Velocity (EEV) metric:

EEV = launch_speed - (28 - launch_angle) if launch_angle < 28
EEV = launch_speed - 3 * (launch_angle - 28) if launch_angle >= 28
This EEV metric aims to balance the effects of exit velocity and launch angle on pitch outcome.

Final Feature Set: release_speed, pfx_x, pfx_z, release_spin_rate, release_extension, release_pos_z, release_pos_x, arm_slot, EEV

Model Development
Model Choice: XGBoost
Selected for its ability to handle complex non-linear relationships and its interpretability. Well-suited for capturing the intricate interactions between pitch characteristics.

Hyperparameter Tuning:
Implemented random search with cross-validation to optimize model performance.

Tuned parameters: number of trees, maximum tree depth, learning rate, L1 regularization term, subsampling ratio, column sampling ratio.

Objective: Maximize correlation between predicted and actual EEV values.

Model Training:
Trained on pitch-by-pitch data to capture the nuanced relationships between pitch characteristics and outcomes.

Used EEV as the target variable, allowing the model to learn the complex interactions that contribute to pitch effectiveness.

EEV was chosen over Run Value as the target variable due to several key advantages:

1. Independence from External Factors: Run Value can be influenced by external variables that are beyond the pitcher's control, such as weather conditions, fielding quality, and lucky bounces. These factors can skew the evaluation of pitch effectiveness by introducing noise into the data. In contrast, Effective Exit Velocity (EEV) is a raw measure that purely reflects the quality of contact made between the bat and the ball. By focusing on EEV, the model evaluates the pitch itself without being affected by these external forces.

2. Purity of Measurement: Run Value incorporates the outcome of the play, which can be heavily dependent on what happens after the ball is in play (e.g., whether a fielder makes a good or bad play). EEV, on the other hand, directly measures the speed and angle at which the ball leaves the bat. This makes it a more direct and reliable indicator of how effective a pitch was in inducing weak contact or limiting hard contact. By using EEV, the model can capture the intrinsic qualities of the pitch, such as its velocity, movement, and spin, which are the factors directly controlled by the pitcher.

3. Consistency and Reliability: EEV provides a more consistent and reliable metric for evaluating pitch effectiveness because it is not influenced by the randomness inherent in the game of baseball. By removing the noise associated with play outcomes and focusing on the quality of contact, the model can better generalize to different contexts and provide more stable predictions.

4. Improved Predictive Power: Using EEV as the target variable allows the model to learn from more stable and less noisy data. This enhances the model's ability to accurately predict pitch effectiveness in future situations, as it is not confounded by external factors that could introduce variance in the results.

In summary, EEV was selected as the target variable because it offers a more direct, consistent, and unbiased measure of pitch effectiveness, allowing the model to better understand the complex interactions that contribute to a pitcher's success.

Evaluation Approach
Rationale for Averaging:
The xgboost model maximize correlation between predicted and actual EEV values. After prediction, results were averaged by pitcher and pitch type. Correlations between the average values of xEEV and EEV were chosen as the primary evaluation metric.

By averaging xEEV and EEV for each pitcher-pitch type combination, the overall performance of that pitch type was captured. Instead of focusing on individual pitches, this approach averages the effectiveness over many pitches, giving a better representation of how that pitch type performs overall.

Pitch-by-pitch predictions can be noisy due to the inherent variability in baseball. Aggregating by pitcher and pitch type provides a more stable and meaningful assessment of a pitcher's overall 'stuff'. This approach allows for easier interpretation and application in player evaluation and strategy development.

Through a comprehensive analysis of various pitch count thresholds, it became evident that the correlation between Expected Exit Velocity (xEEV) and Effective Exit Velocity (EEV) consistently improved as the minimum number of pitches per group increased. Starting with a threshold of 0 pitches, the correlation was already reasonably strong at 0.829. However, as the threshold was gradually raised, the correlation strengthened significantly, reaching 0.944 at 50 pitches. This pattern suggests that larger sample sizes enhance the reliability and precision of the relationship between xEEV and EEV. Setting a minimum threshold filters the data to include only those pitcher-pitch type combinations that meet or exceed the specified pitch count. This method progressively excludes combinations with fewer pitches, ensuring that the analysis focuses on larger sample sizes, which typically yield more stable and accurate correlations.

Similarly, when evaluating ranges of pitch counts, a clear trend emerged. For instance, in the range of 0-15 pitches, the correlation was 0.796. As the range shifted to 45-60 pitches, the correlation rose to 0.894. This further reinforces the idea that higher pitch counts yield more accurate assessments of pitch effectiveness. Using ranges, on the other hand, allows for an exploration of how performance metrics vary across different segments of pitch counts. This approach provides insights into how correlations behave at different stages, offering a more nuanced understanding of the data across specific intervals, rather than progressively filtering it as in the minimum threshold approach.

Based on this analysis, a pitch count threshold of 50 pitches was identified as the optimal point where the correlation between xEEV and EEV is strongest. This threshold will be utilized when applying the stuff+ metric to evaluate a pitcher's arsenal, ensuring that the evaluation is both robust and dependable. While setting a minimum threshold offers a clear baseline for analysis, understanding the behavior of correlations within specific pitch count ranges adds valuable context, making the evaluation more comprehensive.

In practical terms, this means that when evaluating a pitcher's arsenal, it is crucial to ensure that each pitch type has accumulated enough data (at least 50 pitches) to provide a reliable assessment. By doing so, coaches and analysts can make more informed decisions about a pitcher's strengths and weaknesses. Additionally, this model can be applied during bullpens to predict pitch outcomes in real-time. By inputting data from a bullpen session, the model can provide immediate feedback on how a pitch is expected to perform in a game scenario, based on the xEEV-EEV relationship. This allows pitchers to adjust their mechanics or pitch selection on the fly, optimizing their performance before they step into a live game.

Stuff+ Metric Explanation
Nature of Stuff+:
Stuff+ is a composite metric derived from the model's predictions. It represents the overall quality of a pitch or pitch type, taking into account various physical characteristics.

Calculation:
The model predicts the Expected Effective Exit Velocity (xEEV) for each pitch. These predictions are then averaged by pitcher and pitch type.

The averaged xEEV is then scaled and centered to create the Stuff+ metric:

Stuff+ = 100 + (league_average_xEEV - pitcher_xEEV) / league_standard_deviation_xEEV * 15

This scaling ensures that the average Stuff+ across all pitches is 100, with higher values indicating better 'stuff'.

Interpretation:
A Stuff+ value of 100 represents average pitch quality. Values above 100 indicate above-average pitch quality, while values below 100 indicate below-average pitch quality.

Model Application
Single Model Approach:
One unified model was developed to evaluate all pitch types. This approach allows the model to learn the relative effectiveness of different pitch characteristics across all types of pitches.

Pitch Type Differentiation:
While a single model is used, the inclusion of pitch type as a feature allows the model to capture type-specific relationships. The aggregation by pitch type in the final analysis ensures that each pitch type is evaluated separately, providing insights into a pitcher's effectiveness with different pitches.

Evaluation Process:
The model makes predictions on a pitch-by-pitch basis. These predictions are then averaged to provide Stuff+ values for each pitcher-pitch type combination. This allows for both detailed analysis of individual pitches and broader evaluation of a pitcher's arsenal.

Results and Insights
Model Performance:
The final correlation between predicted and actual Effective Exit Velocity (EEV) after aggregation with a minimum pitch count of 50 is 0.94, demonstrating a strong relationship between the model's predictions and the actual outcomes. Moreover, the Mean Squared Error (MSE) of 987.89, consistent across both the training and test sets, indicates that the model generalizes well, minimizing the risk of overfitting. This suggests that the model is robust and can be relied upon for making predictions across different data samples.

When comparing the original test MSE of 987.89 with the slightly higher MSE of 988.31 under high regularization, we observe that the increase in regularization did not significantly degrade the model's performance. This indicates that the model is relatively stable and resilient to adjustments in regularization parameters, further reinforcing its reliability.

The cross-validation results provide additional insights into the model's performance across multiple folds. The test RMSE steadily decreased across iterations, reaching a minimum at iteration 8 with a test RMSE of 47.03. This early stopping point reflects the optimal balance between training and validation performance, ensuring that the model does not overfit to the training data. The minimal variance between the train and test RMSE values throughout cross-validation further supports the model's ability to generalize effectively.

Feature Importance:
Arm slot (21.29%)
Release speed (15.48%)
Horizontal release position (15.41%)
Horizontal movement (14.59%)
Vertical movement (14.35%)
Vertical release position (12.78%)
Extension (0.06%)
This hierarchy provides insights into which physical characteristics most influence pitch quality.

Correlation Analysis
Exit Velocity:
Strong negative correlation (-0.893, R-squared: 0.797).

Whiff Rate:
Strong positive correlation (0.864, R-squared: 0.746).

Batting Average:
Moderate negative correlation (-0.591, R-squared: 0.349).

Slugging Percentage:
Moderate negative correlation (-0.489, R-squared: 0.239).

These correlations validate the Stuff+ metric by showing its strong relationships with key performance indicators.

Applications in Bullpen Evaluation and Pitcher Development
Pitch Type Optimization:
Pitchers can experiment with different pitch types and variations during bullpen sessions. The Stuff+ model can help identify which pitch types are most effective for each individual pitcher, potentially uncovering hidden strengths or areas for improvement in their repertoire.

Refinement of Pitch Characteristics:
Given the model's insights into feature importance, pitchers can focus on optimizing key characteristics:

Arm slot: Experimenting with slight adjustments to find the most effective release point.
Velocity: Velocity is not everything, but often times it can be the determining factor.
Movement: Adjusting grip or mechanics to enhance horizontal and vertical movement.
Progress Tracking:
Over multiple bullpen sessions, pitchers can track their Stuff+ scores to monitor improvement. This data-driven approach allows for objective measurement of progress, complementing subjective assessments from coaches.

MLB-Level Benchmarking:
Since the model is trained on MLB data, it provides a benchmark for how a pitch might perform at the highest level. This is particularly valuable for minor league pitchers or prospects, offering insights into how their current 'stuff' compares to MLB standards.

Customized Development Plans:
Based on Stuff+ evaluations, coaches and players can create personalized development plans. These plans can focus on enhancing strengths or addressing weaknesses identified by the model.

Conclusion
The Stuff+ model provides a comprehensive and data-driven approach to evaluating pitch quality in baseball. By leveraging advanced statistical techniques and a rich set of pitch characteristics, it offers valuable insights for player development, scouting, and in-game strategy. As with any model, it's important to use Stuff+ in conjunction with other evaluation methods and expert knowledge for a holistic understanding of pitcher performance.
