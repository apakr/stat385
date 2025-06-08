# STAT 385 - Big Data Mining and Applied Statistical Learning

This repository contains coursework and a final project from my STAT 385 class at UIC. The course was built around *An Introduction to Statistical Learning* by James, Witten, Hastie, and Tibshirani. The work focuses on practical applications of statistical learning methods, using real data, clean code, and reproducible workflows. Everything is written in R. I took this class in Spring 2025 and earned an A - rubric is found in repo.

## Main Outcomes

- Built and evaluated **supervised learning** models including **linear regression**, **logistic regression**, **classification trees**, **support vector machines**, and **ensemble methods**.

- Applied model validation techniques such as **k-fold cross-validation**, **bootstrap resampling**, and **train/test splitting** to evaluate performance.

- Used **shrinkage methods** (**LASSO** and **Ridge regression**) to handle **high-dimensional feature spaces**, reduce variance, and control **multicollinearity**.

- Implemented **tree-based algorithms** including **bagging**, **random forests**, and **boosting**, with focused **hyperparameter tuning** and interpretation of **feature importance** metrics. (I was sick for a lot of this portion so my abilities with these are slightly weaker than other parts of the class).

- Developed reproducible, end-to-end modeling work in RMarkdown - each notebook includes data wrangling, exploratory analysis, model development, performance evaluation, and interpretive commentary.


## Technical Highlights

- Use of R libraries like `glmnet`, `randomForest`, `gbm`, and `caret` for training, tuning, and validating models.

- Diagnostic plotting for residuals, learning curves, and misclassification rates to assess model performance and decision boundaries.

- Structured approach to solving problems: clear EDA, model setup, training, evaluation, and conclusions.

## Final Project

The final project brings together methods from across the course to solve a modeling task with multiple competing approaches on a dataset of student grades in Portuguese and Math classes. It demonstrates:

- Full-cycle modeling: from raw data to model selection and final reporting
- Use of both linear and nonlinear methods
- Comparison of model accuracy and generalization using resampling techniques
- Interpretation of results with clarity and attention to statistical rigor

There were three people in our group including me, and we also had a presentation on our findings (see powerpoint). We recieved an A on our final.

## Running the Code

Clone the repository and open any files in RStudio. All dependencies are loaded at the top of each file.

```bash
git clone https://github.com/apakr/stat385.git  
cd stat385
```

Then open the desired file in RStudio and run it (or knit it to HTML or PDF if an rmd file).

## Next Steps

The projects here form a base that can easily extend to financial modeling, signal research, or production-level analytics. The structure and methodology support fast iteration, clean documentation, and reproducibility-making it straightforward to scale or adapt to new domains.

## Disclaimer

The code found in this repo is very sophisticated and highly complex. As students we were given partial starter scripts for a lot of our homeworks and labs which we were meant to complete using techniques we would learn in class. While roughly 50% of the code in homework and lab files originated from those templates, all model implementation, debugging, completion logic, and interpretation were written independently. In many cases, significant effort was required to restructure or correct the provided code to produce working models and accurate results (the code provided would often be very fragmented and would lack good instructions on how to make it work, sorry Prof Embers!).

## Repository Structure

- `homework2`–`homework8` — Weekly homework assignments covering topics from regression to SVM.
- `labs` — In-class lab exercises with hands-on model implementation.
- `final_project` — Group project on student grade prediction using multiple supervised learning models.
- `STAT385_syllabi.pdf` — Full syllabus detailing course structure, schedule, etc.

## Tools Used

- R, RStudio
- tidyverse, glmnet, caret, randomForest, gbm, e1071
- RMarkdown for literate programming and reproducibility
