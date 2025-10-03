---
title: 'RgentAI: An AI-Powered Agent System for Guided Statistical Analysis in R'
tags:
  - R
  - statistical computing
  - artificial intelligence
  - data science
  - machine learning
  - productivity enhancement
  - RStudio integration
authors:
  - name: Nathan Bresette
    orcid: 0009-0003-1554-6006
    affiliation: 1
  - name: Dane Winterboer
    orcid: 0009-0006-4113-2538
    affiliation: 2
affiliations:
 - name: Independent Researcher
   index: 1
 - name: Independent Researcher
   index: 2
date: 6 September 2025
bibliography: paper.bib
---

# Summary

Statistical analysis in R requires extensive domain knowledge, creating barriers for researchers. RgentAI provides an AI assistant with four specialized agents (data cleaning, transformation, statistical analysis, and machine learning) plus three core features: conversational chat, plot analysis, and error debugging.

The system integrates with RStudio through an R package, offering real-time assistance. Features include: (1) conversational AI chat, (2) automatic plot analysis, (3) intelligent error debugging, (4) guided workflow automation, and (5) statistical guidance. RgentAI accelerates RStudio workflows through intelligent automation.

The software targets domain scientists, students, and researchers seeking to accelerate analytical workflows. Early evaluation demonstrates improvements in analysis efficiency and coding speed. The system operates through a managed cloud service ensuring sustainability while maintaining academic accessibility.

# Statement of need

Modern research relies heavily on statistical analysis, yet choosing appropriate methods creates barriers for domain experts. Researchers must navigate data preprocessing, statistical tests, effect size calculations, and modeling approaches—requiring specialized knowledge outside their expertise [@wickham2014tidy; @wilson2014best].

Existing solutions are either too general (base R functions) or too specialized (domain-specific packages). General tools require extensive statistical knowledge, while specialized packages lack complete workflow integration. This forces researchers to piece together solutions from multiple sources.

RgentAI fills this gap by providing intelligent AI assistance throughout the analytical process. Unlike existing tools, it combines workflow automation with conversational AI support to accelerate development while ensuring methodological appropriateness. The target audience includes domain scientists, students, and researchers seeking to maximize analytical productivity.

# Software description

RgentAI implements an AI assistance system for R programming with general AI support features and specialized analytical agents:

## Core AI Features

**Conversational AI Chat** provides assistance for R programming questions, code explanation, debugging help, and methodological guidance. Users ask natural language questions about R syntax, statistical concepts, or data analysis approaches, receiving contextual responses with executable code examples.

**Analyze Last Plot** identifies and analyzes the most recent plot command from R history, providing intelligent interpretation of visualizations including patterns, outliers, distribution characteristics, and relationships. The system suggests visualization improvements and identifies potential data quality issues.

**Debug Last Error** monitors R console errors and provides intelligent diagnosis and solutions. When R code fails, the system analyzes the error message, examines code context, and suggests specific fixes with explanations.

## Specialized Analytical Agents

**Data Cleaning Agent** automates detection and handling of missing values, outliers, duplicates, and data type inconsistencies. The agent provides intelligent recommendations based on data characteristics, such as median imputation for skewed numeric variables or mode imputation for categorical variables [@wickham2014tidy].

**Transformation Agent** provides automated data transformations, including mathematical transformations based on skewness analysis (automatic log transformation for highly skewed data), new variable creation through feature engineering, and data reshaping using tidyverse principles [@wickham2019welcome].

**Statistical Analysis Agent** offers intelligent method selection for hypothesis testing, effect size calculation, and exploratory analysis. The system automatically selects appropriate tests based on data types and sample characteristics (e.g., Shapiro-Wilk for normality testing with small samples, Anderson-Darling for larger samples).

**Modeling Agent** provides smart algorithm recommendations based on target variable characteristics, automatically suggesting logistic regression for binary classification, multinomial regression for multi-class problems, and linear regression for continuous outcomes. The agent handles feature engineering, model interpretation, and performance evaluation.

The system's intelligence lies in context-aware decision making. For example, when analyzing categorical variables, the system examines unique categories and automatically suggests binary classification methods for two-category variables and multi-class methods for three or more categories, preventing methodological errors while educating users.

**Architecture**: The software consists of an open-source R package providing the agent framework and user interface, integrated with a cloud-based AI service for natural language processing and code generation. The R package functions independently for basic workflows, with AI enhancement available through managed access codes.

**Installation and Usage**: RgentAI is installed from GitHub using `devtools::install_github("NathanBresette/Rgent-AI", force = TRUE, upgrade = "never")`. Users launch the assistant with `run_rgent()` and enter their access code to activate AI features. The system integrates seamlessly with RStudio [@rstudio2024], providing real-time assistance through the familiar development environment.

**Comparison to existing tools**: Unlike general statistical packages (R base [@r2024base], SPSS) requiring extensive expertise, or specialized tools (mlr3, caret) focusing on specific domains, RgentAI provides comprehensive AI assistance across all aspects of R programming and analysis. The system offers conversational support, visual analysis, and error debugging—capabilities not found in traditional statistical software.

**Integration**: The software integrates with RStudio through a native add-in, providing comprehensive AI assistance. All AI interactions occur within the RStudio interface, accelerating coding workflows. Generated code can be inserted directly into R scripts with a single click, eliminating copy-paste overhead. The system automatically monitors R console errors and analyzes recent plot commands, providing instant contextual assistance without interrupting development flow.

# Impact and ongoing research

RgentAI enhances productivity and accelerates development workflows in R programming and statistical analysis. Early adoption demonstrates improvements in coding speed, analytical efficiency, and development cycle times. The system's productivity enhancements are valuable for time-sensitive research projects and intensive data analysis workflows.

The AI-powered architecture provides a foundation for extending intelligent assistance to other aspects of scientific computing. Current development focuses on expanding productivity features, reducing time-to-insight, and improving integration with the R ecosystem. This aligns with reproducible research practices [@peng2011reproducible] by providing tools that enhance both productivity and methodological rigor.

The open-source R package encourages community contributions and customization, while the managed cloud service ensures sustainable operation and continuous improvement of AI capabilities.

# Acknowledgements

We acknowledge the R community for creating the foundation that makes this work possible, and the developers of the statistical packages that inform our methodological recommendations.

# References
