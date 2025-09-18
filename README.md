# RgentAI: AI-Powered Statistical Analysis Assistant for R

An intelligent agent system that guides users through complete statistical analysis workflows in R. RgentAI provides AI-powered assistance for data cleaning, transformation, statistical analysis, and machine learning while maintaining reproducibility and statistical rigor.

## Features

### 🧹 **Data Cleaning Agent**
- Automated missing value detection and intelligent imputation
- Duplicate detection and outlier identification  
- Data type validation and column name standardization

### 🔄 **Transformation Agent**
- Intelligent mathematical transformations based on skewness analysis
- Automated new variable creation (ratios, interactions, features)
- Smart categorical encoding and data reshaping

### 📊 **Statistical Analysis Agent**
- Automatic test selection based on data characteristics
- Intelligent effect size calculation and distribution analysis
- Smart group comparison methods with assumption checking

### 🤖 **Modeling Agent**
- Intelligent algorithm recommendations based on target variable:
  - Binary classification → Logistic regression
  - Multi-class classification → Multinomial regression  
  - Continuous outcomes → Linear regression
- Automated feature engineering and model interpretability

### 📈 **Visualization Agent**
- Smart plot selection based on data types and analysis goals
- Automated chart generation with proper statistical annotations
- Interactive visualizations and publication-ready graphics

## Installation

```r
# Install from GitHub
if (!require(devtools)) install.packages("devtools")
devtools::install_github("NathanBresette/rstudioai")

# Load and launch
library(rstudioai)
run_rgent()
```

## Quick Start

```r
# Launch the AI assistant
run_rgent()

# Select from 5 specialized agents and follow the guided workflow:
# 1. Choose your dataframe
# 2. Configure analysis options  
# 3. Get AI-powered guidance and code generation
```

## Requirements

- R >= 4.0.0
- RStudio (recommended for full integration)
- Internet connection for AI features
- Access code (free for academic use - visit [rgentai.com](https://rgentai.com))

## Documentation

```r
# Package help
?rstudioai

# Function documentation  
?run_rgent
?start_cleaning_agent
?start_transformation_agent
?start_statistical_analysis
?start_modeling_agent
?start_visualization_agent
```

## Contributing

See [joss-submission/CONTRIBUTING.md](joss-submission/CONTRIBUTING.md) for contribution guidelines.

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Citation

If you use RgentAI in your research:

```bibtex
@software{bresette2024rgentai,
  title={RgentAI: AI-Powered Statistical Analysis Assistant for R},
  author={Bresette, Nathan},
  year={2024},
  url={https://github.com/NathanBresette/rstudioai}
}
```
