# RgentAI: AI-Powered Statistical Analysis Assistant for R

[![R-CMD-check](https://github.com/NathanBresette/rstudioai/workflows/R-CMD-check/badge.svg)](https://github.com/NathanBresette/rstudioai/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

RgentAI is an intelligent AI assistant for R programming that provides both general-purpose coding support and specialized analytical workflow guidance. The software offers conversational AI chat, automatic plot analysis, error debugging, plus four specialized agents for data cleaning, transformation, statistical analysis, and machine learning—all while maintaining reproducibility and statistical rigor.

## Statement of Need

Statistical analysis in R requires extensive domain knowledge, creating barriers for researchers across scientific disciplines. Existing solutions are either too general (requiring deep statistical expertise) or too specialized (limited to specific domains). RgentAI fills this gap by providing intelligent guidance throughout the entire analytical process, combining workflow automation with AI-powered assistance to ensure both efficiency and methodological appropriateness.

## Key Features

### Core AI Assistance

🤖 **Conversational AI Chat** - General R programming help, code explanation, and statistical guidance  
📊 **Analyze Last Plot** - Automatic interpretation and analysis of R visualizations  
🐛 **Debug Last Error** - Intelligent error diagnosis with suggested solutions

### Specialized Analytical Agents

### 🧹 **Data Cleaning Agent**
- **Automated missing value detection** and intelligent imputation strategies
- **Duplicate detection** and removal with conflict resolution
- **Outlier identification** using IQR, Z-score, or custom methods
- **Data type validation** and automatic conversion recommendations
- **Column name standardization** (snake_case, camelCase, PascalCase)

### 🔄 **Transformation Agent**
- **Intelligent mathematical transformations** based on skewness analysis
- **Automated new variable creation** (ratios, interactions, polynomial features)
- **Smart categorical encoding** (one-hot, label encoding, target encoding)
- **Date/time feature engineering** with automated extraction
- **Data reshaping and aggregation** with guided workflows

### 📊 **Statistical Analysis Agent**
- **Automatic test selection** based on data characteristics and assumptions
- **Intelligent effect size calculation** (Cohen's d, eta-squared, odds ratios)
- **Comprehensive distribution analysis** with normality testing
- **Smart group comparison** methods with assumption checking
- **Power analysis** and sample size calculations

### 🤖 **Modeling Agent**
- **Intelligent algorithm recommendations** based on target variable type:
  - Binary classification → Logistic regression only
  - Multi-class classification → Multinomial regression only  
  - Continuous outcomes → Linear regression only
  - Universal algorithms → Random Forest, XGBoost (work for all)
- **Automated feature engineering** and selection
- **Model interpretability** with SHAP values and feature importance
- **Performance evaluation** with appropriate metrics

## Installation

### From GitHub (Development Version)
```r
# Install devtools if not already installed
if (!require(devtools)) install.packages("devtools")

# Install RgentAI
devtools::install_github("NathanBresette/rstudioai")

# Load the package
library(rstudioai)
```

### Dependencies
RgentAI requires R >= 4.0.0 and the following packages:
- `httr` - HTTP requests and API communication
- `jsonlite` - JSON parsing and generation  
- `rstudioapi` - RStudio integration
- `clipr` - Clipboard operations
- `shiny` - Web interface framework
- `httpuv` - HTTP and WebSocket server

## Quick Start

### Basic Usage
```r
# Launch the AI assistant
library(rstudioai)
run_rgent()
```

### Example Workflow: Data Cleaning
```r
# Load sample data with issues
data(mtcars)
messy_data <- mtcars
messy_data[1:5, "mpg"] <- NA  # Add missing values

# Start cleaning agent
run_rgent()
# 1. Select "Data Cleaning Agent"
# 2. Choose 'messy_data' from dropdown
# 3. Configure cleaning options
# 4. Follow guided workflow
```

### Example Workflow: Statistical Analysis
```r
# Create test data for analysis
analysis_data <- data.frame(
  group = rep(c("Treatment", "Control"), each = 30),
  outcome = c(rnorm(30, mean = 12, sd = 3), rnorm(30, mean = 10, sd = 3))
)

# Start statistical analysis
run_rgent()
# 1. Select "Statistical Analysis Agent"  
# 2. Choose 'analysis_data' from dropdown
# 3. Select analysis types (group comparisons, effect sizes)
# 4. Get automatic method selection and results
```

### Example Workflow: Machine Learning
```r
# Prepare modeling data
model_data <- iris
model_data$Species <- factor(model_data$Species)

# Start modeling agent  
run_rgent()
# 1. Select "Modeling Agent"
# 2. Choose 'model_data' from dropdown
# 3. Select target variable: 'Species' (3 categories)
# 4. System automatically suggests multinomial regression
# 5. Configure and train models
```

## AI Enhancement

### Access Requirements
RgentAI uses a managed cloud service for AI functionality to ensure computational sustainability and service reliability. The core R package provides statistical guidance and workflow automation, enhanced with AI assistance when access codes are provided.

- **Academic researchers**: Free access codes available at [rgentai.com](https://rgentai.com)
- **Educational use**: Institution-wide access available
- **Commercial use**: Enterprise licensing available

### Offline Capabilities
The package provides substantial functionality without AI enhancement:
- Complete workflow frameworks for all four agents
- Statistical method selection based on data characteristics  
- Automated analysis pipelines
- Educational guidance and best practices

## For JOSS Reviewers

**Reviewer Access Codes**: 
- Primary: `JOSS_REVIEWER_2024_1`
- Backup: `JOSS_REVIEWER_2024_2`  
- Demo: `JOSS_DEMO_2024`

**Testing Guide**: See [JOSS_REVIEWER_GUIDE.md](JOSS_REVIEWER_GUIDE.md) for comprehensive testing instructions.

**Expected Testing Time**: 30-45 minutes to evaluate all four agents with provided sample workflows.

## Documentation

### Package Documentation
```r
# View main package help
?rstudioai

# Function-specific help
?run_rgent
?start_cleaning_agent
?start_statistical_analysis
```

### Vignettes and Examples
```r
# Browse available vignettes
browseVignettes("rstudioai")

# Quick start guide
vignette("quickstart", package = "rstudioai")

# Statistical analysis examples
vignette("statistical-workflows", package = "rstudioai")
```

## Architecture

### System Components
1. **Open Source R Package** (MIT License)
   - Complete agent workflow framework
   - Statistical method selection logic
   - User interface components
   - Local execution capabilities

2. **Cloud AI Service** (Managed Access)
   - Natural language processing
   - Code generation assistance  
   - Advanced method recommendations
   - Scalable computational resources

### Integration
- **RStudio Add-in**: Native integration with RStudio IDE
- **WebSocket Communication**: Real-time bidirectional data exchange
- **Code Insertion**: Direct insertion of generated code into R scripts
- **Environment Awareness**: Automatic detection of available data frames and variables

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on:

- **Reporting bugs** and requesting features
- **Code contributions** and development setup
- **Documentation improvements**
- **Testing** and quality assurance

### Development Setup
```r
# Clone and setup development environment
git clone https://github.com/NathanBresette/rstudioai.git
cd rstudioai

# Install development dependencies
install.packages(c("devtools", "roxygen2", "testthat"))

# Load package for development
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()
```

## Comparison with Existing Tools

| Feature | RgentAI | Base R | RStudio | mlr3/caret | SPSS/SAS |
|---------|---------|--------|---------|------------|----------|
| **Guided Workflows** | ✅ Full | ❌ None | ⚠️ Limited | ⚠️ ML only | ✅ Yes |
| **AI Assistance** | ✅ Advanced | ❌ None | ⚠️ Basic | ❌ None | ❌ None |
| **Method Selection** | ✅ Automatic | ❌ Manual | ❌ Manual | ⚠️ ML only | ⚠️ Limited |
| **Statistical Rigor** | ✅ High | ✅ High | ✅ High | ⚠️ ML focus | ✅ High |
| **Learning Curve** | ✅ Low | ❌ High | ⚠️ Medium | ❌ High | ⚠️ Medium |
| **Reproducibility** | ✅ High | ✅ High | ✅ High | ✅ High | ❌ Limited |
| **Open Source** | ✅ Yes | ✅ Yes | ❌ Freemium | ✅ Yes | ❌ No |

## Citation

If you use RgentAI in your research, please cite:

```bibtex
@article{bresette2024rgentai,
  title={RgentAI: An AI-Powered Agent System for Guided Statistical Analysis in R},
  author={Bresette, Nathan},
  journal={Journal of Open Source Software},
  year={2024},
  note={Submitted for review}
}
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

### Getting Help
- **Documentation**: Start with package documentation and vignettes
- **GitHub Issues**: For bug reports and feature requests
- **GitHub Discussions**: For questions and community support
- **Email**: [support@rgentai.com] for technical assistance

### Response Times
- **GitHub Issues**: Usually within 48 hours
- **Email Support**: Within 24 hours for urgent issues
- **Feature Requests**: Reviewed and prioritized monthly

## Acknowledgments

- **R Core Team** for the R statistical computing environment
- **RStudio Team** for the development environment and integration APIs
- **Statistical Computing Community** for methods and best practices
- **Open Source Contributors** who help improve and extend RgentAI

---

**RgentAI**: Making advanced statistical analysis accessible to researchers worldwide. 🚀
