# JOSS Reviewer Testing Guide

This guide provides step-by-step instructions for JOSS reviewers to test RgentAI functionality.

## Quick Start

### 1. Installation
```r
# Install required packages
if (!require(devtools)) install.packages("devtools")

# Install RgentAI (choose one repository and keep consistent)
remotes::install_github("NathanBresette/Rgent-AI")

# Load the package
library(rstudioai)
```

### 2. Launch the Assistant
```r
# Start the AI assistant
run_rgent()
```

### 3. Access Codes for Reviewers
Use one of these access codes for full functionality testing:
- **Primary**: `JOSS_REVIEWER_2024_1`
- **Backup**: `JOSS_REVIEWER_2024_2`
- **General Demo**: `JOSS_DEMO_2024`

*Note: These codes provide full access to all AI features for evaluation purposes.*

## Testing Each Agent

### Data Cleaning Agent

#### Test with Built-in Data:
```r
# Load sample data with issues
data(mtcars)
mtcars_messy <- mtcars
mtcars_messy[1:5, "mpg"] <- NA  # Add missing values
mtcars_messy[10, ] <- mtcars_messy[9, ]  # Add duplicate
```

#### Expected Workflow:
1. **Select Data Cleaning Agent**
2. **Choose dataframe**: `mtcars_messy`
3. **Test these features**:
   - Missing values detection and handling
   - Duplicate detection
   - Data type analysis
   - Outlier detection using IQR method
   - Column name standardization

#### Expected Outputs:
- Missing value summary: "5 missing values detected in mpg column"
- Duplicate detection: "1 duplicate row found"
- Data type recommendations
- Outlier analysis with IQR boundaries
- Cleaned dataset with standardized names

### Transformation Agent

#### Test with iris dataset:
```r
data(iris)
```

#### Expected Workflow:
1. **Select Transformation Agent**
2. **Choose dataframe**: `iris`
3. **Test these features**:
   - Mathematical transformations (automatic skewness-based)
   - New variable creation (ratios, interactions)
   - Categorical transformations
   - Distribution analysis

#### Expected Outputs:
- Skewness analysis: "Sepal.Length skewness: 0.31 (no transformation needed)"
- New variables created: ratios between measurements
- Distribution plots and statistics
- Transformation recommendations

### Statistical Analysis Agent

#### Test with Two-Group Comparison:
```r
# Create test data
test_data <- data.frame(
  group = rep(c("A", "B"), each = 25),
  value = c(rnorm(25, 10, 2), rnorm(25, 12, 2))
)
```

#### Expected Workflow:
1. **Select Statistical Analysis Agent**
2. **Choose dataframe**: `test_data`
3. **Test these features**:
   - Group comparisons (automatic t-test selection)
   - Effect size analysis (automatic Cohen's d)
   - Distribution analysis
   - Power analysis

#### Expected Outputs:
- Automatic normality testing
- Two-sample t-test results
- Cohen's d effect size: ~1.0 (large effect)
- Statistical interpretation and recommendations

### Modeling Agent

#### Test with Binary Classification:
```r
# Create binary classification dataset
model_data <- data.frame(
  outcome = factor(rep(c("Success", "Failure"), each = 30)),
  predictor1 = c(rnorm(30, 1, 1), rnorm(30, -1, 1)),
  predictor2 = c(rnorm(30, 0.5, 1), rnorm(30, -0.5, 1))
)
```

#### Expected Workflow:
1. **Select Modeling Agent**
2. **Choose dataframe**: `model_data`
3. **Select target variable**: `outcome`
4. **Expected behavior**: Only logistic regression should be suggested (binary outcome)
5. **Test features**:
   - Automatic algorithm selection
   - Model training and evaluation
   - Feature importance analysis

#### Expected Outputs:
- Algorithm suggestion: "Logistic regression recommended for binary classification"
- Model performance metrics
- Feature importance rankings
- Generated R code for model training

### Visualization Agent

#### Test with iris dataset:
```r
data(iris)
```

#### Expected Workflow:
1. **Select Visualization Agent**
2. **Choose dataframe**: `iris`
3. **Test these features**:
   - Automatic chart recommendations by variable type
   - Annotated plots (e.g., means, CIs)
   - Publication-ready export

#### Expected Outputs:
- Suggested chart types for numeric/categorical variables
- Generated ggplot2 code with annotations
- Interpretation of patterns and outliers

### Multi-class Classification Test:
```r
# Test with iris (3 classes)
data(iris)
```

#### Expected Workflow:
1. **Select target variable**: `Species` (3 categories)
2. **Expected behavior**: Only multinomial regression should be suggested
3. **Verify**: Logistic regression should be hidden

## Advanced Testing

### Custom Options Testing
1. **Test "Other/Custom" options** in each agent
2. **Verify text input fields** appear when "Other" is selected
3. **Test validation**: Ensure custom descriptions are required
4. **Test workflow**: Custom requirements should be incorporated

### Error Handling
1. **Test with invalid data**:
   ```r
   invalid_data <- data.frame(x = letters[1:5])
   ```
2. **Test connection issues**: Disconnect internet briefly
3. **Test invalid access codes**: Use incorrect code
4. **Expected**: Graceful error messages, not crashes

### Integration Testing
1. **Code insertion**: Test "Insert at Cursor" functionality
2. **Copy to clipboard**: Verify code copying works
3. **Workflow persistence**: Test multi-step workflows
4. **RStudio integration**: Verify addin appears in RStudio

## Expected Performance

### Response Times:
- **Agent startup**: < 5 seconds
- **Data analysis**: < 10 seconds for datasets < 1000 rows
- **AI responses**: < 30 seconds (depends on network)
- **Code generation**: < 15 seconds

### Memory Usage:
- **Package loading**: < 50MB
- **Agent operation**: < 100MB additional
- **Large datasets**: Graceful handling up to system limits

## Troubleshooting

### Common Issues:

#### "Access code invalid"
- **Solution**: Use provided reviewer codes exactly as listed
- **Check**: No extra spaces or characters

#### "WebSocket connection failed"
- **Solution**: Check internet connection
- **Try**: Restart R session and try again

#### "Agent not responding"
- **Solution**: Wait 30 seconds, then refresh interface
- **Check**: RStudio console for error messages

#### Package installation fails:
```r
# Alternative installation method
install.packages("remotes")
remotes::install_github("NathanBresette/rstudioai")
```

### Getting Help:
- **Email**: [reviewer-support@rgentai.com]
- **GitHub Issues**: Create issue with "JOSS Review" label
- **Expected response time**: < 24 hours during review period

## Evaluation Checklist

### Functionality ✓
- [ ] All five agents launch successfully
- [ ] Data analysis workflows complete without errors
- [ ] AI responses are relevant and helpful
- [ ] Generated code is executable and correct
- [ ] User interface is intuitive and responsive

### Documentation ✓
- [ ] Installation instructions work as described
- [ ] Examples produce expected outputs
- [ ] Function documentation is clear and accurate
- [ ] Error messages are helpful and actionable

### Software Quality ✓
- [ ] No crashes or fatal errors during testing
- [ ] Graceful handling of edge cases
- [ ] Reasonable performance with test datasets
- [ ] Code follows R package best practices

### Research Value ✓
- [ ] Addresses real problems in statistical analysis
- [ ] Provides educational value for users
- [ ] Likely to be useful for broader research community
- [ ] Improves upon existing solutions

## Contact Information

**For review-specific questions:**
- Email: joss-review@rgentai.com
- Response time: < 24 hours

**For technical issues:**
- GitHub: https://github.com/NathanBresette/rstudioai/issues
- Label: "JOSS Review"

Thank you for reviewing RgentAI! Your feedback helps improve statistical analysis accessibility for the research community.
