# 🧪 Production System Testing Guide

## 📦 Installation

1. **Install the package:**
```r
install.packages("~/Desktop/rstudioai_0.1.0.tar.gz", repos = NULL, type = "source")
```

2. **Load the package:**
```r
library(rstudioai)
```

## 🚀 Quick Test

Run the comprehensive test script:
```r
source("test_production_system.R")
```

## 🎯 Manual Testing Steps

### Step 1: Basic Functionality
```r
# Launch the AI chat
launch_websocket_chat()
```

### Step 2: Test Performance
1. **Create test data:**
```r
# Create a large data frame
big_df <- data.frame(
  id = 1:1000,
  value = rnorm(1000),
  category = sample(letters[1:5], 1000, replace = TRUE)
)

# Create some functions
analyze_data <- function(df) {
  summary(df)
}

plot_data <- function(df, col) {
  hist(df[[col]], main = paste("Histogram of", col))
}
```

2. **Test rapid queries:**
- Ask: "What data do I have?"
- Ask: "Plot the big_df data"
- Ask: "Create a function to analyze the data"
- Type rapidly and see if there's any lag

### Step 3: Test Context Intelligence
```r
# Ask specific questions about your workspace
# The AI should intelligently select relevant context

# Data analysis query
"Show me a summary of the big_df data"

# Function query  
"Create a function to calculate statistics"

# Environment query
"What packages are loaded?"

# Specific object query
"Tell me about the analyze_data function"
```

### Step 4: Test Memory Management
```r
# Create many objects to test memory limits
for(i in 1:50) {
  assign(paste0("obj_", i), data.frame(x = 1:100, y = rnorm(100)))
}

# Check if system handles memory efficiently
# Should see cleanup messages in console
```

## 📊 Expected Results

### ✅ Success Indicators
- **Instant responses** (< 100ms)
- **Intelligent context selection** (only relevant info sent)
- **Smooth typing** (no lag during rapid edits)
- **Memory usage** stays under 50MB
- **Error recovery** (graceful fallbacks)

### ⚠️ Warning Signs
- Responses take > 500ms
- Memory usage grows beyond 50MB
- Context includes irrelevant information
- Lag during typing
- Error messages in console

## 🔧 Performance Monitoring

The system includes built-in performance monitoring. Watch for:
- `DEBUG: Production indexing system initialized`
- `DEBUG: Session index updated with X objects`
- `DEBUG: Memory cleanup completed`
- `WARNING: Slow performance detected` (if > 500ms)

## 🐛 Troubleshooting

### Package won't install
```r
# Check if you have the tar.gz file
list.files("~/Desktop/", pattern = "rstudioai.*tar.gz")

# Try installing dependencies first
install.packages(c("httpuv", "jsonlite", "httr", "digest"))
```

### WebSocket server won't start
```r
# Check if port is available
# Try alternative port (8889)
# Check firewall settings
```

### No AI responses
- Check internet connection
- Verify backend is running
- Check console for error messages

## 🎉 Success Criteria

The system is working correctly if:

1. **Zero visible lag** during typing and editing
2. **Intelligent context selection** (AI gets relevant info)
3. **Fast responses** (< 100ms for context assembly)
4. **Memory efficient** (automatic cleanup)
5. **Error resilient** (graceful fallbacks)

## 📈 Performance Benchmarks

| Metric | Target | Current |
|--------|--------|---------|
| Context Assembly | < 100ms | ? |
| Memory Usage | < 50MB | ? |
| Token Reduction | > 90% | ? |
| Error Rate | < 1% | ? |

Run the test script to measure these metrics! 