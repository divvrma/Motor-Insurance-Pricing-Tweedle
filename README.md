# Motor Insurance Pricing with Tweedie GLM

A complete R implementation of motor insurance pricing using Tweedie Generalized Linear Models (GLM) and Gradient Boosting Machines (GBM) on French motor insurance data, featuring an interactive Shiny application for rate change simulation.

## 🚗 Overview

This project demonstrates advanced actuarial modeling techniques for motor insurance pricing:

- **Tweedie GLM**: Ideal distribution for insurance claims (handles zero claims and continuous positive amounts)
- **GBM Benchmark**: Gradient boosting comparison model for performance evaluation  
- **Interactive Simulation**: Shiny web app for testing rate change scenarios
- **Model Validation**: Comprehensive lift curves, calibration plots, and Gini metrics

Built using the CAS `freMTPL2` dataset - a real-world French motor third-party liability insurance dataset with 678,013 policies.

## 📊 Key Features

- **Data Processing**: Automated cleaning and feature engineering of insurance data
- **Model Training**: Tweedie power parameter estimation via profile likelihood
- **Performance Metrics**: Exposure-weighted Gini coefficients, lift analysis, calibration
- **Rate Simulation**: Interactive testing of portfolio-wide rate changes
- **Loss Ratio Analysis**: Real-time calculation of impact on profitability

## 🛠️ Installation & Setup

### Prerequisites
- R (>= 4.0.0)
- Required packages will be auto-installed on first run

### Quick Start
```r
# 1. Fit models and generate predictions
source("01_fit_models.R")

# 2. Create performance plots (optional)
source("02_plots_metrics.R")

# 3. Launch interactive rate simulator
shiny::runApp(".")
```

## 📁 Project Structure

```
├── 01_fit_models.R      # Main modeling script
├── 02_plots_metrics.R   # Performance visualization  
├── app.R               # Interactive Shiny application
├── README.md           # This file
├── .gitignore         # Git ignore patterns
├── data/              # Model data (generated)
├── models/            # Saved model objects (generated)
├── outputs/           # Results and scored data (generated)
└── figures/           # Performance plots (generated)
```

## 🎯 Model Performance

The Tweedie GLM is specifically designed for insurance data:
- **Power Parameter**: Automatically estimated via profile likelihood (typically ~1.2-1.7)
- **Zero Inflation**: Naturally handles policies with no claims
- **Right Skew**: Accommodates high-value claims in the tail
- **Exposure Weighting**: All metrics properly weighted by policy exposure

## 💡 Interactive Features

The Shiny application provides:
- **Model Selection**: Switch between GLM and GBM predictions
- **Rate Adjustment**: Apply portfolio-wide rate changes (+/- 40%)
- **Loss Ratio**: Real-time profitability impact calculation
- **Distribution Plots**: Visualize premium change effects across policies
- **Calibration Analysis**: Model accuracy by prediction deciles

## 🔧 Technical Details

### Tweedie Distribution
- **Variance Function**: V(μ) = φμᵖ where p ∈ (1,2)
- **Log Link**: Ensures positive predictions
- **Profile Likelihood**: Optimal p parameter selection

### Performance Metrics
- **Gini Coefficient**: Ranking quality (higher = better discrimination)
- **Calibration**: Predicted vs observed pure premiums by decile
- **Lift Analysis**: Cumulative loss concentration in top deciles

## 📈 Usage Examples

### Running Models
```r
# Fit Tweedie GLM and GBM models
source("01_fit_models.R")
# Output: Models saved to models/, metrics in outputs/
```

### Generate Plots
```r  
# Create lift and calibration visualizations
source("02_plots_metrics.R")
# Output: PNG files in figures/ directory
```

### Interactive Simulation
```r
# Launch web application
shiny::runApp(".")
# Access at http://localhost:3838
```

## 📋 Data Source

Uses CAS `freMTPL2` datasets:
- **freMTPL2freq**: Policy-level frequency data (678,013 records)
- **freMTPL2sev**: Claim-level severity data
- **Features**: Vehicle characteristics, driver demographics, geographic factors
- **Target**: Pure premium (claim cost per unit exposure)

## 🚀 Future Enhancements

- [ ] Individual risk factor adjustments
- [ ] Territory-specific rate changes  
- [ ] Model ensemble predictions
- [ ] Advanced visualization dashboards
- [ ] Export capabilities for rate filings

## 📄 License

Open source project for educational and research purposes in actuarial science and insurance analytics.

---
*Built with R, Shiny, and modern actuarial modeling techniques*