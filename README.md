# NYC Yellow Taxi Data Visualization

An interactive Shiny web application for visualizing and analyzing New York City Yellow Taxi trip data from 2015. This application provides comprehensive data exploration tools including interactive maps, statistical summaries, and analytical charts.

## Features

### 📊 Data Overview Tab
- **Key Statistics**: Total trips, total revenue, average fare, average trip distance, and average trip duration
- **Filter Summary**: Real-time display of active filters and filtered data summary
- **Data Table**: Interactive preview of filtered data (first 100 rows)

### 📈 Analytics Tab
- **Daily Distribution Chart**: Bar and line chart showing order volume and average total amount by weekday
- **Trip Distance Distribution**: Violin plot showing trip distance distribution across weekdays
- **Total Amount Distribution**: Box plot showing total amount distribution by weekday
- **Distance vs Fare Relationship**: Scatter plot with linear regression line
- **Passenger Count Distribution**: Donut chart showing distribution by passenger count

### 🗺️ Pickup Locations Map Tab
- Interactive map showing pickup locations across NYC
- Color-coded by number of pickups (log scale)
- Hover tooltips showing detailed location information
- Download functionality for high-resolution map images

### 💰 Revenue Heatmap Tab
- Interactive heatmap showing total revenue by pickup location
- Color-coded visualization of revenue hotspots
- Filtered to show locations with more than 3 pickups
- Download functionality for high-resolution map images

## Filters

The application provides three types of filters to explore the data:

1. **Area Filter**: Filter by geographic region (All, SW, SE, NW, NE)
   - Divides NYC into four quadrants based on latitude/longitude
   - Center point: 40.7128°N, -74.0059°W

2. **Weekday Filter**: Select one or more days of the week
   - Monday through Sunday
   - Multiple selection supported

3. **Time of Day Filter**: Filter by hour of pickup
   - Range slider from 0 to 24 hours
   - 0.5-hour increments

## Requirements

### R Packages
- `shiny` - Web application framework
- `tidyverse` - Data manipulation and visualization
- `data.table` - Fast data reading and manipulation
- `dplyr` - Data manipulation
- `ggplot2` - Static plotting
- `DT` - Interactive data tables
- `scales` - Scale functions for visualization
- `hexbin` - Hexagonal binning
- `sf` - Spatial data handling
- `plotly` - Interactive plots
- `lubridate` - Date and time manipulation
- `viridis` - Color scales

### Data File
- `trips_2015-week02_final_clean.csv` - Cleaned NYC taxi trip data (431MB)

**Note:** Due to GitHub's file size limitations, the data file is hosted on Google Drive. Please download it manually before running the application.

## Data Download

The data file `trips_2015-week02_final_clean.csv` (431MB) is required to run this application.

### Download Options

You can download the data file from the following sources:

1. **Google Drive (Recommended):**
   - [Download from Google Drive Folder](https://drive.google.com/drive/folders/110OsnwhdQOtS_ifV-ETxXx4GtfaBaFTZ?usp=share_link)
   - Contains: `app.R`, `README.md`, and `trips_2015-week02_final_clean.csv`
   - File size: 431.2 MB
   - **This is the processed dataset containing one week of data (week 02, 2015)**
   - **This is the exact dataset used by this application**
   - Simply download the CSV file from the folder

2. **Kaggle Dataset:**
   - [NYC Yellow Taxi Trip Data on Kaggle](https://www.kaggle.com/datasets/elemento/nyc-yellow-taxi-trip-data)
   - Original full dataset source (contains data for the entire year 2015)
   - **Note:** The Kaggle dataset is much larger as it contains the complete dataset for the entire year
   - **Our application uses only one week of data (week 02), which is available in the Google Drive folder above**
   - If you download from Kaggle, you will need to extract and process the data for week 02 to match the format used by this application

### Setup Instructions

1. **Download the data file:**
   - **Recommended:** Download from Google Drive (this is the exact dataset used by the application - one week of processed data)
   - **Alternative:** Download from Kaggle if you want the full dataset (requires processing to extract week 02 data)
   - Download `trips_2015-week02_final_clean.csv`

2. **Place the file:**
   - Save the file in the same directory as `app.R`
   - The file should be named exactly: `trips_2015-week02_final_clean.csv`

3. **Verify the file:**
   - File size should be approximately 431MB
   - File should be in CSV format

### Alternative: Command Line Download

If you prefer using command line to download from Google Drive, you can use:

```bash
# Note: For folder downloads, you may need to download individual files
# Visit the Google Drive folder link above and download the CSV file manually
```

## Installation

1. Clone the repository or download the files
2. Install required R packages:
```r
install.packages(c("shiny", "tidyverse", "data.table", "dplyr", "ggplot2", 
                   "DT", "scales", "hexbin", "sf", "plotly", "lubridate", "viridis"))
```

3. Ensure the data file `trips_2015-week02_final_clean.csv` is in the same directory as `app.R`

## Usage

1. Open R or RStudio
2. Set the working directory to the folder containing `app.R`
3. Run the application:
```r
shiny::runApp("app.R")
```

Alternatively, if you're already in the directory:
```r
shiny::runApp()
```

4. The application will open in your default web browser

## Data Format

The application expects a CSV file with the following columns:
- `tpep_pickup_datetime` - Pickup timestamp (ISO format)
- `tpep_dropoff_datetime` - Dropoff timestamp (ISO format)
- `pickup_latitude` - Pickup location latitude
- `pickup_longitude` - Pickup location longitude
- `fare_amount` - Fare amount in USD
- `total_amount` - Total trip amount in USD
- `trip_distance` - Trip distance in miles
- `passenger_count` - Number of passengers

## Technical Details

- **Data Processing**: Uses `data.table` for efficient data manipulation
- **Visualization**: Interactive plots powered by `plotly`
- **Maps**: Scatter plots with geographic coordinates
- **Performance**: Automatic sampling for large datasets (5000+ rows) in certain visualizations
- **Styling**: Custom CSS for enhanced UI components

## Notes

- The application processes data reactively, updating all visualizations when filters change
- Large datasets are automatically sampled for certain visualizations to maintain performance
- Map downloads are saved as PNG files with 300 DPI resolution
- All timestamps are processed in UTC timezone

## License

This project is provided as-is for educational and research purposes.

## Author

**Class1 Group3**

Developed by:
- Zhenbei Guo
- Yuecheng Hong
- Minyu Zhang

Contact: yuecheng.hong@edu.em-lyon.com

