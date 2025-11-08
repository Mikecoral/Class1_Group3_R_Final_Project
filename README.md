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

### Download Instructions

1. **Download from Google Drive:**
   - [Download Data File](https://drive.google.com/uc?export=download&id=YOUR_FILE_ID) 
   - **Note:** Replace `YOUR_FILE_ID` with the actual Google Drive file ID after uploading the file.
   
2. **Using command line (alternative):**
   ```bash
   wget --no-check-certificate 'https://drive.google.com/uc?export=download&id=YOUR_FILE_ID' -O trips_2015-week02_final_clean.csv
   ```
   
   Or using curl:
   ```bash
   curl -L 'https://drive.google.com/uc?export=download&id=YOUR_FILE_ID' -o trips_2015-week02_final_clean.csv
   ```

3. **Place the file:**
   - Save the file in the same directory as `app.R`
   - The file should be named exactly: `trips_2015-week02_final_clean.csv`

4. **Verify the file:**
   - File size should be approximately 431MB
   - File should be in CSV format

**How to get the Google Drive file ID:**
1. Upload the file to Google Drive
2. Right-click the file and select "Get link" or "Share"
3. Set permissions to "Anyone with the link can view"
4. Copy the link: `https://drive.google.com/file/d/FILE_ID/view?usp=sharing`
5. Extract the `FILE_ID` (the string between `/d/` and `/view`)
6. Replace `YOUR_FILE_ID` in the download link above

For detailed instructions, see [GOOGLE_DRIVE_GUIDE.md](GOOGLE_DRIVE_GUIDE.md)

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

