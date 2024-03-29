

### Instructions for running CCRP_Climate_Futures.Rmd

Maintained by: Climate Change Response Program

*This script was created using R version >= 4.0.2 and RStudio version >= 1.3.1056*
*These instructions assume the user has already connected RStudio to GitHub and has access to the nationalparkservice/CCRP_Climate_Futures repo*

**Step 1. Clone CCRP_Climate_Futures into your local repository folder and connect to RStudio**

    - In RStudio, click on File --> New Project. Select 'Version Control' and then 'Git'.
    - Enter the URL for the repo. Connect using SSH. [CCRP_Climate_Futures] (git@github.com:nationalparkservice/CCRP_Climate_Futures.git)
    - Enter the name of the repo for the Project directory name (CCRP_Climate_Futures)
    - Create the project as a subdirectory of the local folder in which you store your respositories (example: ~/Documents/Repos)
    - Select Create Project. 
    - In the lower right window of RStudio, you should see the files and folders from the repo under the 'Files' tab. 
        - These should be the same files and folders found on 
[CCRP_Climate_Futures](https://github.com/nationalparkservice/CCRP_Climate_Futures)

    
**Step 2: Create a new folder for park-specific data and make sure GitHub ignores it**

The parsing scripts are not functional in version 1 (due to remote work) so data must be parsed and placed appropriately in the './data/park-specific/input' folder before running the .Rmd script

*A note on file structure: The notation './' is used to refer to the project root directory. This is where your .RProj file is located.*

    - Note the folder structure in the lower right window. There should be a 'data' folder and within it a subfolder labeled 'general'
    - You will need to create another subfolder called 'park-specific'
    - Create folders within your repo for data by clicking on the 'New Folder' button at the top left of the lower right window.
    - Make sure you are inside the 'data' folder and create a new folder labeled 'park-specific'. If it is incorrectly named the script will not run. 
    - Select the 'Git' tab in the upper right window of RStudio
    - Make sure your new folder does not appear. If it does, make sure it is placed in the correct location and spelled exactly as written. Please contact Annie Kellner Dillon (anne_dillon@nps.gov) or Amber Runyon (amber_runyon@nps.gov) if anything appears under the Git tab at this point. 


**Step 3: Leave RStudio and use File Explorer to create data subfolders and add necessary data files**
*To request parsed data, see Amber Runyon at amber_runyon@nps.gov. To request other files see Annie Kellner Dillon (anne_dillon@nps.gov) or Amber Runyon*

*A zipped folder of all data is located at [CCRP_Climate_Futures_Data](https://doimspp.sharepoint.com/:u:/r/sites/NPS-CCRP-FCScienceAdaptation/Shared%20Documents/Reproducible%20Climate%20Futures/script%20rewrites/data.zip?csf=1&web=1&e=mneP82)

File names must be spelled exactly as they are written here or the scripts will not work. The notation '/' refers to a folder with multiple files (i.e. the .shp, .dbf, .prj etc. that comprise an ESRI shapefile)

    - Create the following subfolder within the ./data folder:
        - park-specific ('./data/park-specific')
        
    - Now, within the park-specific subfolder, create another subfolder called 'input' ('./data/park-specific/input')
    
    - Create the following subfolder within the ./data/general folder:
    
        - spatial-data ('./data/general/spatial-data') 
        
    - Place the following files in folders as follows (PARK is a placeholder for the four-letter park code - i.e., CONG):
        - './data/park-specific/input':
            - PARK_init_parsed.RData (normally created by MACA parsing script)
            - PARK_PRISM_PptTminTmax_IntermediateFiles.RData (this file is normally created by the PRISM parsing script: './scripts/PRISM/RSS PRISM AN81 4km crop summarize v01.1.R)
            - GridMet.csv (normally created by GridMet parsing script)
            
        - './data/general/spatial-data'
            - water_storage.tif 
            - elevation_cropped.tif
            - /US_Counties 
            - /State_Shapefile
            - /nps_boundary
            - /nps_boundary_centroids
            - /Climate_grid

*NOTE: 'general' data only has to be added one time for all CONUS parks. 'park-specific' input data will have to be added for every new park.* 

**Step 4: Install Java on your machine if you do not already have it. If you have trouble, see document* [Instructions_for_installing_Java_for_use_with_R.docx](https://doimspp.sharepoint.com/:w:/r/sites/NPS-CCRP-FCScienceAdaptation/_layouts/15/Doc.aspx?sourcedoc=%7B2E0D1A3C-7FA7-4E3E-BB4B-7943CCCAD699%7D&file=Instructions_for_installing_Java_for_use_with_R.docx&action=default&mobileredirect=true)

*Note: Java and rJava are only used for creating maps within the get_params script. All analyses can be run without them.*

    - Install Java from the following website: https://java.com/en/download/manual.jsp  
        - Make sure you install the correct version (i.e, 64-bit for 64-bit machines). You want to select the 'offline' version that matches your machine. 
        

**Step 5: Return to RStudio and tell R where to find Java**

    - Enter the following code to point R to the directory into which you installed Java:

            Sys.setenv(JAVA_HOME='path/to/jreX.X.X_XXX')  * check the program file for the jre number.

       * This is an example of where Java might be installed and what the code should look like: 
                Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_261')
                
**Step 6: Run the script!**

    - Make sure your new files and folders do not appear beneath the 'Git' tab
    - Enter park data and all parameters into the user-input chunk in the .Rmd script
    - Run the script! Select Run --> Run All or press Ctrl+Alt+R
    - The script will create all the necessary files and folders from here on out. You can find data output in the ./data/park-specific/output folder, and figures in the ./figures folder


## End instructions for running CCRP_Climate_Futures.Rmd
    

## The following describes the set of functions to parse historical (PRISM and GridMET) data and CMIP5 (MACA) projections, create data tables, and plot visualizations.

**1. Historical trends**

    - Script for parsing data; script for plotting data
**2. Climate Futures**

    - Script for parsing MACA; script for parsin GridMET; Plot table creation; scatterplots and diagnostics; plotting bar charts
**3. Water balance (package in own repository)**

    - Daily water balance
**4. Summary plots**

    - Summary plots (bias-corrected timeseries for temp and precip)
    - Summary WB (bias-corrected, run WB (monthly) for all GCMs in CFs; plot timeseries)
**5. Additoinal tables and plots** (these include scripts for plots that are regionally specific as well as query of Audubon results).

    - Drought plots - work with any park, but will mostly be used in western parks
    - Forest vulnerability - (FLI FBI plots) - These only cover eastern parks
    - Audubon - most parks included

