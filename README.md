# GDIF-damageprediction

This repository contains the source code and *some* of the data to apply the geospatial data integration framework (G-DIF) to data from the 2015 Nepal Earthquake and replicate key results from the following paper:

Loos, Sabine, David Lallemant, Jack Baker, Jamie McCaughey, Sang-Ho Yun, Nama Budhathoki, Feroz Khan, and Ritika Singh. “G-DIF: A Geospatial Data Integration Framework to Rapidly Estimate Post-Earthquake Damage (In Review).” *Earthquake Spectra*, 2019. 

Since we will be updating the code over time, to find the specific version associated with this paper please see the zip files located on the Stanford Digital Repository:  https://purl.stanford.edu/gn368cq4893.

## Licensing and availability

The code and the data are both licensed under the CC-by-NC-SA license. Feel free to use either based on the terms and conditions listed in the LICENSE.md file in the code and data's respective folders and reference the above paper. We intend this code to be used for NON-COMMERCIAL uses, if you'd like to use either for commercial uses, please contact Sabine Loos at  [sloos@stanford.edu](mailto::sloos@stanford.edu).

### Data availability

Because the data used in this paper at this resolution is proprietary to the Government of Nepal, we cannot make the data available at this time. However, we urge users to recreate the analysis using the open data listed in the "GDIF_nb.Rmd" file. All data on the impact of the 2015 Nepal earthquake has been made openly available at the ward-level at [http://eq2015.npc.gov.np/#/](http://eq2015.npc.gov.np/#/).



## Using the code

The interactive code for this paper can be found at  https://sabineloos.github.io/GDIF-damageprediction/GDIF_nb.html.

If you would like to develop this yourself, the markdown titled "GDIF_nb.Rmd" stitches all functions together. To knit this file, run the "run.sh" file in your command line.

### Required Operating Environment

This code was developed using R version 3.5.2. All necessary packages for this code to run are included in the "install.R" file. You can see the session info for this code at the bottom of the "GDIF_nb.Rmd".

Copyright (c) Sabine Loos, 2020.