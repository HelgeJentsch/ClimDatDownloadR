---
title: 'ClimDatDownloadR: Accessing Climate Data Repositories for Modelling'
tags:
  - Climate Data
  - Chelsa
  - WorldClim
  - Species Distribution Modelling
  - Ecological Niche Modelling
  - Easy Access Geodata

authors:
  - name: Helge Jentsch
    orcid: 0000-0003-4955-8358
    equal-contrib: true
    corresponding: true
    affiliation: '1, 2' # (Multiple affiliations must be quoted)
  - name: Johannes Weidinger
    orcid: 0000-0003-4354-7711
    equal-contrib: true
    corresponding: false
    affiliation: 2
  - name: Melanie Werner
    orcid: 0009-0005-4965-7024
    equal-contrib: true
    corresponding: false
    affiliation: 2
  - name: Maria Bobrowski
    orcid: 0000-0002-9660-1614
    equal-contrib: true
    corresponding: false
    affiliation: 2

affiliations:
  - name: Climate Geography (CliG), Albert-Ludwigs-Universität Freiburg, Freiburg i. Breisgau, Germany
    index: 1
    ror: 0245cg223
  - name: Centrum for Earth System Research and Sustainability (CEN), University of Hamburg, Hamburg, Germany
    index: 2
    ror: 00g30e956

date: 08 August 2025
bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:

# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing

aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

# Summary

Systematical accessing, downloading, and pre-processing climatological data from CHELSA [@karger_Climatologies_2021; @karger_Data_2018; @karger_Climatologies_2017] and WorldClim [@fick_WorldClim_2017; @hijmans_Very_2005] remains a challenge in different environmental disciplines like Species Distribution Modelling (SDM) and climate studies. This package provides a set of functions that allow easy access and customized selection of climate data sets. Besides downloading the raw data, also functionalities to complete pre-processing steps like clipping, rescaling, and file management are available. The applications of the package range from one-time-use to implementing the functions in automatic processing of scientific workflows.

# Statement of need

The climatology datasets CHELSA and WorldClim contribute as crucial data bases for studies in various scientific fields. Primarily used in studies with focus on ecology (\~4,200 publications[^1]), environmental sciences (\>2,200 publications), and biodiversity conservation (\>1,600 publications), usages extend to a wide variety of scientific disciplines. The main usage of the datasets, however, lies in Species Distribution Modelling (SDM) and Ecological Niche Modelling (ENM). Their free availability and frequent citation in widely referenced papers on SDM and ENM strategies [e.g., @RANDIN2020111626; @Zurell.2020] have contributed to their widespread adoption, facilitating comparability between modelling studies at different spatial and temporal scales.

[^1]: Following the Web of Science Categories, citations of @karger_Data_2018 (Data from CHELSA 2.1) had 1,155 citations in the field of Ecology. The WorldClim 2 data [@fick_WorldClim_2017] has 3,044 citations in the same Web of Science category. Both numbers are of the date 17.05.2025. The "Web of Science Categories are assigned at the journal level", meaning the publishing journal defines the category ([source](https://support.clarivate.com/ScientificandAcademicResearch/s/article/Web-of-Science-Core-Collection-Web-of-Science-Categories?language=en_US)).

The high resolution global climatological datasets (30 arc-sec. ~ 1km) include downscaled and bias-corrected data from 30-year time-periods, providing always monthly mean, minimum, and maximum values of temperature and monthly precipitation sums for analysis[^2]. Additionally, [19 bioclimatic parameters](https://www.worldclim.org/data/bioclim.html) are accessible, which enable conclusions about seasonality.

[^2]: Function `Chelsa.timeseries.download` supports also the download of potential evapotranspiration (PET) from CHELSA 2.1 [@karger_Data_2018]

Since their initial releases in 2018 (CHELSA V1.2), the CHELSA [@karger_Climatologies_2017; @karger_Data_2018] datasets were cited in more than 2,800 peer reviewed papers, indexed on the Web of Science ([source, Aug. 2025](https://www.webofscience.com/wos/woscc/summary/1910f819-cfa2-430f-8e84-8882fbb25463-014c01bbb6/date-descending/1)). The latest release of WorldClim 2 in 2017 [@fick_WorldClim_2017] was cited more than 10,600 times ([source, Aug. 2025](https://www.webofscience.com/wos/woscc/summary/97f360a0-0e1e-4bd1-bf33-c34083cb6c8c-014c017f6b/date-descending/1)).

CHELSA and WorldClim datasets are commonly utilized in models predicting the potential past, current, and future distribution of species, particularly in studies on monitoring distribution shifts under climate change [e.g., @bobrowski_Modelling_2017; @twala_Projecting_2023; @werner_Treeline_2025], tracking endangered species and planning conservation strategies [e.g., @Franklin.2013; @Muscatello.2021], assessing the spread of invasive species [@srivastava_Species_2019], and management strategies in forestry and agriculture [e.g., @agbezuge_Application_2024; @PECCHI2019108817].

Recent studies have also assessed the performance of these datasets in SDM/ENM approaches, highlighting their respective strengths and limitations [e.g., @bobrowski_new_2021; @datta_Origin_2020; @MARIA201792; @BOBROWSKI2021109693;@rodriguez-rey_Differing_2024]. Given that dataset performance may vary depending on the research scope, it is recommended to test multiple datasets to to ensure their suitability for the research target and region.

For these applications, `ClimDatDownloadR` offers key advantages by enabling efficient retrieval from both dataset providers and pre-processing steps such as partial selection of parameters, months, and bioclimatic parameters, temporal subsets of timeseries, customized extent, and included file management as well as an output of the provider's respective citation file. In addition to time-saving aspects, the storage usage and management played a key role in the development of the `ClimDatDownloadR`.

The implemented data management creates a hierarchical, clear, and reproducible data structure for analyses during the processing. Downloaded data can be kept as is, deleted, or packed in a zip-archive file. All of raised _ease-of-use_ add-ons contribute to the primary goal of `ClimDatDownloadR` to enable more scientists and other users or organisations to download and pre-process CHELSA and WorldClim data to gain more experience in geodata handling and applications.

Since the official release in 2023, the use of `ClimDatDownloadR` steadily increased [@bobrowski_new_2021;@twala_Projecting_2023;@maitner_global_2023;@Santi_etal_2024;@costa-saura_Are_2025;@werner_Treeline_2025;@chen_PhyloControl_2025]. Further, the need of having software for downloading and pre-processing of freely available data is shown by the steady stream of interested visitors on [ResearchGate (3,399 unique visits, 04.08.2025)](https://www.researchgate.net/publication/370497166_R-package_ClimDatDownloadR_WorldClim_and_CHELSA_download_and_preprocessing), [Zenodo (>1000 views, > 150 downloads)](https://zenodo.org/records/8319614) [@jentsch_ClimDatDownloadR_2023], and citations in peer-reviewed papers.

The package implements the datasets CHELSA V1.2 , V2.1, WorldClim V1.4, and V2.1. More specifically the CHELSA Climatologies, Timeseries, CRU Timeseries (CHELSAcruts), and WorldClim Histclim datasets for present data. For past data, the CHELSA PIMP3 data from CHELSA V1.2 is also available. For future data, both CHELSA and WorldClim provide datasets incorporating various CMIP 5 and 6 global circulation models with various emission scenarios and reference periods. An overview as well as a introduction to the usage of the functions is provided in the [Readme of the package on GitHub](https://github.com/HelgeJentsch/ClimDatDownloadR?tab=readme-ov-file).

# Acknowledgements

We acknowledge the thorough testing efforts by the co-authors and Nadine Kaul, and the open access efforts by the Eidg. Forschungsanstalt für Wald, Schnee und Landschaft WSL [Chelsa; @karger_Climatologies_2017] and University of California, Davis [WorldClim; @fick_WorldClim_2017] working groups. Also we want to acknowledge the wide user group that implemented the `ClimDatDownloadR` in their unpublished scientific work or motivated us to implement new datasets or update broken links.
In addition we want to thank the developers of the various `R`-packages for their contributions that made this package possible. In alphabetical order these were: `curl` [@ooms_curl_2025], `httr` [@wickham_httr_2023], `ncdf4` [@pierce_ncdf4_2024], `RCurl` [@templelang_RCurl_2025], `RefManageR` [@mclean_Straightforward_2014; @mclean_RefManageR_2017], `sf` [@pebesma_Simple_2018; @pebesma_Spatial_2023], `stringr` [@wickham_stringr_2023], `sp` [@bivand_Applied_2013; @pebesma_Classes_2005], `terra` [@hijmans_terra_2025], and the development team of `R` [@rcoreteam_language_2025].

# References
