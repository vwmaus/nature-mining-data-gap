# Mining Data Gap

This repository contains the R script used to produce the mining data gap map for a commentary in Nature.

## Usage

The script requires two datasets. The first consists of polygons that cover global mine areas, derived from satellite images acquired circa 2019 (Maus et al., 2022). This dataset will be automatically downloaded by the `main.R` script.

The second dataset consists of 6,201 coordinates of mining properties extracted from the SNL Metals & Mining Dataset by S&P Global Marketplace. This subset of mines includes all mines that have reported production at any time between 2000 and 2019. The script expects this data in the file `./data/snl2020.gpkg`. If the file has a different name, you can modify line 41 in `main.R`. This dataset is not included with the repository, as we do not have the rights to share it.

## Output

The script will create a global grid of 50 km x 50 km cells and check which cells contain a production data gap, i.e., mining areas identified in the satellite images but with no information on production in the SNL database. The final map, highlighting gap cells in red, will be written to the PDF file `./data/gap_map_50km_grid.pdf`.

## References

Maus, V., Giljum, S., da Silva, D.M., Gutschlhofer, J., da Rosa, R.P., Luckeneder, S., Gass, S.L., Lieber, M., and McCallum, I., 2022. An Update on Global Mining Land Use. Scientific Data, 9(1), pp.1-11.

Tang, L., and Werner, T.T., 2023. Global Mining Footprint Mapped from High-Resolution Satellite Imagery. Communications Earth & Environment, 4(1), p.134.
