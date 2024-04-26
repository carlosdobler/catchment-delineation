# Catchment delineation

Context: We use the [CWatM model](https://cwatm.iiasa.ac.at/index.html) to evaluate hydrological risks. Several model outputs need to be aggregated at a catchment level. To obtain the catchments, we could use an existing product - like HydroSHEDS's [HydroBASINS product](https://www.hydrosheds.org/products/hydrobasins). The problem is that this product does not align perfectly with the layers that CWatM uses as inputs, specifically the local drainage direction and upstream (accumulated) flow maps. Because they don't align, when overlaid with these maps, HydroBASINS catchments present "false" inflow and/or outflow points.

Objective: Delineate catchments with the same layers that CWatM uses as inputs. This will ensure these catchments will be consistent with CWatM outputs and will not present false inflow or outflow points. Catchments should have roughly the same size and spatial configuration as HydroBASINS ones.

The script in this repo conducts a test on the Mississippi River.
