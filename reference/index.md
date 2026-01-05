# Package index

## Geographical Detector Models

- [`gd()`](https://stscl.github.io/gdverse/reference/gd.md) : native
  geographical detector(GD) model
- [`opgd()`](https://stscl.github.io/gdverse/reference/opgd.md) :
  optimal parameters-based geographical detector(OPGD) model
- [`gozh()`](https://stscl.github.io/gdverse/reference/gozh.md) :
  geographically optimal zones-based heterogeneity(GOZH) model
- [`lesh()`](https://stscl.github.io/gdverse/reference/lesh.md) :
  locally explained stratified heterogeneity(LESH) model
- [`spade()`](https://stscl.github.io/gdverse/reference/spade.md) :
  spatial association detector (SPADE) model
- [`idsa()`](https://stscl.github.io/gdverse/reference/idsa.md) :
  interactive detector for spatial associations(IDSA) model
- [`rgd()`](https://stscl.github.io/gdverse/reference/rgd.md) : robust
  geographical detector(RGD) model
- [`rid()`](https://stscl.github.io/gdverse/reference/rid.md) : robust
  interaction detector(RID) model
- [`srsgd()`](https://stscl.github.io/gdverse/reference/srsgd.md) :
  spatial rough set-based geographical detector(SRSGD) model

## Compare Size Effect of Spatial Units

- [`sesu_opgd()`](https://stscl.github.io/gdverse/reference/sesu_opgd.md)
  : comparison of size effects of spatial units based on OPGD
- [`sesu_gozh()`](https://stscl.github.io/gdverse/reference/sesu_gozh.md)
  : comparison of size effects of spatial units based on GOZH

## Variable Discretization Methods

- [`gd_optunidisc()`](https://stscl.github.io/gdverse/reference/gd_optunidisc.md)
  : optimal univariate discretization based on geodetector q-statistic
- [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md)
  : discretization of variables based on recursive partitioning
- [`robust_disc()`](https://stscl.github.io/gdverse/reference/robust_disc.md)
  : univariate discretization based on offline change point detection
- [`cpsd_disc()`](https://stscl.github.io/gdverse/reference/cpsd_disc.md)
  : optimal spatial data discretization based on SPADE q-statistics

## Important Algorithm Functions

- [`spd_lesh()`](https://stscl.github.io/gdverse/reference/spd_lesh.md)
  : shap power of determinants

- [`loess_optscale()`](https://stscl.github.io/gdverse/reference/loess_optscale.md)
  : determine optimal spatial data analysis scale

- [`geodetector()`](https://stscl.github.io/gdverse/reference/geodetector.md)
  : geographical detector

- [`factor_detector()`](https://stscl.github.io/gdverse/reference/factor_detector.md)
  : factor detector

- [`interaction_detector()`](https://stscl.github.io/gdverse/reference/interaction_detector.md)
  : interaction detector

- [`risk_detector()`](https://stscl.github.io/gdverse/reference/risk_detector.md)
  : risk detector

- [`ecological_detector()`](https://stscl.github.io/gdverse/reference/ecological_detector.md)
  : ecological detector

- [`srs_geodetector()`](https://stscl.github.io/gdverse/reference/srs_geodetector.md)
  : spatial rough set-based geographical detector

- [`srs_factor_detector()`](https://stscl.github.io/gdverse/reference/srs_factor_detector.md)
  : spatial rough set-based factor detector

- [`srs_interaction_detector()`](https://stscl.github.io/gdverse/reference/srs_interaction_detector.md)
  : spatial rough set-based interaction detector

- [`srs_ecological_detector()`](https://stscl.github.io/gdverse/reference/srs_ecological_detector.md)
  : spatial rough set-based ecological detector

- [`gozh_detector()`](https://stscl.github.io/gdverse/reference/gozh_detector.md)
  : geographically optimal zones-based heterogeneity detector

- [`cpsd_spade()`](https://stscl.github.io/gdverse/reference/cpsd_spade.md)
  : compensated power of spatial determinant(CPSD)

- [`psd_spade()`](https://stscl.github.io/gdverse/reference/psd_spade.md)
  : power of spatial determinant(PSD)

- [`psmd_spade()`](https://stscl.github.io/gdverse/reference/psmd_spade.md)
  : power of spatial and multilevel discretization determinant(PSMD)

- [`psd_iev()`](https://stscl.github.io/gdverse/reference/psd_iev.md) :
  PSD of an interaction of explanatory variables (PSD-IEV)

- [`pid_idsa()`](https://stscl.github.io/gdverse/reference/pid_idsa.md)
  :

  IDSA Q-saistics `PID`

- [`psd_pseudop()`](https://stscl.github.io/gdverse/reference/psd_pseudop.md)
  : calculate power of spatial determinant(PSD) and the corresponding
  pseudo-p value

- [`psmd_pseudop()`](https://stscl.github.io/gdverse/reference/psmd_pseudop.md)
  : power of spatial and multilevel discretization determinant(PSMD) and
  the corresponding pseudo-p value

## Utility Functions

- [`gen_permutations()`](https://stscl.github.io/gdverse/reference/gen_permutations.md)
  : generate permutations
- [`weight_assign()`](https://stscl.github.io/gdverse/reference/weight_assign.md)
  : assign values by weight
- [`all2int()`](https://stscl.github.io/gdverse/reference/all2int.md) :
  convert all discretized vectors to integer
- [`F_informationloss()`](https://stscl.github.io/gdverse/reference/F_informationloss.md)
  : measure information loss by information entropy

## S3 Methods for Geographical Detector Models

- [`plot(`*`<ecological_detector>`*`)`](https://stscl.github.io/gdverse/reference/plot.ecological_detector.md)
  : plot ecological detector
- [`plot(`*`<factor_detector>`*`)`](https://stscl.github.io/gdverse/reference/plot.factor_detector.md)
  : plot factor detector result
- [`plot(`*`<gd_result>`*`)`](https://stscl.github.io/gdverse/reference/plot.gd_result.md)
  : plot GD result
- [`plot(`*`<gozh_result>`*`)`](https://stscl.github.io/gdverse/reference/plot.gozh_result.md)
  : plot GOZH result
- [`plot(`*`<idsa_result>`*`)`](https://stscl.github.io/gdverse/reference/plot.idsa_result.md)
  : plot IDSA risk result
- [`plot(`*`<interaction_detector>`*`)`](https://stscl.github.io/gdverse/reference/plot.interaction_detector.md)
  : plot interaction detector result
- [`plot(`*`<lesh_result>`*`)`](https://stscl.github.io/gdverse/reference/plot.lesh_result.md)
  : plot LESH model result
- [`plot(`*`<opgd_result>`*`)`](https://stscl.github.io/gdverse/reference/plot.opgd_result.md)
  : plot OPGD result
- [`plot(`*`<rgd_result>`*`)`](https://stscl.github.io/gdverse/reference/plot.rgd_result.md)
  : plot RGD result
- [`plot(`*`<rid_result>`*`)`](https://stscl.github.io/gdverse/reference/plot.rid_result.md)
  : plot RID result
- [`plot(`*`<risk_detector>`*`)`](https://stscl.github.io/gdverse/reference/plot.risk_detector.md)
  : plot risk detector
- [`plot(`*`<sesu_gozh>`*`)`](https://stscl.github.io/gdverse/reference/plot.sesu_gozh.md)
  : plot gozh sesu
- [`plot(`*`<sesu_opgd>`*`)`](https://stscl.github.io/gdverse/reference/plot.sesu_opgd.md)
  : plot opgd sesu
- [`plot(`*`<spade_result>`*`)`](https://stscl.github.io/gdverse/reference/plot.spade_result.md)
  : plot SPADE power of spatial and multilevel discretization
  determinant
- [`plot(`*`<srs_ecological_detector>`*`)`](https://stscl.github.io/gdverse/reference/plot.srs_ecological_detector.md)
  : plot spatial rough set-based ecological detector
- [`plot(`*`<srs_factor_detector>`*`)`](https://stscl.github.io/gdverse/reference/plot.srs_factor_detector.md)
  : plot spatial rough set-based factor detector result
- [`plot(`*`<srs_interaction_detector>`*`)`](https://stscl.github.io/gdverse/reference/plot.srs_interaction_detector.md)
  : plot spatial rough set-based interaction detector result
- [`plot(`*`<srsgd_result>`*`)`](https://stscl.github.io/gdverse/reference/plot.srsgd_result.md)
  : plot SRSGD result
- [`print(`*`<ecological_detector>`*`)`](https://stscl.github.io/gdverse/reference/print.ecological_detector.md)
  : print ecological detector
- [`print(`*`<factor_detector>`*`)`](https://stscl.github.io/gdverse/reference/print.factor_detector.md)
  : print factor detector
- [`print(`*`<gd_result>`*`)`](https://stscl.github.io/gdverse/reference/print.gd_result.md)
  : print GD result
- [`print(`*`<gozh_result>`*`)`](https://stscl.github.io/gdverse/reference/print.gozh_result.md)
  : print GOZH result
- [`print(`*`<idsa_result>`*`)`](https://stscl.github.io/gdverse/reference/print.idsa_result.md)
  : print IDSA result
- [`print(`*`<interaction_detector>`*`)`](https://stscl.github.io/gdverse/reference/print.interaction_detector.md)
  : print interaction detector
- [`print(`*`<lesh_result>`*`)`](https://stscl.github.io/gdverse/reference/print.lesh_result.md)
  : print LESH model interaction result
- [`print(`*`<opgd_result>`*`)`](https://stscl.github.io/gdverse/reference/print.opgd_result.md)
  : print OPGD result
- [`print(`*`<rgd_result>`*`)`](https://stscl.github.io/gdverse/reference/print.rgd_result.md)
  : print RGD result
- [`print(`*`<rid_result>`*`)`](https://stscl.github.io/gdverse/reference/print.rid_result.md)
  : print RID result
- [`print(`*`<risk_detector>`*`)`](https://stscl.github.io/gdverse/reference/print.risk_detector.md)
  : print risk detector
- [`print(`*`<sesu_gozh>`*`)`](https://stscl.github.io/gdverse/reference/print.sesu_gozh.md)
  : print gozh sesu
- [`print(`*`<sesu_opgd>`*`)`](https://stscl.github.io/gdverse/reference/print.sesu_opgd.md)
  : print opgd sesu
- [`print(`*`<spade_result>`*`)`](https://stscl.github.io/gdverse/reference/print.spade_result.md)
  : print SPADE power of spatial and multilevel discretization
  determinant
- [`print(`*`<srs_ecological_detector>`*`)`](https://stscl.github.io/gdverse/reference/print.srs_ecological_detector.md)
  : print spatial rough set-based ecological detector
- [`print(`*`<srs_factor_detector>`*`)`](https://stscl.github.io/gdverse/reference/print.srs_factor_detector.md)
  : print spatial rough set-based factor detector
- [`print(`*`<srs_interaction_detector>`*`)`](https://stscl.github.io/gdverse/reference/print.srs_interaction_detector.md)
  : print spatial rough set-based interaction detector
- [`print(`*`<srsgd_result>`*`)`](https://stscl.github.io/gdverse/reference/print.srsgd_result.md)
  : print SRSGD result

## Data

- [`NTDs`](https://stscl.github.io/gdverse/reference/NTDs.md) : NTDs
  data
- [`ndvi`](https://stscl.github.io/gdverse/reference/ndvi.md) : dataset
  of NDVI changes and its influencing factors
- [`sim`](https://stscl.github.io/gdverse/reference/sim.md) : Simulation
  data.
- [`srs_table`](https://stscl.github.io/gdverse/reference/srs_table.md)
  : example of spatial information system table
- [`srs_wt`](https://stscl.github.io/gdverse/reference/srs_wt.md) :
  example of spatial information system spatial adjacency matrix
