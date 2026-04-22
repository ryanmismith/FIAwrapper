# FIAwrapper

**Clean forest inventory tree lists from FIA data, in forestry language.**

FIAwrapper transforms raw USDA Forest Inventory and Analysis (FIA) database
exports into clean, imperial-unit tree lists using the terminology foresters
actually use on the ground. This package removes the need to navigate database
variables like `CONDPROP_UNADJ`, `TPA_UNADJ`, `SPCD`, or `STATUSCD` and translates
FIA data into familiary terminology like `dbh`, `height`, `tpa`, `species`, and `status`.

```r
# install.packages("devtools")
devtools::install_github("ryanmismith/FIAwrapper")

library(FIAwrapper)
set_fvs_variant("NE")

fia       <- fetch_fia_data(states = "RI")
tree_list <- build_tree_list(fia)
```

The result is a single tibble, one row per tree, ready to hand to a
silviculturist, drop into FVS, or feed into a growth model.

---

## How FIAwrapper Builds on rFIA

[`rFIA`](https://cran.r-project.org/package=rFIA) is the established R
interface to FIA data, and it is excellent at what it was built for:
downloading the database and producing **population-level estimates**
(tables of TPA, BAA, volume, biomass, etc. for a state, county, or ownership).

FIAwrapper is a complementary tool, not a replacement. It sits *above* rFIA
and focuses on a different job:

| Need | Use rFIA | Use FIAwrapper |
|------|----------|----------------|
| Download FIA tables for a state | ✓ (FIAwrapper calls it) | ✓ (wraps rFIA) |
| State-wide population estimates (EVALIDator-style) | ✓ | ✗ |
| Tree-level analytical tables in forestry units | limited | ✓ |
| Tree lists ready for FVS | ✗ | ✓ |
| Plot-level summaries for a user-drawn AOI | ✗ | ✓ |
| Remeasurement time series linked by tree | ✗ | ✓ |
| Competition indices for growth modeling | ✗ | ✓ |
| Outlier flagging on biological bounds | ✗ | ✓ |
| Enrichment with terrain / climate / soils APIs | ✗ | ✓ |

In short: **rFIA answers "what does the FIA estimate for this
population?"; FIAwrapper answers "give me a clean, tree-level dataset I
can actually model, simulate, or report on."**

FIAwrapper uses `rFIA::getFIA()` as its preferred download backend (falling
back to the FIA DataMart when rFIA is not installed), then does its own
work on the raw tables to produce analyst-friendly outputs.

---

## What the Package Does

- **Tree lists** — `build_tree_list()` joins TREE / PLOT / COND, computes
  per-acre expansion factors honoring FIA's subplot/microplot design and
  `CONDPROP_UNADJ`, translates species codes, and renames columns to
  forestry terms.
- **Spatial selection** — `get_plots_in_area()` filters plots by shapefile
  or bounding box (sf-first, with a reticulate/geopandas fallback).
- **FVS export** — write FVS-ready tree lists and stand metadata for any
  FVS variant (NE, SN, LS, CS, PN, WC, BM, CR, EM, IE, SO, AK).
- **Time series** — `build_time_series()` links remeasured trees via
  `PREV_TRE_CN` to track growth, ingrowth, and mortality between cycles.
- **Plot / stand summaries** — per-plot TPA, BAA, QMD, dominant species,
  stocking, and species composition in imperial units.
- **Growth modeling prep** — competition indices, regeneration stocking,
  and variance estimates for sampling error on any attribute.
- **Quality control** — `flag_outliers()` checks trees against
  biologically plausible height/DBH/crown bounds.
- **Site enrichment** — pull terrain (USGS), climate (PRISM), and soils
  (SSURGO) from federal APIs for any plot location.
- **Units and naming** — everything imperial; `fia_to_forestry_names()` /
  `forestry_to_fia_names()` translate in both directions.

See the full package guide in [`vignettes/FIAwrapper_guide.qmd`](vignettes/FIAwrapper_guide.qmd).

---

## Why It's Designed This Way

**Forestry terminology over database jargon.** FIA column names are
optimized for database normalization, not for the people who actually use
the data. A silviculturist should not need to memorize that `DIA` is DBH,
`HT` is height, `STATUSCD == 1` means live, and `TPA_UNADJ` has to be
adjusted by `CONDPROP_UNADJ`. FIAwrapper does that translation once,
correctly, so downstream code reads like forestry instead of SQL.

**Imperial units by default.** FIA mixes units (inches for DBH, feet for
height, metric for some biomass fields). FIAwrapper standardizes on
imperial everywhere because that is what the target users — practicing
US foresters, forest managers, and FVS users — work in. Metric helpers
are available for users who need them.

**Tree-level, not population-level.** rFIA and EVALIDator already produce
excellent population estimates. The gap is a clean tree-level table that
analysts can hand to FVS, to a taper equation, to a growth model, or into
a report about a specific ownership or AOI. That gap is what FIAwrapper
fills.

**sf-first, Python as a fallback.** Spatial operations use R's `sf`
package when available. A reticulate/geopandas backend exists only for
environments where `sf` is hard to install (e.g., locked-down Windows
boxes). Most users will never touch Python. If you don't need spatial
filtering, Python is never required.

**FVS variant as a session setting.** The same FIA species code maps to
different FVS alpha codes depending on the variant. Rather than thread a
variant argument through every function, FIAwrapper uses a single
session-level `set_fvs_variant()` call so every downstream function —
tree lists, FVS exports, species lookups — stays consistent.

**Wraps, does not replace.** Where rFIA already does something well
(downloads, on-disk caching), FIAwrapper calls into it rather than
reinventing it. This keeps the package focused on the
translation/analysis layer where its value is.

---

## Limitations

- **Not a replacement for rFIA's population estimators.** If you need
  EVALIDator-style design-based estimates at state, county, or ownership
  scale, use rFIA's `tpa()`, `biomass()`, `growMort()`, etc. FIAwrapper's
  `estimate_variance()` is for sampling error on custom attributes, not a
  full replacement for the FIA evaluation/EVALID framework.
- **Fuzzed coordinates.** Public FIA plot coordinates are intentionally
  offset by up to ~1 mile for landowner privacy. They are fine for
  regional analysis and polygon-based filtering, but should not be used
  for precise site-level work. Exact coordinates require a
  [confidential data agreement](https://www.fia.fs.usda.gov/tools-data/spatial/index.php)
  with the FIA program.
- **FVS variant species coverage varies.** A species present in your data
  may not have a mapping in every variant. Unmapped species fall back to
  a generic "OT" (other) FVS code, which you should review before
  running simulations.
- **FIADB schema is a moving target.** FIAwrapper tracks FIADB v9.2.
  Future schema changes may require updates. If a column has been renamed
  or dropped in a newer release, file an issue.
- **US only.** FIA is a US program. FIAwrapper has no applicability to
  Canadian, Mexican, or overseas inventory systems.
- **Assumes standard 4-subplot plot design.** The per-acre expansion
  factors baked into `compute_tpa()` assume the standard FIA Phase 2 plot
  layout. Non-standard designs (Phase 3, experimental intensifications)
  may need custom handling.
- **Optional API dependencies.** Terrain, climate, and soils enrichment
  functions hit federal APIs that may be rate-limited, temporarily
  unavailable, or require network access. They are optional — the core
  tree-list and FVS-export workflows work fully offline once data is
  cached.

---

## Getting Started

```r
library(FIAwrapper)

# 1. Set your FVS variant (controls species code mapping)
set_fvs_variant("NE")

# 2. Get data — tries rFIA, falls back to the FIA DataMart
fia <- fetch_fia_data(states = c("ME", "NH"),
                      tables = c("TREE", "PLOT", "COND", "SUBPLOT"))

# 3. Build a clean tree list
trees <- build_tree_list(fia)

# 4. Summarize at the plot level
plots <- summarize_plots(trees)

# 5. Export to FVS
export_fvs(trees, file = "stand.fvs")
```

A bundled example dataset `fia_ri` (12 Rhode Island plots, 2 cycles, 467
trees, 15 species) is available for exploring the package without a
download:

```r
data(fia_ri)
trees <- build_tree_list(fia_ri)
```

The full guide (installation, plot design background, every function,
and end-to-end workflows) is in
[`vignettes/FIAwrapper_guide.qmd`](vignettes/FIAwrapper_guide.qmd).

---

## Citation and References

- Bechtold, W. A., & Patterson, P. L. (2005). *The Enhanced Forest
  Inventory and Analysis Program — National Sampling Design and
  Estimation Procedures*. Gen. Tech. Rep. SRS-80.
- Burrill, E. A., et al. (2024). *The Forest Inventory and Analysis
  Database: Database Description and User Guide for Phase 2, v9.2*.
  USDA Forest Service.
- Stanke, H., Finley, A. O., Weed, A. S., Walters, B. F., & Domke, G. M.
  (2020). *rFIA: An R package for estimation of forest attributes with
  the US Forest Inventory and Analysis database*. Environmental Modelling
  & Software, 127, 104664.

### Related tools

- [rFIA](https://cran.r-project.org/package=rFIA) — R interface and
  population estimators for FIA data (used as a backend here).
- [FIA DataMart](https://apps.fs.usda.gov/fia/datamart/) — raw FIA data.
- [EVALIDator](https://apps.fs.usda.gov/fiadb-api/evalidator) — web tool
  for design-based FIA estimates. Useful for cross-checking.
- [Forest Vegetation Simulator](https://www.fs.usda.gov/fvs/whatis.shtml)
  — growth-and-yield simulator; FIAwrapper produces input-ready files.

---

## License

MIT © Ryan Smith. See [LICENSE](LICENSE).

## Contributing

Issues and pull requests are welcome at
<https://github.com/ryanmismith/FIAwrapper>. When reporting a bug, please
include the output of `sessionInfo()` and, if possible, a minimal
reproducible example using the bundled `fia_ri` dataset.
