# =============================================================================
# naics_tiers.R  --  single source of truth for NAICS PFAS-likelihood tiering
# -----------------------------------------------------------------------------
# Sourced by naics_experiments.R, analysis_naics_weighted.R, and (optionally)
# clean_data_construction.R so every script uses ONE definition of which NAICS
# codes are credible PFAS sources and how strongly to weight them.
#
# tier 1 = high confidence (direct PFAS manufacture/use, strong evidence)
# tier 2 = medium (plausible secondary user or waste pathway)
# tier 3 = low / speculative (generic code, weak or no direct PFAS link)
#
# epa_regulated = codes EPA is ACTIVELY writing PFAS effluent-limit rules for
#   (OCPSF PFAS mfrs/formulators, metal finishing, chromium electroplating,
#    landfills) -- strongest possible regulatory evidence of real water discharge.
#
# Grounding: Salvatore et al. 2022 (Presumptive Contamination), EPA Effluent
# Guidelines Program Plans 15 & 16, EPA Multi-Industry PFAS Study 2021,
# MN PCA / MI EGLE metal-finishing findings.
# =============================================================================

suppressMessages(library(tibble))

naics_meta <- tribble(
  ~code,     ~sector,       ~tier, ~description,
  "325199",  "chemical",        1L, "All other basic organic chemical mfg (fluorochemicals)",
  "325211",  "chemical",        1L, "Plastics material & resin mfg (fluoropolymers, PTFE)",
  "325998",  "chemical",        1L, "All other misc chemical product & prep (PFAS formulation)",
  "325510",  "chemical",        1L, "Paint & coating mfg (fluoropolymer coatings)",
  "325611",  "chemical",        2L, "Soap & detergent mfg",
  "325612",  "chemical",        2L, "Polish & other sanitation good mfg",
  "332813",  "metal",           1L, "Electroplating, plating, anodizing (chrome PFOS mist suppressant)",
  "332812",  "metal",           1L, "Metal coating, engraving, heat treating",
  "332999",  "metal",           3L, "All other misc fabricated metal product mfg",
  "488119",  "airport",         1L, "Other airport operations (AFFF firefighting foam)",
  "562212",  "waste",           1L, "Solid waste landfill (leachate reservoir)",
  "562211",  "waste",           2L, "Hazardous waste treatment & disposal",
  "562213",  "waste",           2L, "Solid waste combustors & incinerators",
  "562112",  "waste",           2L, "Hazardous waste collection",
  "562219",  "waste",           2L, "Other nonhazardous waste treatment & disposal",
  "314110",  "textile",         1L, "Carpet & rug mills (stain-repellent treatments)",
  "313320",  "textile",         1L, "Fabric coating mills (water/stain repellents)",
  "313310",  "textile",         1L, "Textile & fabric finishing mills",
  "313210",  "textile",         2L, "Broadwoven fabric mills",
  "313230",  "textile",         2L, "Nonwoven fabric mills",
  "313110",  "textile",         2L, "Fiber, yarn & thread mills",
  "313220",  "textile",         2L, "Narrow fabric mills",
  "314910",  "textile",         2L, "Textile bag & canvas mills",
  "316110",  "textile",         2L, "Leather & hide tanning & finishing (water repellents)",
  "322121",  "paper",           2L, "Paper (except newsprint) mills (grease-resistant)",
  "322130",  "paper",           2L, "Paperboard mills",
  "322220",  "paper",           1L, "Paper bag & coated/treated paper mfg (direct grease-proofing application)",
  "322219",  "paper",           2L, "Other paperboard container mfg",
  "326113",  "plastics",        2L, "Unlaminated plastics film & sheet mfg",
  "326112",  "plastics",        2L, "Plastics packaging film & sheet mfg",
  "334413",  "electronics",     2L, "Semiconductor & related device mfg (fluorinated etchants)",
  "334419",  "electronics",     2L, "Other electronic component mfg",
  "335999",  "electronics",     3L, "All other misc electrical equipment mfg",
  "324110",  "petroleum",       2L, "Petroleum refineries",
  "324191",  "petroleum",       3L, "Petroleum lubricating oil & grease mfg",
  "323111",  "printing",        3L, "Commercial / gravure printing",
  "323120",  "printing",        3L, "Support activities for printing",
  "333318",  "other",           3L, "Other commercial & service industry machinery mfg",
  "424690",  "wholesale",       3L, "Other chemical & allied products merchant wholesalers"
)

tier_weight         <- c(`1` = 1.0, `2` = 0.5, `3` = 0.15)
epa_regulated_codes <- c("325199", "325211", "325998", "325510",
                         "332812", "332813", "562212")

naics_meta$weight        <- tier_weight[as.character(naics_meta$tier)]
naics_meta$epa_regulated <- naics_meta$code %in% epa_regulated_codes

# -----------------------------------------------------------------------------
# build_weighted_idw(): exponential inverse-distance weight summed over the k
# nearest facilities, each facility scaled by a per-facility weight.
#   dmat    water x facilities distance matrix (meters); cols align to `weights`
#   weights per-facility weight (length == ncol(dmat)); 0 = excluded
#   lambda  decay (m); k = number of nearest facilities to sum
# Equal weights + all facilities reproduces the base idw_10().
# -----------------------------------------------------------------------------
build_weighted_idw <- function(dmat, weights, lambda = 10000, k = 10) {
  stopifnot(ncol(dmat) == length(weights))
  keep <- which(weights > 0)
  if (length(keep) == 0) return(rep(NA_real_, nrow(dmat)))
  sub  <- dmat[, keep, drop = FALSE]
  w    <- weights[keep]
  vapply(seq_len(nrow(sub)), function(i) {
    d <- as.numeric(sub[i, ])
    o <- order(d)[seq_len(min(k, length(d)))]
    sum(w[o] * exp(-d[o] / lambda))
  }, numeric(1))
}
