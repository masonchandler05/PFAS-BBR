# =============================================================================
# download_data.R
# -----------------------------------------------------------------------------
# Fetches every large public dataset used by this project from its official
# source into PFAS_Project_Data/. Run this once after cloning the repository,
# from the repository root:
#
#     Rscript download_data.R
#
# Small / non-scriptable files (CWNS, DoD, MIRTA) ship inside the repo, so this
# script does not need to fetch them. The two R packages that pull data on demand
# (nhdplusTools for NHDPlus, tigris for Census ZCTAs) download what they need the
# first time the analysis scripts run.
#
# Requires `curl` and `unzip` on PATH (preinstalled on macOS/Linux). Total
# download is ~5.5 GB; expect this to take a while. Re-running skips files that
# already exist.
#
# See PFAS_Project_Data/DATA_SOURCES.md for citations and exact URLs.
# =============================================================================

root <- "PFAS_Project_Data"
for (d in c("ucmr5", "ucmr3", "echo", "frs", "tri"))
  dir.create(file.path(root, d), showWarnings = FALSE, recursive = TRUE)

# Download with curl (resumable; reliable for multi-GB files). Skips if present.
fetch <- function(url, dest) {
  if (file.exists(dest)) { message("✓ exists, skipping: ", dest); return(invisible()) }
  message("↓ downloading: ", url)
  code <- system2("curl", c("-L", "-C", "-", "--fail", "-o", shQuote(dest), shQuote(url)))
  if (code != 0 || !file.exists(dest)) stop("download failed: ", url)
}

# Extract specific members of a zip directly into a target dir (junk paths).
unzip_members <- function(zip, members, outdir) {
  args <- c("-o", "-j", shQuote(zip), shQuote(members), "-d", shQuote(outdir))
  if (system2("unzip", args) != 0) stop("unzip failed: ", zip)
}

tmp <- tempfile(fileext = ".zip")

# ── 1. UCMR5 (PFAS outcomes, 2023–2025) ──────────────────────────────────────
if (!file.exists(file.path(root, "ucmr5/UCMR5_All.csv"))) {
  fetch("https://www.epa.gov/system/files/other-files/2023-08/ucmr5-occurrence-data.zip", tmp)
  unzip_members(tmp, c("UCMR5_All.csv", "UCMR5_ZIPCodes.txt", "UCMR5_AddtlDataElem.txt"),
                file.path(root, "ucmr5"))
}

# ── 2. UCMR3 (PFAS outcomes, 2013–2015; DiD panel) ───────────────────────────
if (!file.exists(file.path(root, "ucmr3/UCMR3_All.txt"))) {
  fetch("https://www.epa.gov/system/files/other-files/2024-04/ucmr3-occurrence-data.zip", tmp)
  unzip_members(tmp, "UCMR3_All.txt", file.path(root, "ucmr3"))
}

# ── 3. ECHO Exporter (water-system coordinates; ~2 GB) ───────────────────────
if (!file.exists(file.path(root, "echo/ECHO_EXPORTER.csv"))) {
  fetch("https://echo.epa.gov/files/echodownloads/echo_exporter.zip", tmp)
  unzip_members(tmp, "ECHO_EXPORTER.csv", file.path(root, "echo"))
}

# ── 4. FRS National Combined (facilities, NAICS, start dates; ~3 GB) ──────────
if (!file.exists(file.path(root, "frs/NATIONAL_FACILITY_FILE.CSV"))) {
  fetch("https://ordsext.epa.gov/FLA/www3/state_files/national_combined.zip", tmp)
  unzip_members(tmp, c("*NATIONAL_FACILITY_FILE.CSV",
                       "*NATIONAL_NAICS_FILE.CSV",
                       "*NATIONAL_ENVIRONMENTAL_INTEREST_FILE.CSV"),
                file.path(root, "frs"))
}

# ── 5. TRI Basic Data Files 2022 & 2023 (PFAS releases; served as CSV) ────────
fetch("https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/2022_US/csv",
      file.path(root, "tri/tri_2022_us.csv"))
fetch("https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/2023_US/csv",
      file.path(root, "tri/tri_2023_us.csv"))

unlink(tmp)
message("\nDone. CWNS, DoD, and MIRTA files are included in the repository.")
message("NHDPlus (nhdplusTools) and Census ZCTAs (tigris) download on first use.")
