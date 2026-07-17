"""Build a geocoded dataset of DoD fire/crash training installations.

Pipeline:
  1. Parse List-of-military-fire-and-crash-training-sites-2014.pdf (KBCRS
     inventory) into site rows, collapse to unique installations.
  2. Attach coordinates from EWG's geocoded version of the same list
     (ewg_crashsites_2019NOV26.geojson, 305 sites) by state-aware fuzzy match.
  3. Fall back to MIRTA (DoD installation points) for anything EWG missed.

Output: PFAS_Project_Data/military/fire_training_sites_geocoded.csv
Requires: pip install pypdf rapidfuzz pandas
"""
import json
import re

import pandas as pd
from pypdf import PdfReader
from rapidfuzz import process, fuzz

PDF = "List-of-military-fire-and-crash-training-sites-2014.pdf"
EWG = "PFAS_Project_Data/military/ewg_crashsites_2019NOV26.geojson"
MIRTA = "PFAS_Project_Data/military/mirta_-223606765265040761.csv"
OUT = "PFAS_Project_Data/military/fire_training_sites_geocoded.csv"

COMPONENTS = ["Army", "Navy", "Air Force", "Marine Corps", "DLA", "NGB",
              "Defense Logistics Agency", "Washington Headquarters Services"]
STATES = ["Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
    "Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas",
    "Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
    "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey",
    "New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
    "Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
    "Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming",
    "Puerto Rico","Guam","Virgin Islands","American Samoa","District of Columbia",
    "Northern Mariana Islands","Johnston Atoll","Wake Island","Midway Islands"]
FUND = r"(?:DERA|BRAC|FUDS|O&M|OTHER|DSMOA)"

# joint-base / renamed-site aliases the fuzzy matcher can't bridge on its own
ALIASES = {
    "jber elmendorf": "joint base elmendorf richardson",
    "jber richardson": "joint base elmendorf richardson",
    "jbmdl dix": "joint base mcguire dix lakehurst",
    "jbmdl lakehurst": "joint base mcguire dix lakehurst",
    "jbmdl mcguire": "joint base mcguire dix lakehurst",
    "jbsa fort sam houston": "joint base san antonio",
    "jbsa lackland": "joint base san antonio",
    "jbsa randolph": "joint base san antonio",
    "jble eustis": "joint base langley eustis",
    "jble langley": "joint base langley eustis",
    "jb charleston air": "joint base charleston",
    "galena fol": "galena forward operating location",
    "north penn usarc": "north penn us army reserve command",
    "san diego faswtc pac": "san diego fleet anti submarine training center",
}


def parse_pdf():
    text = "\n".join(p.extract_text() for p in PdfReader(PDF).pages)
    flat = re.sub(r"\s+", " ", text)
    comp_pat = "|".join(sorted(COMPONENTS, key=len, reverse=True))
    state_pat = "|".join(sorted(STATES, key=len, reverse=True))
    rec_re = re.compile(rf"({comp_pat}) ({state_pat}) (.*?) ({FUND}) ", re.I)
    rows = []
    for m in rec_re.finditer(flat):
        inst = re.sub(r"(DoD Inventory.*|Page \d+ of \d+.*|Print Date.*)", "",
                      m.group(3)).strip()
        if inst:
            rows.append({"component": m.group(1), "state": m.group(2),
                         "installation": inst})
    fire = pd.DataFrame(rows)
    return (fire.groupby(["component", "state", "installation"], as_index=False)
                .size().rename(columns={"size": "n_fire_sites"}))


def clean(x):
    x = str(x).lower()
    # EWG names sometimes glue words together ("FALLONNAVAL AIR STATION")
    x = re.sub(r"(?<=[a-z])(naval|marine)", r" \1", x)
    x = re.sub(r"[^a-z0-9 ]", " ", x)
    x = re.sub(r"\b(ft|fort)\b", "fort", x)
    x = re.sub(r"\b(afb|air force base)\b", "afb", x)
    x = re.sub(r"\b(naval air station)\b", "nas", x)
    x = re.sub(r"\b(mcas|marine corps? air station)\b", "mcas", x)
    x = re.sub(r"\b(nwirp|naval weapons industrial reserve plant)\b", "nwirp", x)
    x = re.sub(r"\b(ncts|naval computer and telecommunications station)\b", "ncts", x)
    x = re.sub(r"\b(nawc|naval air warfare center)\b", "nawc", x)
    x = re.sub(r"\b(air national guard|ang)\b", "ang", x)
    x = re.sub(r"\s+", " ", x).strip()
    return ALIASES.get(x, x)


def attach(inst, ref, name_col, state_col, lon_col, lat_col, label, cutoff=80):
    """State-aware fuzzy match; fills lon/lat/coord_source where still missing."""
    ref = ref.copy()
    ref["clean"] = ref[name_col].apply(clean)
    ref["state_l"] = ref[state_col].astype(str).str.strip().str.lower()
    for i, row in inst[inst["lon"].isna()].iterrows():
        pool = ref[ref["state_l"] == row["state"].lower()]
        if pool.empty:
            pool = ref
        m = process.extractOne(clean(row["installation"]), pool["clean"].tolist(),
                               scorer=fuzz.token_set_ratio, score_cutoff=cutoff)
        if m:
            hit = pool[pool["clean"] == m[0]].iloc[0]
            inst.loc[i, ["lon", "lat", "coord_source", "matched_name", "match_score"]] = [
                hit[lon_col], hit[lat_col], label, hit[name_col], m[1]]
    return inst


def main():
    inst = parse_pdf()
    for c in ["lon", "lat", "matched_name"]:
        inst[c] = pd.NA
    inst["coord_source"] = pd.NA
    inst["match_score"] = pd.NA

    ewg = pd.json_normalize(json.load(open(EWG))["features"])
    ewg.columns = [c.replace("properties.", "") for c in ewg.columns]
    inst = attach(inst, ewg, "basename", "state", "Longitude", "Latitude", "EWG_2019")

    mirta = pd.read_csv(MIRTA)
    mirta.columns = [c.strip() for c in mirta.columns]
    ABBR = {"alabama":"al","alaska":"ak","arizona":"az","arkansas":"ar","california":"ca",
        "colorado":"co","connecticut":"ct","delaware":"de","florida":"fl","georgia":"ga",
        "hawaii":"hi","idaho":"id","illinois":"il","indiana":"in","iowa":"ia","kansas":"ks",
        "kentucky":"ky","louisiana":"la","maine":"me","maryland":"md","massachusetts":"ma",
        "michigan":"mi","minnesota":"mn","mississippi":"ms","missouri":"mo","montana":"mt",
        "nebraska":"ne","nevada":"nv","new hampshire":"nh","new jersey":"nj",
        "new mexico":"nm","new york":"ny","north carolina":"nc","north dakota":"nd",
        "ohio":"oh","oklahoma":"ok","oregon":"or","pennsylvania":"pa","rhode island":"ri",
        "south carolina":"sc","south dakota":"sd","tennessee":"tn","texas":"tx","utah":"ut",
        "vermont":"vt","virginia":"va","washington":"wa","west virginia":"wv",
        "wisconsin":"wi","wyoming":"wy","puerto rico":"pr","guam":"gu",
        "district of columbia":"dc","virgin islands":"vi"}
    # MIRTA stores 2-letter codes; map installation states to codes for the join
    inst_state_code = inst["state"].str.lower().map(ABBR).fillna(inst["state"])
    tmp = inst.assign(state=inst_state_code)
    tmp = attach(tmp, mirta, "Site Name", "State Name Code", "x", "y", "MIRTA")
    inst[["lon", "lat", "coord_source", "matched_name", "match_score"]] = \
        tmp[["lon", "lat", "coord_source", "matched_name", "match_score"]]

    inst.to_csv(OUT, index=False)
    n = len(inst)
    got = inst["lon"].notna().sum()
    print(f"{got}/{n} installations geocoded "
          f"({inst.coord_source.value_counts().to_dict()})")
    print(f"wrote {OUT}")
    miss = inst[inst["lon"].isna()]
    if len(miss):
        print(f"\nstill missing ({len(miss)}):")
        print(miss[["component", "state", "installation"]].to_string(index=False))


if __name__ == "__main__":
    main()
