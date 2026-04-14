"""
Just exploring the dataset for now, running this takes a second. Yields an interactive map of the
"""

import geopandas as gpd
from pathlib import Path
import dtale

# Path management
project_root = Path(__file__).resolve().parents[1]
data_path = project_root / "data"
output_path = project_root / "outputs"
output_path.mkdir(exist_ok=True)

# Data files
global_mine_polygons = data_path / "Global_mine_polygons_74726" / "Global_mine_polygons_74726.shp"
cshapes = data_path / "cshapes_0.6" / "cshapes.shp"

# Load data
mines = gpd.read_file(global_mine_polygons)
countries = gpd.read_file(cshapes)

# Map mine status to labels
status_map = {
    1: "active",
    0: "undefined",
    -1: "closed"
}
mines["status_label"] = mines["status"].map(status_map).fillna("unknown")

# Latest country polygons
#TODO: Update data - 2016??
countries_latest = (
    countries.sort_values(["CNTRY_NAME", "GWSYEAR"])
    .groupby("CNTRY_NAME", as_index=False)
    .tail(1)
)

# Base map: countries
m = countries_latest.explore(
    color=None,
    style_kwds={"fillOpacity": 0.0, "weight": 0.4, "color": "gray"},
    tooltip=False,
    popup=False,
    highlight=False,
    name="Countries",
    tiles="CartoDB positron"
)

# Mine overlay
mines.explore(
    m=m,
    column="status_label",
    categorical=True,
    categories=["active", "undefined", "closed"],
    cmap=["red", "yellow", "blue"],
    legend=True,
    tooltip=["status_label", "country_na", "wld_rgn", "Hectare", "NDVI", "BSP", "NTL"],
    popup=True,
    style_kwds={"weight": 0.6, "fillOpacity": 0.8},
    name="Mine polygons"
)

# Save HTML
#out_file = output_path / "global_mines_interactive.html"
#m.save(out_file)


# Quick metadata check
d = dtale.show(mines)
print(d._main_url)

input("D-Tale is running. Press Enter to stop it...")

