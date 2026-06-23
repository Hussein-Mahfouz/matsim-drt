"""
Step 1 of TOID crosswalk:
Extract link_id -> osm_way_id mapping from MATSim network.xml.gz

Output: link_osm_crosswalk.csv
Columns:
  - link_id       : MATSim link ID
  - osm_way_id    : OSM way ID (stored as attribute in network.xml)
  - from_node     : MATSim from node ID
  - to_node       : MATSim to node ID
  - length        : link length in metres
  - freespeed     : freespeed in m/s
  - freespeed_mph : freespeed in mph
  - capacity      : flow capacity (veh/hr)
  - permlanes     : number of lanes
  - highway_type  : OSM highway type (e.g. residential, primary)
  - name          : road name if available
"""

import gzip
import xml.etree.ElementTree as ET
import csv
import os

# ── paths ──────────────────────────────────────────────────────────────────
NETWORK_FILE = "target/classes/data/supply/network.xml.gz"
OUTPUT_FILE  = "calibration/link_osm_crosswalk.csv"

# ── parse ───────────────────────────────────────────────────────────────────
print(f"Reading {NETWORK_FILE} ...")

rows = []

with gzip.open(NETWORK_FILE, 'rb') as f:
    tree = ET.parse(f)
    root = tree.getroot()

links_element = root.find('links')
if links_element is None:
    raise ValueError("No <links> element found in network.xml")

total = 0
matched = 0

for link in links_element.findall('link'):
    total += 1

    link_id    = link.get('id')
    from_node  = link.get('from')
    to_node    = link.get('to')
    length     = float(link.get('length', 0))
    freespeed  = float(link.get('freespeed', 0))
    capacity   = float(link.get('capacity', 0))
    permlanes  = float(link.get('permlanes', 1))

    # extract attributes
    osm_way_id    = None
    highway_type  = None
    name          = None

    attrs = link.find('attributes')
    if attrs is not None:
        for attr in attrs.findall('attribute'):
            attr_name = attr.get('name')
            if attr_name == 'osm:way:id':
                osm_way_id = attr.text
            elif attr_name == 'osm:way:highway':
                highway_type = attr.text
            elif attr_name == 'osm:way:name':
                name = attr.text

    if osm_way_id is not None:
        matched += 1

    rows.append({
        'link_id'      : link_id,
        'osm_way_id'   : osm_way_id,
        'from_node'    : from_node,
        'to_node'      : to_node,
        'length_m'     : round(length, 4),
        'freespeed_ms' : round(freespeed, 6),
        'freespeed_mph': round(freespeed * 2.23694, 2),
        'capacity'     : capacity,
        'permlanes'    : permlanes,
        'highway_type' : highway_type,
        'name'         : name,
    })

# ── write CSV ───────────────────────────────────────────────────────────────
fieldnames = [
    'link_id', 'osm_way_id', 'from_node', 'to_node',
    'length_m', 'freespeed_ms', 'freespeed_mph',
    'capacity', 'permlanes', 'highway_type', 'name'
]

with open(OUTPUT_FILE, 'w', newline='', encoding='utf-8') as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(rows)

# ── summary ─────────────────────────────────────────────────────────────────
print(f"\n✅ Done!")
print(f"   Total links       : {total:,}")
print(f"   With osm_way_id   : {matched:,} ({100*matched/total:.1f}%)")
print(f"   Without osm_way_id: {total-matched:,}")
print(f"   Output saved to   : {OUTPUT_FILE}")

# ── quick freespeed breakdown by highway type ────────────────────────────────
print(f"\n── Freespeed summary by highway type ──────────────────────────────")
from collections import defaultdict
type_speeds = defaultdict(list)
for r in rows:
    if r['highway_type']:
        type_speeds[r['highway_type']].append(r['freespeed_mph'])

for htype, speeds in sorted(type_speeds.items()):
    avg = sum(speeds) / len(speeds)
    print(f"   {htype:<20} n={len(speeds):>6,}  avg freespeed={avg:.1f} mph")
