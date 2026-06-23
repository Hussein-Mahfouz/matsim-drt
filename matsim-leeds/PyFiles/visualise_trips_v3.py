"""
MATSim Leeds Visualiser v3
- Car agents follow real road network links
- PT passengers follow precomputed Dijkstra routes between stops
- Walk/bike teleport (correct - schedule based)
- Fading trail behind each agent
- Basemap switcher
"""

import gzip
import json
import math
import re
import csv
from collections import defaultdict

NETWORK_FILE = "scenarios/basic/results_dmc/output_network.xml.gz"
EVENTS_FILE  = "scenarios/basic/results_dmc/output_events.xml.gz"
TRIPS_FILE   = "scenarios/basic/results_dmc/output_trips.csv.gz"
PT_CACHE     = "scenarios/basic/results_dmc/pt_routes_cache.json"
OUTPUT       = "scenarios/basic/results_dmc/leeds_trips_map_v3.html"

MODE_COLORS = {
    "car":           "#e74c3c",
    "pt":            "#3498db",
    "walk":          "#2ecc71",
    "bike":          "#f39c12",
    "car_passenger": "#9b59b6",
    "taxi":          "#1abc9c",
}

def merc_to_wgs84(x, y):
    lon = x / 20037508.34 * 180
    lat = math.degrees(2 * math.atan(math.exp(y / 20037508.34 * math.pi)) - math.pi / 2)
    return round(lon, 6), round(lat, 6)

def time_to_sec(t):
    try:
        p = t.split(":")
        return int(p[0])*3600 + int(p[1])*60 + int(p[2])
    except:
        return 0

# ── 1. Load network for car routing ─────────────────────────────────────────
print("Loading network...")
nodes = {}
links = {}

with gzip.open(NETWORK_FILE, "rt", encoding="utf-8") as f:
    content = f.read()

for m in re.finditer(r'<node id="([^"]+)" x="([^"]+)" y="([^"]+)"', content):
    nid, x, y = m.group(1), float(m.group(2)), float(m.group(3))
    nodes[nid] = merc_to_wgs84(x, y)

for m in re.finditer(r'<link id="([^"]+)" from="([^"]+)" to="([^"]+)"', content):
    lid, frm, to = m.group(1), m.group(2), m.group(3)
    if frm in nodes and to in nodes:
        links[lid] = (nodes[frm], nodes[to])

print(f"  {len(nodes)} nodes, {len(links)} links")
del content

# ── 2. Load PT cache ─────────────────────────────────────────────────────────
print("Loading PT route cache...")
with open(PT_CACHE) as f:
    pt_cache = json.load(f)
pt_trajectories = pt_cache["pt_trajectories"]
print(f"  {len(pt_trajectories)} PT passenger trajectories loaded")

# ── 3. Parse events for car trajectories ────────────────────────────────────
print("Parsing events for car trajectories...")
trajectories = defaultdict(list)
veh_to_person = {}
veh_current_link = {}

with gzip.open(EVENTS_FILE, "rt", encoding="utf-8") as f:
    for line in f:
        line = line.strip()
        if not line.startswith("<event"):
            continue

        def attr(name):
            m = re.search(f'{name}="([^"]*)"', line)
            return m.group(1) if m else None

        etype = attr("type")
        time  = float(attr("time") or 0)

        if etype == "PersonEntersVehicle":
            person = attr("person")
            veh    = attr("vehicle")
            if person and veh and not veh.startswith("tr_"):
                veh_to_person[veh] = person

        elif etype == "vehicle enters traffic":
            veh    = attr("vehicle")
            link   = attr("link")
            person = attr("person") or veh_to_person.get(veh)
            if person and link and link in links:
                veh_current_link[veh] = link
                lon, lat = links[link][0]
                trajectories[person].append({"t": time, "lon": lon, "lat": lat})

        elif etype == "entered link":
            veh    = attr("vehicle")
            link   = attr("link")
            person = veh_to_person.get(veh)
            if person and link and link in links:
                veh_current_link[veh] = link
                lon, lat = links[link][0]
                trajectories[person].append({"t": time, "lon": lon, "lat": lat})

        elif etype == "left link":
            veh    = attr("vehicle")
            link   = veh_current_link.get(veh)
            person = veh_to_person.get(veh)
            if person and link and link in links:
                lon, lat = links[link][1]
                trajectories[person].append({"t": time, "lon": lon, "lat": lat})

        elif etype == "arrival":
            person = attr("person")
            link   = attr("link")
            mode   = attr("legMode")
            if person and link and link in links and mode != "pt":
                lon, lat = links[link][1]
                trajectories[person].append({"t": time, "lon": lon, "lat": lat})

print(f"  {len(trajectories)} car agents with trajectories")

# ── 4. Load trips metadata ───────────────────────────────────────────────────
print("Loading trips...")
trips_meta = {}
with gzip.open(TRIPS_FILE, "rt") as f:
    reader = csv.DictReader(f, delimiter=";")
    for row in reader:
        pid  = row["person"]
        mode = row["main_mode"].strip()
        dep  = time_to_sec(row["dep_time"])
        arr  = dep + time_to_sec(row["trav_time"])
        sx, sy = float(row["start_x"]), float(row["start_y"])
        ex, ey = float(row["end_x"]),   float(row["end_y"])
        slon, slat = merc_to_wgs84(sx, sy)
        elon, elat = merc_to_wgs84(ex, ey)
        if pid not in trips_meta:
            trips_meta[pid] = []
        trips_meta[pid].append({
            "dep": dep, "arr": arr, "mode": mode,
            "sx": slon, "sy": slat, "ex": elon, "ey": elat
        })

# ── 5. Build agent list ──────────────────────────────────────────────────────
print("Building agent data...")
agents = []
all_persons = set(list(trajectories.keys()) + list(trips_meta.keys()) + list(pt_trajectories.keys()))

for pid in all_persons:
    meta_list = trips_meta.get(pid, [])
    if not meta_list:
        continue

    for trip in meta_list:
        mode  = trip["mode"]
        color = MODE_COLORS.get(mode, "#95a5a6")
        dep, arr = trip["dep"], trip["arr"]

        if mode == "pt":
            # Use precomputed PT trajectories
            pt_traj = pt_trajectories.get(pid, [])
            trip_pts = [p for p in pt_traj if dep - 60 <= p["t"] <= arr + 60]
            if trip_pts:
                waypoints = [[p["lon"], p["lat"], p["t"]] for p in trip_pts]
            else:
                # Fallback straight line
                waypoints = [
                    [trip["sx"], trip["sy"], dep],
                    [trip["ex"], trip["ey"], arr]
                ]

        elif mode in ("car", "car_passenger", "taxi"):
            traj = trajectories.get(pid, [])
            trip_pts = [p for p in traj if dep - 30 <= p["t"] <= arr + 30]
            if trip_pts:
                waypoints = [[p["lon"], p["lat"], p["t"]] for p in trip_pts]
            else:
                waypoints = [
                    [trip["sx"], trip["sy"], dep],
                    [trip["ex"], trip["ey"], arr]
                ]

        else:
            # Walk/bike — teleported, straight line with steps
            n = 8
            waypoints = [
                [round(trip["sx"] + i/n*(trip["ex"]-trip["sx"]),6),
                 round(trip["sy"] + i/n*(trip["ey"]-trip["sy"]),6),
                 round(dep + i/n*(arr-dep),1)]
                for i in range(n+1)
            ]

        if len(waypoints) < 2:
            waypoints = [[trip["sx"],trip["sy"],dep],[trip["ex"],trip["ey"],arr]]

        agents.append({
            "id": pid, "mode": mode, "color": color,
            "dep": dep, "arr": arr, "wp": waypoints
        })

print(f"  {len(agents)} agent-trips built")

all_lons = [a["wp"][0][0] for a in agents]
all_lats = [a["wp"][0][1] for a in agents]
centre_lon = sum(all_lons)/len(all_lons)
centre_lat = sum(all_lats)/len(all_lats)
agents_json = json.dumps(agents)

# ── 6. Write HTML ─────────────────────────────────────────────────────────────
print("Writing HTML...")

html = f"""<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8"/>
<title>Leeds MATSim — Agent Movements</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<style>
  * {{ margin:0; padding:0; box-sizing:border-box; }}
  body {{ font-family: Arial, sans-serif; background:#111; color:#eee; }}
  #map {{ width:100vw; height:100vh; }}
  #controls {{
    position:absolute; top:10px; left:50%; transform:translateX(-50%);
    z-index:1000; background:rgba(0,0,0,0.85); padding:10px 18px;
    border-radius:10px; display:flex; align-items:center; gap:12px;
    box-shadow:0 2px 16px rgba(0,0,0,0.6); flex-wrap:wrap; max-width:95vw;
  }}
  #timeDisplay {{ font-size:16px; font-weight:bold; min-width:55px; }}
  button {{
    background:#2980b9; color:#fff; border:none; padding:7px 16px;
    border-radius:5px; cursor:pointer; font-size:13px; font-weight:bold;
  }}
  button:hover {{ background:#3498db; }}
  select {{
    background:#222; color:#eee; border:1px solid #555;
    padding:5px 8px; border-radius:5px; font-size:12px; cursor:pointer;
  }}
  label {{ font-size:12px; display:flex; align-items:center; gap:6px; }}
  #legend {{
    position:absolute; bottom:20px; left:12px; z-index:1000;
    background:rgba(0,0,0,0.85); padding:12px 16px; border-radius:10px;
  }}
  .li {{ display:flex; align-items:center; gap:8px; margin:5px 0; font-size:12px; }}
  .dot {{ width:12px; height:12px; border-radius:50%; flex-shrink:0; }}

</style>
</head>
<body>
<div id="map"></div>
<div id="controls">
  <button id="playBtn" onclick="togglePlay()">▶ Play</button>
  <span id="timeDisplay">06:00</span>
  <input type="range" id="timeSlider" min="20000" max="90000" value="20000"
         style="width:200px" oninput="seek(+this.value)"/>
  <label>Speed <input type="range" id="speed" min="10" max="300" value="60" style="width:70px"/></label>
  <select id="basemapSelect" onchange="switchBasemap(this.value)">
    <option value="dark">Dark</option>
    <option value="positron">Light</option>
    <option value="voyager">Voyager</option>
    <option value="osm">OpenStreetMap</option>
    <option value="toner">Toner</option>
    <option value="toner_lite">Toner Lite</option>
    <option value="watercolor">Watercolor</option>
    <option value="topo">Topo</option>
  </select>
</div>
<div id="legend">
  <div class="li"><div class="dot" style="background:#e74c3c"></div>Car</div>
  <div class="li"><div class="dot" style="background:#3498db"></div>PT Bus</div>
  <div class="li"><div class="dot" style="background:#2ecc71"></div>Walk</div>
  <div class="li"><div class="dot" style="background:#f39c12"></div>Bike</div>
</div>

<script>
const AGENTS = {agents_json};
const TRAIL  = 10;

const API = '89392a1a-95be-44d6-bd9f-38a72c822f04';
const BASEMAPS = {{
  dark:       ['https://{{s}}.basemaps.cartocdn.com/dark_all/{{z}}/{{x}}/{{y}}{{r}}.png','&copy; OSM &copy; CARTO'],
  positron:   ['https://{{s}}.basemaps.cartocdn.com/light_all/{{z}}/{{x}}/{{y}}{{r}}.png','&copy; OSM &copy; CARTO'],
  voyager:    ['https://{{s}}.basemaps.cartocdn.com/rastertiles/voyager/{{z}}/{{x}}/{{y}}{{r}}.png','&copy; OSM &copy; CARTO'],
  osm:        ['https://{{s}}.tile.openstreetmap.org/{{z}}/{{x}}/{{y}}.png','&copy; OSM contributors'],
  toner:      [`https://tiles.stadiamaps.com/tiles/stamen_toner/{{z}}/{{x}}/{{y}}{{r}}.png?api_key=${{API}}`,'&copy; Stadia &copy; Stamen'],
  toner_lite: [`https://tiles.stadiamaps.com/tiles/stamen_toner_lite/{{z}}/{{x}}/{{y}}{{r}}.png?api_key=${{API}}`,'&copy; Stadia &copy; Stamen'],
  watercolor: [`https://tiles.stadiamaps.com/tiles/stamen_watercolor/{{z}}/{{x}}/{{y}}.jpg?api_key=${{API}}`,'&copy; Stadia &copy; Stamen'],
  topo:       ['https://{{s}}.tile.opentopomap.org/{{z}}/{{x}}/{{y}}.png','&copy; OSM, SRTM &copy; OpenTopoMap'],
}};

const map = L.map('map',{{center:[{centre_lat:.5f},{centre_lon:.5f}],zoom:12}});
let currentTile = L.tileLayer(BASEMAPS.dark[0],{{attribution:BASEMAPS.dark[1],subdomains:'abcd',maxZoom:20}}).addTo(map);

function switchBasemap(key) {{
  map.removeLayer(currentTile);
  currentTile = L.tileLayer(BASEMAPS[key][0],{{attribution:BASEMAPS[key][1],subdomains:'abcd',maxZoom:20}}).addTo(map);
  currentTile.bringToBack();
}}

let simTime=20000, playing=false, lastTs=null, raf=null;
const state={{}};
AGENTS.forEach((a,i)=>{{ state[i]={{marker:null,trail:[],trailLines:[]}}; }});

function secToHM(s){{
  return String(Math.floor(s/3600)).padStart(2,'0')+':'+String(Math.floor((s%3600)/60)).padStart(2,'0');
}}

function interp(wp,t){{
  if(!wp.length) return null;
  if(t<=wp[0][2]) return [wp[0][1],wp[0][0]];
  if(t>=wp[wp.length-1][2]) return null;
  for(let i=0;i<wp.length-1;i++){{
    if(t>=wp[i][2]&&t<=wp[i+1][2]){{
      const f=(t-wp[i][2])/Math.max(1,wp[i+1][2]-wp[i][2]);
      return [wp[i][1]+f*(wp[i+1][1]-wp[i][1]),wp[i][0]+f*(wp[i+1][0]-wp[i][0])];
    }}
  }}
  return null;
}}

function updateFrame(t){{
  AGENTS.forEach((a,i)=>{{
    const s=state[i];
    if(t>=a.dep&&t<=a.arr){{
      const pos=interp(a.wp,t);
      if(!pos) return;
      if(!s.marker){{
        s.marker=L.circleMarker(pos,{{radius:5,color:a.color,fillColor:a.color,fillOpacity:0.9,weight:0}})
          .addTo(map).bindTooltip(`${{a.mode}} | person ${{a.id}}`,{{sticky:true}});
      }} else {{ s.marker.setLatLng(pos); }}
      s.trail.push([...pos]);
      if(s.trail.length>TRAIL) s.trail.shift();
      s.trailLines.forEach(l=>map.removeLayer(l)); s.trailLines=[];
      for(let j=1;j<s.trail.length;j++){{
        s.trailLines.push(L.polyline([s.trail[j-1],s.trail[j]],{{
          color:a.color,weight:j<s.trail.length-1?1.5:2.5,opacity:(j/s.trail.length)*0.6
        }}).addTo(map));
      }}
    }} else {{
      if(s.marker){{map.removeLayer(s.marker);s.marker=null;}}
      s.trailLines.forEach(l=>map.removeLayer(l)); s.trailLines=[];s.trail=[];
    }}
  }});
  document.getElementById('timeDisplay').textContent=secToHM(t);
  document.getElementById('timeSlider').value=t;
}}

function step(ts){{
  if(!playing) return;
  if(!lastTs) lastTs=ts;
  const speed=+document.getElementById('speed').value;
  simTime+=(ts-lastTs)/1000*speed; lastTs=ts;
  if(simTime>90000) simTime=20000;
  updateFrame(simTime);
  raf=requestAnimationFrame(step);
}}

function togglePlay(){{
  playing=!playing;
  document.getElementById('playBtn').textContent=playing?'⏸ Pause':'▶ Play';
  if(playing){{lastTs=null;raf=requestAnimationFrame(step);}}
  else cancelAnimationFrame(raf);
}}

function seek(t){{simTime=t;updateFrame(t);}}
updateFrame(simTime);
</script>
</body>
</html>"""

with open(OUTPUT,"w") as f:
    f.write(html)

print(f"\n✅ Done!")
print(f"   open {OUTPUT}")
