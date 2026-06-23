"""
Precompute PT passenger routes between stops for visualisation.
Step 1: Extract PT stop sequences from events
Step 2: Build road graph and run Dijkstra for each unique stop pair
Step 3: Save cache for use in visualisation

Run once, reuse many times.
Output: scenarios/basic/results_dmc/pt_routes_cache.json
"""

import gzip
import re
import json
import math
import heapq
from collections import defaultdict

NETWORK_FILE = "scenarios/basic/results_dmc/output_network.xml.gz"
EVENTS_FILE  = "scenarios/basic/results_dmc/output_events.xml.gz"
CACHE_FILE   = "scenarios/basic/results_dmc/pt_routes_cache.json"

def merc_to_wgs84(x, y):
    lon = x / 20037508.34 * 180
    lat = math.degrees(2 * math.atan(math.exp(y / 20037508.34 * math.pi)) - math.pi / 2)
    return round(lon, 6), round(lat, 6)

# ── 1. Load network: nodes + adjacency graph ─────────────────────────────────
print("Loading network...")
nodes = {}   # node_id -> (lon, lat, x, y)  -- keep raw x,y for distance calc
links = {}   # link_id -> (from_node, to_node, length)
graph = defaultdict(list)  # node_id -> [(neighbour_node, link_length, [waypoint_lons_lats])]

with gzip.open(NETWORK_FILE, "rt", encoding="utf-8") as f:
    content = f.read()

# Parse nodes
for m in re.finditer(r'<node id="([^"]+)" x="([^"]+)" y="([^"]+)"', content):
    nid, x, y = m.group(1), float(m.group(2)), float(m.group(3))
    lon, lat = merc_to_wgs84(x, y)
    nodes[nid] = {"lon": lon, "lat": lat, "x": x, "y": y}

# Parse links - only car-accessible ones
for m in re.finditer(r'<link id="([^"]+)" from="([^"]+)" to="([^"]+)" length="([^"]+)"[^>]*modes="([^"]*)"', content):
    lid, frm, to = m.group(1), m.group(2), m.group(3)
    length = float(m.group(4))
    modes = m.group(5)
    if frm in nodes and to in nodes:
        links[lid] = (frm, to, length)
        # Add to graph (directed)
        graph[frm].append((to, length))

print(f"  {len(nodes)} nodes, {len(links)} links, graph built")
del content

# ── 2. Nearest node lookup ────────────────────────────────────────────────────
def nearest_node(x, y):
    """Find nearest network node to a given EPSG:3857 coordinate."""
    best_id = None
    best_dist = float('inf')
    for nid, n in nodes.items():
        d = (n["x"] - x)**2 + (n["y"] - y)**2
        if d < best_dist:
            best_dist = d
            best_id = nid
    return best_id

# ── 3. Dijkstra ───────────────────────────────────────────────────────────────
def dijkstra(start_node, end_node):
    """Return list of [lon, lat] waypoints from start to end node."""
    if start_node == end_node:
        n = nodes[start_node]
        return [[n["lon"], n["lat"]]]

    dist = {start_node: 0}
    prev = {}
    pq = [(0, start_node)]

    while pq:
        d, u = heapq.heappop(pq)
        if d > dist.get(u, float('inf')):
            continue
        if u == end_node:
            break
        for v, w in graph[u]:
            nd = d + w
            if nd < dist.get(v, float('inf')):
                dist[v] = nd
                prev[v] = u
                heapq.heappush(pq, (nd, v))

    if end_node not in prev and end_node != start_node:
        # No path found — return straight line
        s, e = nodes[start_node], nodes[end_node]
        return [[s["lon"], s["lat"]], [e["lon"], e["lat"]]]

    # Reconstruct path
    path = []
    cur = end_node
    while cur in prev:
        path.append(cur)
        cur = prev[cur]
    path.append(start_node)
    path.reverse()

    return [[nodes[n]["lon"], nodes[n]["lat"]] for n in path]

# ── 4. Extract PT passenger stop sequences from events ────────────────────────
print("Extracting PT stop sequences from events...")

# person -> list of {time, x, y, type}
pt_stops = defaultdict(list)
current_person_mode = {}  # person -> current leg mode

with gzip.open(EVENTS_FILE, "rt", encoding="utf-8") as f:
    for line in f:
        line = line.strip()
        if not line.startswith("<event"):
            continue

        def attr(name):
            m = re.search(f'{name}="([^"]*)"', line)
            return m.group(1) if m else None

        etype = attr("type")
        if not etype:
            continue

        time = float(attr("time") or 0)

        if etype == "departure":
            person = attr("person")
            mode   = attr("legMode")
            if person and mode:
                current_person_mode[person] = mode

        elif etype == "actstart":
            person  = attr("person")
            act     = attr("actType")
            x_str   = attr("x")
            y_str   = attr("y")
            if person and act == "pt interaction" and x_str and y_str:
                pt_stops[person].append({
                    "t": time,
                    "x": float(x_str),
                    "y": float(y_str)
                })

        elif etype == "arrival":
            person = attr("person")
            mode   = attr("legMode")
            x_str  = attr("x") 
            y_str  = attr("y")
            link   = attr("link")
            if person and mode == "pt" and x_str and y_str:
                pt_stops[person].append({
                    "t": time,
                    "x": float(x_str),
                    "y": float(y_str)
                })

print(f"  {len(pt_stops)} PT passengers found")

# ── 5. Find unique stop pairs and run Dijkstra ────────────────────────────────
print("Computing routes between stop pairs...")

# Collect unique (x1,y1) -> (x2,y2) pairs
unique_pairs = set()
for person, stops in pt_stops.items():
    for i in range(len(stops)-1):
        s1, s2 = stops[i], stops[i+1]
        key = (round(s1["x"],0), round(s1["y"],0), round(s2["x"],0), round(s2["y"],0))
        unique_pairs.add(key)

print(f"  {len(unique_pairs)} unique stop pairs to route")

# Cache: "x1,y1->x2,y2" -> [[lon,lat], ...]
routes_cache = {}
total = len(unique_pairs)

for i, (x1, y1, x2, y2) in enumerate(unique_pairs):
    if i % 50 == 0:
        print(f"  Routing {i}/{total}...")

    n1 = nearest_node(x1, y1)
    n2 = nearest_node(x2, y2)
    path = dijkstra(n1, n2)

    key = f"{x1},{y1}->{x2},{y2}"
    routes_cache[key] = path

print(f"  Done — {len(routes_cache)} routes computed")

# ── 6. Build PT passenger trajectory data ────────────────────────────────────
print("Building PT passenger trajectories...")

pt_trajectories = {}  # person -> [{t, lon, lat}, ...]

for person, stops in pt_stops.items():
    traj = []
    for i in range(len(stops)-1):
        s1, s2 = stops[i], stops[i+1]
        key = f"{round(s1['x'],0)},{round(s1['y'],0)}->{round(s2['x'],0)},{round(s2['y'],0)}"
        path = routes_cache.get(key, [])

        if not path:
            lon1, lat1 = merc_to_wgs84(s1["x"], s1["y"])
            lon2, lat2 = merc_to_wgs84(s2["x"], s2["y"])
            path = [[lon1,lat1],[lon2,lat2]]

        n_pts = len(path)
        dt = (s2["t"] - s1["t"]) / max(1, n_pts - 1)
        for j, (lon, lat) in enumerate(path):
            traj.append({"t": round(s1["t"] + j*dt, 1), "lon": lon, "lat": lat})

    if traj:
        pt_trajectories[person] = traj

# ── 7. Save cache ─────────────────────────────────────────────────────────────
print(f"Saving cache to {CACHE_FILE}...")
output = {
    "routes": routes_cache,
    "pt_trajectories": pt_trajectories
}
with open(CACHE_FILE, "w") as f:
    json.dump(output, f)

size_mb = len(json.dumps(output)) / 1e6
print(f"\n✅ Done!")
print(f"   {len(routes_cache)} routes cached")
print(f"   {len(pt_trajectories)} PT passenger trajectories")
print(f"   Cache size: {size_mb:.1f} MB")
print(f"   Saved to: {CACHE_FILE}")
