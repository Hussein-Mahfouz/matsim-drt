"""
Generate synthetic Leeds population for MATSim eqasim model.
Output: plans_sample_eqasim.xml
Coordinate system: EPSG:3857
NO vehicle attributes - these are added by NetworkVehicleInserter
"""

import random
import xml.etree.ElementTree as ET
from xml.dom import minidom

random.seed(42)

X_MIN, X_MAX = -189243.0, -144715.0
Y_MIN, Y_MAX = 7113543.0, 7151240.0
CENTRE_X = -169000.0
CENTRE_Y = 7132000.0
N_AGENTS = 1000

ACTIVITY_TYPES = ["work", "education", "shop", "escort", "visit", "medical", "other"]
ACTIVITY_WEIGHTS = [0.45, 0.15, 0.15, 0.08, 0.07, 0.05, 0.05]
ACTIVITY_DURATIONS = {
    "work": 8*3600, "education": 6*3600, "shop": 1*3600,
    "escort": 1*3600, "visit": 2*3600, "medical": 3*3600, "other": 2*3600,
}
MODES = ["car", "pt", "walk", "bike"]
MODE_WEIGHTS = [0.45, 0.30, 0.15, 0.10]


def random_coord():
    return random.uniform(X_MIN, X_MAX), random.uniform(Y_MIN, Y_MAX)


def biased_work_coord():
    x = max(X_MIN, min(X_MAX, random.gauss(CENTRE_X, 8000)))
    y = max(Y_MIN, min(Y_MAX, random.gauss(CENTRE_Y, 6000)))
    return x, y


def format_time(seconds):
    h = int(seconds // 3600)
    m = int((seconds % 3600) // 60)
    s = int(seconds % 60)
    return f"{h:02d}:{m:02d}:{s:02d}"


def make_person(person_id):
    age = random.randint(18, 75)
    has_licence = age >= 17 and random.random() < (0.85 if age > 25 else 0.4)
    car_avail = "no" if not has_licence else random.choices(["yes", "some", "no"], weights=[0.6, 0.25, 0.15])[0]
    bike_avail = "yes" if random.random() < 0.6 else "no"
    is_student = age < 25 and random.random() < 0.3
    is_passenger = random.random() < 0.1
    hh_income = float(random.randint(1, 10))
    ind_income = float(random.randint(1, 10))
    has_pt_subscription = random.random() < 0.3
    gender = random.choice(["male", "female"])
    hid = person_id // 3

    hx, hy = random_coord()
    act_type = random.choices(ACTIVITY_TYPES, weights=ACTIVITY_WEIGHTS)[0]
    depart_home = max(6*3600, int(random.gauss(8*3600, 0.5*3600)))
    act_duration = ACTIVITY_DURATIONS[act_type] + random.randint(-1800, 1800)
    end_act = depart_home + act_duration

    wx, wy = biased_work_coord() if act_type in ["work", "education"] else random_coord()

    mode = random.choices(MODES, weights=MODE_WEIGHTS)[0]
    if not has_licence and mode == "car":
        mode = "pt"
    if bike_avail == "no" and mode == "bike":
        mode = "walk"

    return {
        "id": str(person_id),
        "age": age,
        "hasLicence": has_licence,
        "CarAvailability": car_avail,
        "BicycleAvailability": bike_avail,
        "isStudent": is_student,
        "isPassenger": is_passenger,
        "outside": False,
        "hhlIncomeSPC": hh_income,
        "indIncomeSPC": ind_income,
        "hasPTSubscription": has_pt_subscription,
        "gender": gender,
        "hid": hid,
        "home": (hx, hy),
        "work": (wx, wy),
        "act_type": act_type,
        "depart_home": depart_home,
        "end_act": end_act,
        "mode": mode,
    }


def build_xml(agents):
    root = ET.Element("population")

    for a in agents:
        pid = a["id"]
        person = ET.SubElement(root, "person", id=pid)
        attrs_el = ET.SubElement(person, "attributes")

        def add_attr(name, cls, value):
            el = ET.SubElement(attrs_el, "attribute", name=name)
            el.set("class", cls)
            el.text = str(value).lower() if isinstance(value, bool) else str(value)

        # Only simple scalar attributes — NO vehicle map
        add_attr("hasLicence",          "java.lang.Boolean", a["hasLicence"])
        add_attr("CarAvailability",     "java.lang.String",  a["CarAvailability"])
        add_attr("BicycleAvailability", "java.lang.String",  a["BicycleAvailability"])
        add_attr("age",                 "java.lang.Integer", a["age"])
        add_attr("gender",              "java.lang.String",  a["gender"])
        add_attr("isStudent",           "java.lang.Boolean", a["isStudent"])
        add_attr("isPassenger",         "java.lang.Boolean", a["isPassenger"])
        add_attr("outside",             "java.lang.Boolean", a["outside"])
        add_attr("hhlIncomeSPC",        "java.lang.Double",  a["hhlIncomeSPC"])
        add_attr("indIncomeSPC",        "java.lang.Double",  a["indIncomeSPC"])
        add_attr("hasPTSubscription",   "java.lang.Boolean", a["hasPTSubscription"])
        add_attr("hid",                 "java.lang.Integer", a["hid"])

        # Plan
        plan = ET.SubElement(person, "plan", selected="yes")
        hx, hy = a["home"]
        wx, wy = a["work"]

        act_home = ET.SubElement(plan, "activity", type="home")
        act_home.set("x", f"{hx:.2f}")
        act_home.set("y", f"{hy:.2f}")
        act_home.set("end_time", format_time(a["depart_home"]))

        ET.SubElement(plan, "leg", mode=a["mode"])

        act_main = ET.SubElement(plan, "activity", type=a["act_type"])
        act_main.set("x", f"{wx:.2f}")
        act_main.set("y", f"{wy:.2f}")
        act_main.set("end_time", format_time(a["end_act"]))

        ET.SubElement(plan, "leg", mode=a["mode"])

        act_home2 = ET.SubElement(plan, "activity", type="home")
        act_home2.set("x", f"{hx:.2f}")
        act_home2.set("y", f"{hy:.2f}")

    return root


def prettify(elem):
    rough = ET.tostring(elem, encoding="unicode")
    reparsed = minidom.parseString(rough)
    return reparsed.toprettyxml(indent="\t")


if __name__ == "__main__":
    print(f"Generating {N_AGENTS} synthetic Leeds agents...")
    agents = [make_person(i) for i in range(N_AGENTS)]
    root = build_xml(agents)
    xml_str = prettify(root)
    xml_str = xml_str.replace(
        '<?xml version="1.0" ?>',
        '<?xml version="1.0" encoding="UTF-8"?>\n<!DOCTYPE population SYSTEM "http://www.matsim.org/files/dtd/population_v6.dtd">'
    )
    output_path = "src/main/resources/data/demand/plans_sample_eqasim.xml"
    with open(output_path, "w") as f:
        f.write(xml_str)
    print(f"Written to {output_path}")
    print(f"Mode split: { {m: sum(1 for a in agents if a['mode']==m) for m in MODES} }")
