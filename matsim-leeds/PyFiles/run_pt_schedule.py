"""
PT Schedule Generator for MATSim Leeds
=======================================
Generates a PT schedule for a specified day of the week.

Usage:
    python run_pt_schedule.py --day monday
    python run_pt_schedule.py --day tuesday
    python run_pt_schedule.py --day wednesday
    python run_pt_schedule.py --day thursday
    python run_pt_schedule.py --day friday
    python run_pt_schedule.py --day saturday
    python run_pt_schedule.py --day sunday

This will:
1. Pick a representative date for the chosen day (term time, Sep 2026)
2. Run Gtfs2TransitSchedule to generate the unmapped schedule
3. Run PT Mapper to generate the mapped schedule
4. Update the simulation config to use the new schedule
"""

import argparse
import subprocess
import os
import sys
import gzip
import shutil

# ── Representative dates (term time week, Sep 2026) ──────────────────────────
DAY_DATES = {
    'monday'    : '20260921',
    'tuesday'   : '20260922',
    'wednesday' : '20260923',
    'thursday'  : '20260924',
    'friday'    : '20260925',
    'saturday'  : '20260926',
    'sunday'    : '20260927',
}

# ── Paths ─────────────────────────────────────────────────────────────────────
JAR             = 'target/matsim-leeds-1.0.jar'
GTFS_FILE       = 'src/main/resources/data/supply/gtfs_bus.zip'
NETWORK_FILE    = 'src/main/resources/data/supply/network.xml.gz'
PT_MAPPER_CONFIG= 'src/main/resources/data/supply/pt_mapper_config.xml'
SUPPLY_DIR      = 'src/main/resources/data/supply'
CRS             = 'EPSG:3857'

def get_paths(day):
    return {
        'schedule_unmapped' : f'{SUPPLY_DIR}/schedule_{day}_unmapped.xml.gz',
        'vehicles_unmapped' : f'{SUPPLY_DIR}/vehicles_{day}_unmapped.xml',
        'schedule_mapped'   : f'{SUPPLY_DIR}/schedule_{day}_mapped.xml.gz',
        'network_mapped'    : f'{SUPPLY_DIR}/network_{day}_mapped.xml.gz',
    }

def run_gtfs2schedule(day, date, paths):
    """Step 1: GTFS → unmapped transit schedule"""
    print(f"\n── Step 1: GTFS → Schedule ({day} = {date}) ───────────────────")
    
    cmd = [
        'java', '-cp', JAR,
        'org.matsim.pt2matsim.run.Gtfs2TransitSchedule',
        GTFS_FILE,
        date,
        CRS,
        paths['schedule_unmapped'],
        paths['vehicles_unmapped'],
    ]
    
    print(f"Running: {' '.join(cmd)}")
    result = subprocess.run(cmd, capture_output=False)
    
    if result.returncode != 0:
        print(f"❌ Gtfs2TransitSchedule failed for {day}")
        sys.exit(1)
    
    print(f"✅ Schedule generated: {paths['schedule_unmapped']}")

def update_pt_mapper_config(day, paths):
    """Update pt_mapper_config.xml to use day-specific files"""
    import re

    with open(PT_MAPPER_CONFIG, 'r') as f:
        content = f.read()

    # read current values to replace
    input_match  = re.search(r'(name="inputScheduleFile"[^v]*value=")([^"]+)(")', content)
    output_match = re.search(r'(name="outputScheduleFile"[^v]*value=")([^"]+)(")', content)
    network_match= re.search(r'(name="outputNetworkFile"[^v]*value=")([^"]+)(")', content)

    if input_match:
        content = content.replace(input_match.group(2), paths['schedule_unmapped'])
    if output_match:
        content = content.replace(output_match.group(2), paths['schedule_mapped'])
    if network_match:
        content = content.replace(network_match.group(2), paths['network_mapped'])

    temp_config = f'{SUPPLY_DIR}/pt_mapper_config_{day}.xml'
    with open(temp_config, 'w') as f:
        f.write(content)

    return temp_config

def run_pt_mapper(day, paths):
    """Step 2: Unmapped schedule → Mapped schedule"""
    print(f"\n── Step 2: PT Mapper ───────────────────────────────────────────")
    
    temp_config = update_pt_mapper_config(day, paths)
    
    cmd = [
        'java', '-Xmx8g', '-cp', JAR,
        'com.husseinmahfouz.matsim.pt2matsim.RunPTMapper',
        temp_config,
    ]
    
    print(f"Running PT Mapper for {day}...")
    result = subprocess.run(cmd, capture_output=False)
    
    # clean up temp config
    os.remove(temp_config)
    
    if result.returncode != 0:
        print(f"❌ PT Mapper failed for {day}")
        sys.exit(1)
    
    print(f"✅ Mapped schedule: {paths['schedule_mapped']}")
    print(f"✅ Mapped network:  {paths['network_mapped']}")

def update_sim_config(day, paths):
    """Step 3: Update simulation config to use the day-specific schedule"""
    import xml.etree.ElementTree as ET
    
    # find sim config
    configs = [
        'src/main/resources/config_simulation_dmc.xml',
        'src/main/resources/config_simulation_dmc_drt.xml',
    ]
    
    print(f"\n── Step 3: Updating simulation config ──────────────────────────")
    
    for config_path in configs:
        if not os.path.exists(config_path):
            continue
        
        with open(config_path, 'r') as f:
            content = f.read()
        
        # replace schedule file reference
        import re
        # match any existing schedule_*_mapped or schedule_mapped
        content_new = re.sub(
            r'(value=")([^"]*schedule[^"]*mapped[^"]*\.xml\.gz)(")',
            f'\\1{paths["schedule_mapped"]}\\3',
            content
        )
        # replace network_mapped reference  
        content_new = re.sub(
            r'(value=")([^"]*network[^"]*mapped[^"]*\.xml\.gz)(")',
            f'\\1{paths["network_mapped"]}\\3',
            content_new
        )
        
        with open(config_path, 'w') as f:
            f.write(content_new)
        
        print(f"✅ Updated: {config_path}")
        print(f"   transitScheduleFile → {paths['schedule_mapped']}")
        print(f"   network             → {paths['network_mapped']}")

def check_already_exists(day, paths):
    """Check if schedule already generated for this day"""
    if os.path.exists(paths['schedule_mapped']):
        print(f"⚠️  Mapped schedule for {day} already exists: {paths['schedule_mapped']}")
        answer = input("   Regenerate? (y/n): ").strip().lower()
        return answer != 'y'
    return False

def main():
    parser = argparse.ArgumentParser(
        description='Generate MATSim PT schedule for a specific day of the week'
    )
    parser.add_argument(
        '--day',
        required=True,
        choices=list(DAY_DATES.keys()),
        help='Day of week: monday, tuesday, wednesday, thursday, friday, saturday, sunday'
    )
    parser.add_argument(
        '--skip-gtfs',
        action='store_true',
        help='Skip GTFS conversion (use existing unmapped schedule)'
    )
    parser.add_argument(
        '--skip-mapper',
        action='store_true',
        help='Skip PT mapping (use existing mapped schedule)'
    )
    parser.add_argument(
        '--config-only',
        action='store_true',
        help='Only update simulation config, skip schedule generation'
    )
    
    args = parser.parse_args()
    day  = args.day
    date = DAY_DATES[day]
    paths = get_paths(day)
    
    print(f"{'='*60}")
    print(f"  MATSim Leeds — PT Schedule Generator")
    print(f"  Day:  {day.capitalize()}")
    print(f"  Date: {date}")
    print(f"{'='*60}")
    
    # check jar exists
    if not os.path.exists(JAR):
        print(f"❌ JAR not found: {JAR}")
        print("   Run: mvn clean package -DskipTests")
        sys.exit(1)
    
    if not args.config_only:
        # check if already exists
        if not check_already_exists(day, paths):
            if not args.skip_gtfs:
                run_gtfs2schedule(day, date, paths)
            if not args.skip_mapper:
                run_pt_mapper(day, paths)
    
    update_sim_config(day, paths)
    
    print(f"\n{'='*60}")
    print(f"  ✅ Done! Ready to simulate {day.capitalize()}")
    print(f"  Run the simulation with:")
    print(f"  java -Xmx8g -cp {JAR} \\")
    print(f"    com.husseinmahfouz.matsim.dmc.RunDMCSimulation \\")
    print(f"    --config-path src/main/resources/config_simulation_dmc.xml")
    print(f"{'='*60}")

if __name__ == '__main__':
    main()
