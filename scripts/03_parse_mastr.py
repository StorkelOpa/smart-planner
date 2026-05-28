import xml.etree.ElementTree as ET
import csv
import os
import sys
import gzip

def parse_wind_xml(xml_path, csv_path):
    print(f"Reading and parsing XML: {xml_path}")
    print("This will extract relevant wind turbine data...")
    
    # Ensure output directory exists
    os.makedirs(os.path.dirname(csv_path), exist_ok=True)
    
    # Fields we want to extract from each <EinheitWind>
    fields = [
        "EinheitMastrNummer",
        "Gemeindeschluessel",
        "Landkreis",
        "Gemeinde",
        "Laengengrad",
        "Breitengrad",
        "Inbetriebnahmedatum",
        "EinheitBetriebsstatus",
        "Bruttoleistung",
        "Nettonennleistung",
        "Lage"
    ]
    
    count = 0
    is_gzip = xml_path.endswith(".gz")
    open_func = lambda: gzip.open(xml_path, "rb") if is_gzip else open(xml_path, "r", encoding="utf-8")
    
    try:
        with open(csv_path, "w", newline="", encoding="utf-8") as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=fields)
            writer.writeheader()
            
            with open_func() as xml_file:
                # Using iterparse for memory efficiency (O(1) memory usage)
                context = ET.iterparse(xml_file, events=("end",))
                
                for event, elem in context:
                    if elem.tag == "EinheitWind":
                        row = {}
                        for field in fields:
                            val = elem.find(field)
                            row[field] = val.text if val is not None else ""
                        
                        writer.writerow(row)
                        count += 1
                        
                        if count % 10000 == 0:
                            print(f"  Processed {count} turbines...")
                            
                        # Clear the element to prevent memory build-up
                        elem.clear()
                        
        print(f"Successfully finished parsing! Extracted {count} wind turbines.")
        print(f"Saved clean dataset to: {csv_path}")
        
    except Exception as e:
        print(f"Error parsing XML file: {e}", file=sys.stderr)

if __name__ == "__main__":
    XML_FILE = "Gesamtdatenexport_20240101_23.1/EinheitenWind.xml"
    XML_GZ_FILE = "data/EinheitenWind.xml.gz"
    CSV_FILE = "data/wind_turbines.csv"
    
    if os.path.exists(XML_GZ_FILE):
        parse_wind_xml(XML_GZ_FILE, CSV_FILE)
    elif os.path.exists(XML_FILE):
        parse_wind_xml(XML_FILE, CSV_FILE)
    else:
        print(f"Error: Could not find XML file at {XML_GZ_FILE} or {XML_FILE}")
        sys.exit(1)

