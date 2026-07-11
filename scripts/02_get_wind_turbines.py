# ==============================================================================
# SCHRITT 2a: MASTR-XML PARSEN (Rohdaten -> CSV)
# ==============================================================================
# Datenquelle: siehe docs/00_datenquellen.md, Abschnitt "MaStR".
#
# Liest den komprimierten MaStR-Gesamtdatenexport (XML) und schreibt je
# Windkraftanlage eine Zeile mit den Feldern, die wir spaeter brauchen, in
# eine CSV. Nutzt iterparse, damit die grosse XML-Datei nicht komplett in
# den Speicher geladen werden muss.
#
# Eingang : data_raw/EinheitenWind.xml.gz
# Ausgang : data/wind_turbines_roh.csv (eine Zeile je Anlage, ungefiltert)
# ==============================================================================
import xml.etree.ElementTree as ET
import csv
import gzip
import os

XML_GZ_FILE = "data_raw/EinheitenWind.xml.gz"
CSV_FILE = "data/wind_turbines_roh.csv"

FIELDS = [
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
    "Lage",
]


def parse_wind_xml(xml_path, csv_path):
    print(f"Lese und parse XML: {xml_path}")
    os.makedirs(os.path.dirname(csv_path), exist_ok=True)

    count = 0
    with open(csv_path, "w", newline="", encoding="utf-8") as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=FIELDS)
        writer.writeheader()

        with gzip.open(xml_path, "rb") as xml_file:
            for event, elem in ET.iterparse(xml_file, events=("end",)):
                if elem.tag == "EinheitWind":
                    row = {}
                    for field in FIELDS:
                        val = elem.find(field)
                        row[field] = val.text if val is not None else ""
                    writer.writerow(row)
                    count += 1
                    if count % 20000 == 0:
                        print(f"  {count} Anlagen verarbeitet...")
                    elem.clear()

    print(f"Fertig: {count} Windkraftanlagen extrahiert -> {csv_path}")


if __name__ == "__main__":
    if not os.path.exists(XML_GZ_FILE):
        raise SystemExit(f"Rohdatei nicht gefunden: {XML_GZ_FILE}")
    parse_wind_xml(XML_GZ_FILE, CSV_FILE)
