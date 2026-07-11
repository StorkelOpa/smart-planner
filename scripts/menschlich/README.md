# Menschliche Stil-Version der Scripts

Dieselbe Pipeline wie in `scripts/`, aber im schlichteren "Kurs-Stil"
geschrieben (so, wie man R im DataCamp-Kurs lernt):

- keine Metaprogrammierung (`!!name := ...`) — jede Variable ausgeschrieben
- kein `across(everything(), ~ ...)` — Spalten einzeln mit `scale()`
- kaum `cat(...)`-Logging — Ergebnisse einfach in der Konsole anschauen
- kurze, normale Kommentare statt großer `# ===`-Banner
- nur vertraute Verben: `select`, `filter`, `mutate`, `group_by`,
  `summarise`, `left_join`/`full_join`, `case_when`, `ggplot`

**Wichtig:** Diese Versionen schreiben in dieselben `data/`- und `plots/`-
Dateien wie die Originale und erzeugen dieselben Ergebnisse — sie sind also
1:1 austauschbar. Aus dem Projektordner heraus ausführen, z.B.:

    Rscript scripts/menschlich/01_get_geodata.R

## Bewusst weggelassen (im Vergleich zu scripts/)

Die Originale laden fehlende Rohdaten notfalls automatisch aus dem Netz nach
(`download.file(...)` in 01 und 03) und geben ausführliche Fortschritts- und
Zusammenfassungs-Meldungen aus. Beides ist für die schlichte Version nicht
nötig, solange die Rohdaten in `data_raw/` liegen (tun sie).
