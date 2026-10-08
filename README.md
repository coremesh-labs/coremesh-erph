# CoreMesh ERP in Haskell (`coremesh-erph`)

Fachplugins in Haskell für [CoreMesh](../coremesh), neben den Go-Plugins in
[coremesh-erp](../coremesh-erp). Ein Plugin ist ein eigener Prozess; der Host startet ihn wie
jedes Go-Plugin. Grundlage ist das Haskell-SDK des Kerns
([`../coremesh/sdk/haskell`](../coremesh/sdk/haskell)).

```
coremesh-labs/
├── coremesh/           Kern, Host, Go-SDK und Haskell-SDK (sdk/haskell)
├── coremesh-erp/       Go-Fachplugins (ledger, realestate, contract)
└── coremesh-erph/      dieses Repository
    ├── cabal.project                     alle Plugins + SDK in einem Projekt
    ├── plugins/<plugin>/<plugin>.cabal   ein Plugin = ein Paket (Bibliothek, Programm, Tests)
    ├── configs/                          Host-Konfiguration der Haskell-Plugins
    └── bin/plugins/<xx>/<name>-<version>-<os>-<arch>[.exe]
```

## Plugins

| Plugin | Objects | Inhalt | Version |
|---|---|---|---|
| `contract-billing` | `ContractBilling` | Vertragsbuchung: Sollstellung der Verträge über die Vorerfassung des Hauptbuchs | 0.1.0 |

## Bauen, testen, starten

Voraussetzungen wie beim SDK: GHC 9.10 und cabal (ghcup, auch unter Windows und macOS) und
`protoc` im PATH.

```bash
./build.sh           # Linux/macOS: alle Plugins nach bin/plugins
./build.sh --test    # Tests aller Pakete (Plugins und SDK)
./build.sh --run     # bauen und den Host mit allen Konfigurationen starten
```

```powershell
.\build.ps1          # Windows
.\build.ps1 -Test
.\build.ps1 -Run
```

Der Host liest die Konfigurationen der drei Repositories nacheinander:

```bash
cd ../coremesh
bin/host -config configs,../coremesh-erp/configs,../coremesh-erph/configs
```

`configs/40-contract-billing.yaml` setzt `host.extra_plugin_dirs` auf **beide** Verzeichnisse
(`../coremesh-erp/bin/plugins`, `../coremesh-erph/bin/plugins`): Listen ersetzen beim
Zusammenführen die der früheren Datei.

Ein neues Plugin: Verzeichnis `plugins/<name>/` mit `<name>.cabal` (Programm `exe:<name>`),
Eintrag in `configs/`. `build.sh` findet es über die `.cabal`-Datei.

---

## Vertragsbuchung (`contract-billing`)

Bucht die fälligen Konditionen der Verträge (Plugin `contract`) im Hauptbuch (Plugin `ledger`),
und zwar wie eine manuelle Buchung über die **Vorerfassung**: Buchungskopf anlegen, Positionen
füllen, buchen. Je Vertrag und Fälligkeitstag entsteht ein Beleg.

**Oberfläche** im Modul Verträge (die Objects gehören dem Plugin `contract` und leiten hierher
weiter):

- **Buchung → Buchungsläufe:** „Neu“ bucht für einen Buchungskreis alle Fälligkeiten bis zum
  Stichtag, die noch nicht gebucht sind – also seit der letzten Ausführung. „Vorschau …“
  plant nur. Jeder Lauf zeigt Belege, Fälligkeiten und Meldungen je Vertrag.
- **Buchung → Sollstellungen:** die gebuchten Fälligkeiten mit Beleg; auch als Abschnitt am
  Vertrag und am Buchungslauf.
- **„Buchen …“ am Vertrag:** dasselbe für einen Vertrag, bis zu einem wählbaren Datum.

Konsole: `console --object ContractBilling --action run --param company_code='"1000"' --param to_date=2026-10-31`
(`--param simulate=true` für die Vorschau).

### Fälligkeiten

| Regel | Inhalt |
|---|---|
| Perioden | Kalendermonat, Quartal (Jan/Apr/Jul/Okt), Halbjahr, Jahr; einmalig = Beginn der Kondition |
| Teilperioden | Deckt die Kondition nur einen Teil ab (Beginn am 15., Anpassung, Kündigung), ist der Betrag tagesgenau anteilig, kaufmännisch gerundet |
| vorschüssig | fällig am Fälligkeitstag im ersten Monat der Periode, nicht vor Beginn des abgedeckten Teils |
| nachschüssig | fällig am Fälligkeitstag im Monat nach dem Ende der Periode |
| Fälligkeitstag | über dem Monatsende: letzter Tag des Monats |
| je Einheit | Preis × Bemessung des Objekts am Beginn des abgedeckten Teils (realestate `Measurement`) |
| Zeitraum | Kondition ∩ Vertragslaufzeit; Verträge im Status aktiv oder gekündigt |

### Belege

| | Soll | Haben |
|---|---|---|
| Forderung (Vertragsart Richtung „wir erhalten“) | Abstimmkonto der Vertragsart (Debitor) | Sachkonto der Kondition (Kontenfindung) |
| Verbindlichkeit („wir zahlen“) | Sachkonto der Kondition | Abstimmkonto (Kreditor) |

- **Einstellungen** an der Vertragsart (Verträge → Vertragsarten, Gruppe „Buchung“):
  Abstimmkonto der Sollstellung (Pflicht für die Buchung) und Belegart (leer = DR bzw. KR).
- **Negative Beträge** (Minderungen) tauschen Soll und Haben.
- **Kontierungen:** Vertrag (`rent_contract_id`), Mietobjekt (`rent_object_id`: Objekt der
  Kondition, sonst Hauptobjekt des Vertrags – nur Mietobjekte), Debitor bzw. Kreditor
  (`sd_customer_id`/`supplier_id`: abweichender Zahler, sonst Vertragspartner). Gesetzt wird nur,
  was die **Feldstatusgruppe** des Kontos zulässt; Pflichtfelder prüft der Ledger.
- **Kopf:** Buchungs- und Belegdatum = Fälligkeit, Text „Sollstellung <Vertrag> <Datum>“,
  Referenz `<Vertrag>/<JJJJMMTT>`.

### Transaktion und „seit der letzten Ausführung“

Jeder Beleg läuft in **einer** Transaktion: Vorerfassung, Positionen, Buchen und der Vermerk in
`contract_billing__posting` (der Ledger nimmt an der Transaktion des Plugins teil). Scheitert
ein Schritt – etwa eine gesperrte Periode –, ist für diesen Beleg nichts gebucht und keine
Vorerfassung bleibt liegen; der Lauf meldet es und bucht die übrigen. Gebucht wird beim nächsten
Lauf alles, was bis zum Stichtag fällig und dort noch nicht vermerkt ist. Ein zweiter Lauf mit
demselben Stichtag bucht nichts.

### Tabellen

| Tabelle | Inhalt |
|---|---|
| `contract_billing__run` | Buchungslauf: Buchungskreis, Stichtag, Status (DONE, PARTIAL, FAILED, EMPTY), Anzahl, Meldungen |
| `contract_billing__posting` | gebuchte Fälligkeit: Vertrag, Konditionsart, Objekt, Zeitraum, Fälligkeit, Betrag (Cent), Konto, Vorerfassung, Beleg, Lauf |

### Aufbau

| Modul | Inhalt |
|---|---|
| `Billing.Schedule` | Perioden, anteilige Beträge, Fälligkeitstage (rein) |
| `Billing.Plan` | Belege aus Verträgen, Konditionen, Objekten, Konten und Feldstatus (rein) |
| `Billing.Data` | Datensätze aus den Datenströmen der anderen Plugins |
| `Billing.Run` | Lauf: lesen (`DispatchRead`), planen, je Beleg buchen (`inTx` + `dispatch`) |
| `Billing.Store` | Atlas-Schema und SQL der eigenen Tabellen |
| `Billing.Api` | Object `ContractBilling` (run, runs, getRun, postings, getPosting; postings auch als Datenstrom) |

Tests (`cabal test contract-billing`): Fälligkeiten (Teilperioden, Anpassung, Kündigung,
Quartal nachschüssig, Monatsende, einmalig, Rundung) und Belegplanung (Richtung, Kontierung je
Feldstatus, Minderung, fehlendes Abstimmkonto, bereits Gebuchtes).

### Nächste Schritte

- Zahlungseingänge und Ausgleich der offenen Posten (Debitor), Mahnung.
- Steuerkennzeichen der Konditionsart (Gewerbemiete mit Umsatzsteuer).
- Nebenkostenabrechnung aus den Vorauszahlungen.
