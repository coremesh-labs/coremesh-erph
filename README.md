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
| `contract-billing` | `ContractBilling`, `ContractBillingCache` | Vertragsbuchung: rechnet und bucht die Sollstellung der Verträge über die Vorerfassung des Hauptbuchs – ohne eigene Daten, ohne SQL | 0.2.0 |

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

Rechnet die fälligen Konditionen der Verträge (Plugin `contract`) und bucht sie im Hauptbuch
(Plugin `ledger`) – wie eine manuelle Buchung über die **Vorerfassung**. Je Vertrag und
Fälligkeitstag entsteht ein Beleg.

**Aufgabenteilung:** Haskell rechnet und bucht, Go hält die Daten und stellt die Oberfläche.
Das Plugin hat **keine eigenen Tabellen und kein SQL**; es spricht nur über Objects und Actions
mit dem System:

| Wer | Was |
|---|---|
| Vertragsmodul (Go) | Läufe (`ContractPostingRun`), Sollstellungen (`ContractPosting`), Oberfläche, Einstellungen an der Vertragsart, Abgleich mit dem Hauptbuch |
| contract-billing (Haskell) | Fälligkeiten und Belege rechnen, Vorerfassung anlegen, prüfen, ggf. buchen; Kopie der Stammdaten im Speicher |

**Oberfläche** (Modul Verträge → Buchung): „Buchungslauf …“ bucht im Buchungskreis alles, was
bis zum Stichtag fällig und noch nicht vermerkt ist; „Vorschau …“ plant nur; „Buchen …“ am
Vertrag für einen Vertrag. Konsole:
`console --object ContractBilling --action run --param company_code='"1000"' --param to_date=2026-10-31 --param simulate=true`.

### Ablauf je Beleg

In **einer** Transaktion (das Plugin öffnet sie über die Host-API, `inTx`; Ledger und
Vertragsmodul nehmen an ihr teil):

1. `JournalDraft.create`, je Position `JournalDraftItem.create`,
2. `JournalDraft.simulate` – nicht buchbar (gesperrte Periode, fehlende Kontierung …): nichts
   wird angelegt, der Lauf meldet den Grund,
3. **„Automatisch ins Hauptbuch buchen“** an der Vertragsart (Standard aus): `JournalDraft.post`;
   sonst bleibt die geprüfte Vorerfassung offen und wird im Hauptbuch gebucht,
4. `ContractPostingService.record`: Fälligkeiten vermerken (vorerfasst bzw. gebucht).

Wird eine offene Vorerfassung später im Hauptbuch **gebucht**, übernimmt die Sollstellung die
Belegnummer; wird sie **verworfen**, ist die Fälligkeit wieder offen und kommt im nächsten Lauf.
Das Vertragsmodul hört dafür auf die SystemEvents `JournalDraft.post`/`deactivate` und gleicht
vor jedem Lauf die offenen Vorerfassungen ab (Events gehen höchstens einmal zu).

### Kopie der Stammdaten

- Beim ersten Gebrauch je Mandant und Buchungskreis liest das Plugin per Datenstrom:
  Vertragsarten, Verträge, Konditionen, Vertragsobjekte, Bemessungen, Sachkonten im
  Buchungskreis, Feldstatus. Geladen wird als **Systemanfrage** (`ingress: true`), unabhängig
  vom buchenden Benutzer.
- **Events** (`SystemEvent.Register` auf diese Objects, Callback `ContractBillingCache.onEvent`)
  machen den betroffenen Ausschnitt ungültig; er wird beim nächsten Gebrauch neu geladen.
- **Sicherheitsnetz:** spätestens nach `cache_ttl` (Standard 1 h) neu laden;
  `ContractBilling.refresh` verwirft die Kopie sofort, `ContractBilling.cache` zeigt den Zustand.
- Die vermerkten Fälligkeiten (`ContractPosting`) liest jeder Lauf **frisch** – hier darf nichts
  veralten, sonst würde doppelt gebucht.
- **Rechte:** Gebucht werden nur Verträge, die der Benutzer lesen darf (`Account.Check`,
  `Contract.read` je Buchungskreis und Vertragsart); die Buchung selbst prüft der Ledger.

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
| Überschneidung | Zeitscheiben einer Kondition überschneiden sich: Meldung, der Vertrag wird nicht gebucht |

### Belege

| | Soll | Haben |
|---|---|---|
| Forderung (Vertragsart Richtung „wir erhalten“) | Abstimmkonto der Vertragsart (Debitor) | Sachkonto der Kondition (Kontenfindung) |
| Verbindlichkeit („wir zahlen“) | Sachkonto der Kondition | Abstimmkonto (Kreditor) |

- **Einstellungen** an der Vertragsart (Gruppe „Buchung“): Abstimmkonto (Pflicht), Belegart
  (leer = DR bzw. KR), automatisch buchen.
- **Negative Beträge** (Minderungen) tauschen Soll und Haben.
- **Kontierungen:** Vertrag, Mietobjekt (Objekt der Kondition, sonst Hauptobjekt – nur
  Mietobjekte), Debitor bzw. Kreditor (abweichender Zahler, sonst Vertragspartner) – nur, was
  die **Feldstatusgruppe** des Kontos zulässt; Pflichtfelder prüft der Ledger.
- **Kopf:** Buchungs- und Belegdatum = Fälligkeit, Text „Sollstellung <Vertrag> <Datum>“,
  Referenz `<Vertrag>/<JJJJMMTT>`.

### Aufbau

| Modul | Inhalt |
|---|---|
| `Billing.Schedule` | Perioden, anteilige Beträge, Fälligkeitstage (rein) |
| `Billing.Plan` | Belege aus Verträgen, Konditionen, Objekten, Konten und Feldstatus (rein) |
| `Billing.Cache` | Kopie der Stammdaten, Ungültig per Event, Haltezeit |
| `Billing.Data` | Datensätze aus den Datenströmen der anderen Plugins |
| `Billing.Run` | Lauf: Rechte, planen, je Beleg Vorerfassung, Prüfen, Buchen, Vermerk (`inTx` + `dispatch`) |
| `Billing.Api` | Objects `ContractBilling` (run, refresh, cache) und `ContractBillingCache` (onEvent) |

Tests (`cabal test contract-billing`): Fälligkeiten (Teilperioden, Anpassung, Kündigung,
Quartal nachschüssig, Monatsende, einmalig, Rundung) und Belegplanung (Richtung, Kontierung je
Feldstatus, Minderung, fehlendes Abstimmkonto, bereits Vermerktes, Überschneidung).

Seit 0.2.0 liegen die eigenen Tabellen `contract_billing__*` von 0.1.0 leer in der Datenbank
(dbschema löscht keine Tabellen).

### Nächste Schritte

- Zahlungseingänge und Ausgleich der offenen Posten (Debitor), Mahnung.
- Steuerkennzeichen der Konditionsart (Gewerbemiete mit Umsatzsteuer).
- Nebenkostenabrechnung aus den Vorauszahlungen.
