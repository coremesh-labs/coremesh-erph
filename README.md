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
| `contract-billing` | `ContractBilling`, `ContractBillingCache` | Vertragsbuchung: rechnet und bucht die Sollstellung der Verträge über die Vorerfassung des Hauptbuchs – ohne eigene Daten, ohne SQL | 0.8.2 |

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
Buchungsdatum entsteht ein Beleg. Schon Gebuchtes wird nachberechnet, wenn sich die Verträge
rückwirkend ändern.

**Aufgabenteilung:** Haskell rechnet und bucht, Go hält die Daten und stellt die Oberfläche.
Das Plugin hat **keine eigenen Tabellen und kein SQL**; es spricht nur über Objects und Actions
mit dem System:

| Wer | Was |
|---|---|
| Vertragsmodul (Go) | Läufe (`ContractPostingRun`), Sollstellungen (`ContractPosting`), Oberfläche, Einstellungen an der Vertragsart, Abgleich mit dem Hauptbuch |
| contract-billing (Haskell) | Fälligkeiten und Belege rechnen, Vorerfassung anlegen, prüfen, ggf. buchen; Kopie der Stammdaten im Speicher |

**Oberfläche** (Modul Verträge → Buchung): „Buchungslauf …“ bucht im Buchungskreis alles, was
bis zum Stichtag fällig und noch nicht vermerkt ist, und die Nachberechnungen; „Vorschau …“
plant nur; „Buchen …“ am Vertrag für einen Vertrag. „Buchen bis“ ist mit heute vorbelegt, das
Formular zeigt den letzten Lauf. Der Lauf merkt sich kein Datum, sondern je Fälligkeit, ob sie
vermerkt ist – ausgelassene Läufe holt der nächste mit den ursprünglichen Fälligkeiten nach. Konsole:
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
- Die vermerkten Fälligkeiten (`ContractPosting`) und die Buchungskreisdaten der Partner
  (`PartnerCompanyCode`: Abstimmkonto, Buchungssperre) liest jeder Lauf **frisch** – hier darf
  nichts veralten (Doppelbuchung, falsches Konto, gesperrter Partner).
- **Rechte:** Gebucht werden nur Verträge, die der Benutzer lesen darf (`Account.Check`,
  `Contract.read` je Buchungskreis und Vertragsart); die Buchung selbst prüft der Ledger.

### Fälligkeiten

Sollstellung frühestens ab Vertragsbeginn, Beginn der Kondition und – seit 0.8.1 – dem Feld
**„Sollstellung ab“** am Vertrag (`billing_start`, z. B. Übernahmestichtag aus einem Altsystem):
Davor entstehen weder Sollstellungen noch Nachberechnungen.

| Regel | Inhalt |
|---|---|
| Perioden | Kalendermonat, Quartal (Jan/Apr/Jul/Okt), Halbjahr, Jahr; Beginn zur Monatsmitte = anteiliger erster Monat, keine eigenen Perioden |
| einmalig | ein Betrag (z. B. Bereitstellungsgebühr), fällig am Fälligkeitsdatum der Kondition, leer = Beginn |
| Teilperioden | Deckt die Kondition nur einen Teil ab (Beginn am 15., Anpassung, Kündigung), ist der Betrag tagesgenau anteilig, kaufmännisch gerundet |
| vorschüssig | fällig am Fälligkeitstag im ersten Monat der Periode, nicht vor Beginn des abgedeckten Teils |
| nachschüssig | fällig am Fälligkeitstag im Monat nach dem Ende der Periode |
| Fälligkeitstag | z. B. der 15., wenn der Mieter zur Monatsmitte zahlt; über dem Monatsende: letzter Tag des Monats |
| je Einheit | Preis × Bemessung des Objekts am Beginn des abgedeckten Teils (realestate `Measurement`) |
| Zeitraum | Kondition ∩ Vertragslaufzeit; Verträge im Status aktiv oder gekündigt |
| Überschneidung | Zeitscheiben einer Kondition überschneiden sich: Meldung, der Vertrag wird nicht gebucht |

### Kaution und Darlehen

- **Raten:** Einmalige Konditionen mit Monatsraten (z. B. Kaution 3 Raten) – je Rate eine
  Fälligkeit, einen Monat nach der vorigen; der Rest der Division geht auf die erste Rate.
- **Darlehen** (`Billing.Loan`, rein): Zinsperioden enden an den Fälligkeiten; Zins je Periode
  stückweise über Zinssatzwechsel (Zeitscheiben) und Sondertilgungen nach Zinsmethode
  (30/360 deutsch mit Monatsende = 30., act/360, act/365), kaufmännisch gerundet. Tilgung:
  Annuität = Rate − Zins, Ratentilgung = Rate, endfällig = 0; am Vertragsende die Restschuld.
- Daraus entstehen Fälligkeiten wie bei Konditionen – Auszahlung (Bank an Darlehen), Zins
  (Zinsaufwand bzw. -ertrag) und Tilgung (Darlehenskonto) gegen das Partnerkonto der Bank;
  Nachberechnung bei geänderten Konditionen inklusive.
- `ContractBilling.loanSchedule` liefert den Tilgungsplan als Tabelle (Aktion „Tilgungsplan“).

### Vertragsabrechnung

`ContractBilling.checkSettlement` und `postSettlement` (`Billing.Settlement`, rein): Prüfungen
einer Vertragsabrechnung (Einzelbetrag aus Gesamtkosten und Schlüssel, Vorauszahlungen,
Ergebnis, Rücklage gegen Vorjahr und Hauptbuch – `AccountBalance.list`, fehlende Kostenarten)
als Tabelle mit Ampel, Buchungsplan (Kosten im Soll, gebuchte Vorauszahlungen je Konto im
Haben, Entnahme aus der Rücklage, Differenz an das Partnerkonto) und Buchung über die
Vorerfassung in einer Transaktion; der Vermerk geht an `ContractPostingService.recordSettlement`.
Die Vorauszahlungs-Konditionsarten und die Toleranz gibt das Vertragsmodul mit.

### Nebenkostenabrechnung

`ContractBilling.computeOpCost` (`Billing.OpCost` rein, `Billing.OpCostRun` lädt): Regelwerk,
Regeln und Schlüssel schickt das Modul Betriebskosten (`opcost`), es speichert auch Journal und
Ergebnis. Geladen werden die Mietobjekte der Einheit, die Nutzung durch Mietverträge der
Mietervertragsarten (Vertrags- und Objektzeitraum), Personen-Zeitscheiben, Bemessungen, die
Quellen (Einzelposten, gebuchte Eingangsrechnungen, gebuchte Vertragsabrechnungen) und die
Vorauszahlungen (`ContractPosting`, anteilig). Gerechnet wird ganzzahlig in Cent × Faktor auf
internen Konten (Sammeln, Umbuchen, Verteilen, Zeitanteile, Rundung); jede Buchung trägt ihre
Formel. Jede Sammelregel ordnet ihre Quelle nach Leistungs-/Abrechnungszeitraum, Buchungs-
oder Belegdatum zu. Prüfungen: liegengebliebene Töpfe, Summe der Quellen = Mieter + Eigentümer,
Rundung, Vollständigkeit der Vertragsabrechnungen (Lücken, im Vorjahr abgerechnet und jetzt
fehlend, erfasst und nicht gebucht – Fehler, `failures` in der Antwort).

### Nachberechnung

Jeder Lauf rechnet auch die schon vermerkten Perioden neu und vergleicht je Vertrag,
Konditionsart, Objekt und **Kalenderperiode** mit der Summe des Vermerkten (vorerfasst oder
gebucht):

| Fall | Ergebnis |
|---|---|
| Betrag rückwirkend geändert (Korrektur, Mieterhöhung, auch ab Monatsmitte) | Differenz je Periode |
| Minderung wegen Mängeln, nachträglich als Kondition mit negativem Betrag erfasst (z. B. `MM`) | Gutschrift für die schon gebuchten Monate |
| Vertrag rückwirkend beendet, Kondition entfallen | Gutschrift des Vermerkten (Konto aus dem Vermerk) |
| Neue Kondition für eine Zeit, die schon abgerechnet und fällig ist | Nachberechnung statt Sollstellung zur alten Fälligkeit |
| Neue Kondition, die erst künftig fällig ist | Sollstellung zur Fälligkeit |
| nur der Fälligkeitstag geändert | nichts |

- **Buchungsdatum** der Nachberechnung ist der Tag des Laufs (höchstens der Stichtag) – die alten
  Perioden sind oft schon gesperrt. Positionstext „Nachberechnung KM 9/2026“, Kopf
  „Nachberechnung <Vertrag> <Datum>“.
- Vermerkt wird sie als weitere Zeile zum selben Schlüssel (laufende Nummer 1, 2 …, Art
  „Nachberechnung“), so dass Vermerktes und Gerechnetes danach wieder übereinstimmen: Ein zweiter
  Lauf findet nichts mehr.
- Konditionen mit Fehlern (Sachkonto, Bemessung) werden nicht nachberechnet; Überschneidungen
  sperren den Vertrag wie bisher.
- Vermerke von Version 0.2.0 haben keine Kalenderperiode; sie wird aus dem Rhythmus der Kondition
  abgeleitet.

### Belege

| | Soll | Haben |
|---|---|---|
| Forderung (Vertragsart Richtung „wir erhalten“) | Abstimmkonto des Zahlers (Debitor) | Sachkonto der Kondition (Kontenfindung) |
| Verbindlichkeit („wir zahlen“) | Sachkonto der Kondition | Abstimmkonto des Zahlers (Kreditor) |

- **Abstimmkonto:** aus den Buchungskreisdaten des Zahlers – Vertragspartner oder abweichender
  Zahler der Kondition – in der Rolle der Vertragsart (Partnermodul, `PartnerCompanyCode`; jeder
  Lauf liest sie frisch). Fehlen sie oder trägt der Partner eine **Buchungssperre**, wird der
  Vertrag nicht gebucht und der Lauf meldet es.

- **Einstellungen** an der Vertragsart (Gruppe „Buchung“): Belegart (leer = DR bzw. KR), Belegart für Gutschriften (leer = DG bzw. KG), automatisch buchen.
- **Negative Beträge** (Minderungen, Gutschriften) tauschen Soll und Haben; ist der Beleg per
  Saldo negativ, bekommt er die Gutschrift-Belegart.
- **Kontierungen:** Vertrag, Mietobjekt (Objekt der Kondition, sonst Hauptobjekt – nur
  Mietobjekte), Debitor bzw. Kreditor (abweichender Zahler, sonst Vertragspartner) – nur, was
  die **Feldstatusgruppe** des Kontos zulässt; Pflichtfelder prüft der Ledger.
- **Kopf:** Buchungs- und Belegdatum = Fälligkeit, Text „Sollstellung <Vertrag> <Datum>“,
  Referenz `<Vertrag>/<JJJJMMTT>`.

### Aufbau

| Modul | Inhalt |
|---|---|
| `Billing.Schedule` | Perioden, anteilige Beträge, Fälligkeitstage, Einmalzahlungen und Raten (rein) |
| `Billing.Settlement` | Vertragsabrechnung: Prüfungen und Buchungsplan (rein) |
| `Billing.OpCost` | Nebenkostenabrechnung: Abrechnungskonten, Regeln, Verteilung, Rundung, Prüfungen (rein) |
| `Billing.OpCostRun` | Nebenkostenabrechnung: Einheit, Mieter, Quellen und Vorauszahlungen laden |
| `Billing.Loan` | Tilgungsplan: Zinsmethoden, Annuität, Ratentilgung, endfällig, Sondertilgung, Zinswechsel (rein) |
| `Billing.Plan` | Belege aus Verträgen, Konditionen, Objekten, Konten, Feldstatus und dem Vermerkten; Nachberechnung (rein) |
| `Billing.Cache` | Kopie der Stammdaten, Ungültig per Event, Haltezeit |
| `Billing.Data` | Datensätze aus den Datenströmen der anderen Plugins |
| `Billing.Run` | Lauf: Rechte, planen, je Beleg Vorerfassung, Prüfen, Buchen, Vermerk (`inTx` + `dispatch`) |
| `Billing.Api` | Objects `ContractBilling` (run, loanSchedule, checkSettlement, postSettlement, computeOpCost, refresh, cache) und `ContractBillingCache` (onEvent) |

Tests (`cabal test contract-billing:test:spec`): Fälligkeiten (Teilperioden, Anpassung,
Kündigung, Zahlung am 15., Quartal nachschüssig, Monatsende, einmalig mit Fälligkeitsdatum,
Rundung), Belegplanung (Richtung, Kontierung je Feldstatus, Minderung, fehlendes Abstimmkonto,
bereits Vermerktes, Überschneidung) und Nachberechnung (Minderung, Erhöhung ab Monatsmitte,
nachträgliche Kondition, Einmalzahlung, rückwirkendes Ende, entfallene Kondition, verschobener
Fälligkeitstag, alte Vermerke, Belegart nach Saldo), Nebenkostenabrechnung (Personentage,
Wohnfläche mit Leerstand, Umbuchen, direkte Kontierung, Rundung an Mieter bzw. Eigentümer,
liegengebliebene Töpfe).

Seit 0.2.0 liegen die eigenen Tabellen `contract_billing__*` von 0.1.0 leer in der Datenbank
(dbschema löscht keine Tabellen).

### Nächste Schritte

- Zahlungseingänge und Ausgleich der offenen Posten (Debitor), Mahnung.
- Steuerkennzeichen der Konditionsart (Gewerbemiete mit Umsatzsteuer).
- Minderung in Prozent einer anderen Kondition (heute: fester Betrag).
- Stornos im Hauptbuch berücksichtigen (ein stornierter Sollstellungsbeleg gilt noch als vermerkt).
- Nebenkostenabrechnung: Schlüssel Verbrauch (Zähler), Mieterschreiben.
