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
| `contract-billing` | `ContractBilling`, `ContractBillingCache` | Vertragsbuchung: rechnet und bucht die Sollstellung der Verträge über die Vorerfassung des Hauptbuchs – ohne eigene Daten, ohne SQL; Einzelpreis je Einheit, nachgelagerte Konditionen (Steuer, Prozent; Zuschlag oder Anteil), Rundungsregeln, Tilgungsplan bis Stichtag, Positionen (derselbe Gegenstand mehrfach), je Einheit ohne Objekt = Bemessung des Hauptobjekts, keine Sollstellung bei Abrechnungsweg Eingangsrechnung, Gebäude des Vertragsobjekts auf Erlös- und Aufwandszeilen (Einstellung object_fields) | 0.18.0 |
| `profit-loss` | `ProfitLoss`, `TrialBalance`, `AccountSheet`, `PartnerAccounts`, `LedgerReport` | Auswertungen des Hauptbuchs: GuV je Buchungskreis, Gebäude, Wirtschaftseinheit oder Mietobjekt (wahlweise Spalte je Gebäude), Summen- und Saldenliste und Kontoblatt je Gebäude oder Wirtschaftseinheit mit laufendem Saldo, Debitoren und Kreditoren je Abstimmkonto mit Summen, Vertrags- und Partnerkonto sowie Kontostand für die Aktionen am Vertrag und Geschäftspartner – Bildschirm und CSV, nichts wird gespeichert | 0.3.0 |

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
  Buchungskreis, Feldstatus; seit 0.13.0 Konditionsarten, nachgelagerte Konditionsarten,
  Steuerkennzeichen und Rundungsregeln. Geladen wird als **Systemanfrage** (`ingress: true`), unabhängig
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

**Abschreibung** (seit 0.10.0, `Billing.Depreciation`): aus `ContractDepreciation` je Jahr eine
Fälligkeit zum 31.12. bzw. Vertragsende, monatsgenau (erstes Jahr ab dem Monat des Beginns);
linear = Bemessungsgrundlage × Satz, degressiv = Buchwert × Satz, jeweils × Monate / 12, bis
Buchwert 0. Höhere Bemessungsgrundlage in einer neuen Zeitscheibe = Zugang für das ganze Jahr.
Gebucht wird Abschreibungs- an Anlagekonto ohne Partnerzeile (Belegart SA), mit Gebäude bzw.
Wirtschaftseinheit als Kontierung (`dimension_custom_1`/`_2`); `ContractBilling.depreciationSchedule`
liefert den Plan als Tabelle.

**Rückwirkend buchen ab** (seit 0.9.0, Steuerung des Buchungskreises im Hauptbuch,
`LedgerCompanyConfig.backdate_from`): Nachberechnungen buchen sonst zum Tag des Laufs; mit
gesetztem Datum zu ihrer ursprünglichen Fälligkeit – nicht vor diesem Datum, nicht nach dem
Lauf. So lässt sich z. B. ein ganzes Jahr nachträglich durchbuchen (Simulation).

| Regel | Inhalt |
|---|---|
| Perioden | Kalendermonat, Quartal (Jan/Apr/Jul/Okt), Halbjahr, Jahr; Beginn zur Monatsmitte = anteiliger erster Monat, keine eigenen Perioden |
| einmalig | ein Betrag (z. B. Bereitstellungsgebühr), fällig am Fälligkeitsdatum der Kondition, leer = Beginn |
| Teilperioden | Deckt die Kondition nur einen Teil ab (Beginn am 15., Anpassung, Kündigung), ist der Betrag tagesgenau anteilig, kaufmännisch gerundet |
| vorschüssig | fällig am Fälligkeitstag im ersten Monat der Periode, nicht vor Beginn des abgedeckten Teils |
| nachschüssig | fällig am Fälligkeitstag im Monat nach dem Ende der Periode |
| Fälligkeitstag | z. B. der 15., wenn der Mieter zur Monatsmitte zahlt; über dem Monatsende: letzter Tag des Monats |
| je Einheit | Einzelpreis × Bemessung des Objekts am Beginn des abgedeckten Teils (realestate `Measurement`); seit 0.13.0 mit dem genauen Einzelpreis (`unit_price`, z. B. 4 Stellen) nach der Rundungsregel |
| Menge × Einzelpreis | Gesamtpreis je Periode rechnet das Vertragsmodul beim Speichern (`amount`) |
| nachgelagert | seit 0.13.0: aus den Fälligkeiten der anderen Konditionen im Zeitraum der nachgelagerten, je Fälligkeit und Steuerkennzeichen (Konditionsart der Basis bzw. fest an der Art). Steuer: Satz aus `TaxCode`, Konto = Kondition, Art bzw. Steuerkonto; Prozent: Satz der Art. **Zuschlag** auf die Summe als eigene Fälligkeit (Basis späterer Arten, keine Steuer auf Steuer); **Anteil** je Basisfälligkeit gerundet und dort abgezogen (eigenes Konto, Summe gleich) |
| Rundung | Regel der Kondition, der nachgelagerten Art, der Konditionsart, des Buchungskreises (`LedgerCompanyConfig.rounding_rule`), Standardregel, sonst kaufmännisch auf den Cent |
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
oder Belegdatum zu. Einzelposten ohne Mietobjekt gehören – seit 0.9.0 – über Gebäude bzw.
Wirtschaftseinheit zur Einheit; welches Feld der Buchungszeile welches Objekt trägt, steht in der
Objektkontierung der Beschaffung (`ObjectPosting`, Standard Gebäude `dimension_custom_1`,
Wirtschaftseinheit `dimension_custom_2`). Ganz ohne Objekt zählt ein Einzelposten für jede
Einheit, die das Konto sammelt. Prüfungen: liegengebliebene Töpfe, Summe der Quellen = Mieter + Eigentümer,
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

## GuV-Rechnung (`profit-loss`)

Menü **GuV → GuV-Rechnung** (`/m/profit-loss/ProfitLoss`): Maske mit Buchungskreis,
Mietobjekt (leer = alle) und Zeitraum (Vorschlag: laufendes Jahr bis heute). „GuV berechnen“
zeigt darunter Bereiche, Konten, Summen und das Ergebnis (Gewinn +, Verlust −) und bietet die
Tabelle als CSV zum Speichern an (Semikolon, Dezimalkomma, UTF-8 mit BOM für Tabellenkalkulationen).
Nichts wird gespeichert. Der WebServer (ab 0.33.0) zeigt ein Object ohne Übersicht als Bericht.

Gelesen wird mit den Rechten des Benutzers: Sachkonten (`GLAccountCompany`, `GLAccount`) und
Einzelposten (`JournalEntryItem`, Ledger aus `settings.ledger`, Buchungsdatum im Zeitraum,
Mietobjekt über die Dimension `rent_object_id`). Welche Kontotypen in welchen Bereich gehören
und ob ein Bereich Ertrag (`INCOME`, Haben positiv) oder Aufwand (`EXPENSE`, Soll positiv) ist,
steht in `configs/70-profit-loss.yaml` (`settings.sections`).

| Modul | Inhalt |
|---|---|
| `ProfitLoss.Report` | Einzelposten → Bereiche, Summen, Ergebnis, Tabelle, CSV, Beträge deutsch (rein) |
| `ProfitLoss.Meta` | Metamodell (Catalog.Describe): Modul, Object, Maske, Übersetzungen |
| `ProfitLoss.Objects` | Gebäude eines Postens: Kontierungsspalte → Mietobjekt → Vertragsobjekt am Buchungsdatum (rein) |
| `ProfitLoss.Ledger` | Saldenliste, Kontoblatt, Stand je Vertrag (rein) |
| `ProfitLoss.Api` | Objects `ProfitLoss`, `TrialBalance`, `AccountSheet` (compute, formState), `LedgerReport` (ohne Maske), Einstellungen, Lesen aus dem Hauptbuch |

`LedgerReport` (seit 0.2.0) rufen andere Module auf: `contractAccount` (Aktion
„Vertragskonto …“ am Vertrag: Partnerposten der Belege zum Vertrag und der
Bankbelege mit diesem Vertrag), `partnerAccount` („Partnerkonto …“ am
Geschäftspartner, je Vertrag der Stand) und `accountBalance` („Darlehensstand …“:
Saldo des Darlehenskontos am Stichtag). Gezählt werden alle Geschäftsjahre bis
zum Stichtag – das Hauptbuch trägt keine Salden vor, Anfangsbestände sind gebucht.

Tests (`cabal test profit-loss:test:spec`): Zeitraum, Bilanz- und unbekannte Konten, Storno
(Konto ohne Bewegung entfällt), Vorzeichen je Bereich, Ergebnis, Tabelle, Beträge, CSV-Maskierung,
eigene Bereiche aus den Einstellungen.
