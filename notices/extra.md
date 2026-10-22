## Systembibliotheken der Haskell-Programme

- **GMP** (GNU Multiple Precision, LGPL-3.0-or-later oder GPL-2.0-or-later): Die Programme
  `contract-billing` und `profit-loss` nutzen GMP über das Laufzeitsystem von GHC (ghc-bignum).
  Unter Linux wird die GMP des Betriebssystems dynamisch geladen (`libgmp.so.10`) und nicht
  mitgeliefert. Wird GMP in einer Ausgabe für Windows oder macOS statisch gelinkt, gilt die
  LGPL: Der vollständige Quelltext von CoreMesh (EUPL-1.2) steht öffentlich zur Verfügung, so
  dass sich die Programme mit einer anderen GMP neu bauen lassen; GMP selbst:
  https://gmplib.org.
- **zlib** (Zlib-Lizenz) des Betriebssystems, dynamisch.
