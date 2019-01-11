# Quellenangaben
Generelles zum finden, aufbauen sowie löschen innerhalb eines Binärbaumes: https://de.wikipedia.org/wiki/Binärbaum

# Binärbaum

Der Binärbaum hat 4 Komponenten
- Element
- Höhe des Elements
- Linker Teil des Baumes
- Rechter Teil des Baumes

In der Aufgabenstellung geht als Anforderung hervor, dass der Binärbaum als Erlang Tupel mit folgender Struktur {<Element>,<Höhe>,<linker Teilbaum>,<rechter Teilbaum>} dargestellt werden soll.
Ein leerer Baum wird als leeres Tupel „{}“ repräsentiert.
Fehlerhafte Operationen auf auf dem Binärbaum werden schlicht nicht ausgeführt und das Ergebnis sieht aus, als hätte diese Operation nicht stattgefunden.

# Erlang Implementierung
## Datenstruktur
Für die Implementierung in Erlang werden Tupel mit folgender Struktur {<Element>,<Höhe>,<linker Teilbaum>,<rechter Teilbaum>} verwendet. Ein leerer Baum wird als leeres Tupel „{}“ repräsentiert.
Fehlerhafte Operationen auf auf dem Baum werden schlicht nicht ausgeführt und das Ergebnis sieht aus, als hätte diese Operation nicht stattgefunden.

### Element
Das Element eines Knoten ist ein Integer Atom

### Höhe
Die Höhe des Baumes bestimmt, wie viele Elemente es „nach Unten“ geht und wird als Integer Atom dargestellt.

### Linker & Rechter Teil des Baumes
Das Element des linken Teilbaumes ist kleiner als das Integer Atom. Das Element des rechten Teilbaums ist größer als das Integer Atom. Die Teilbäume folgen der oben definierten Datenstruktur.

## Funktionen
### initBT: ∅ → btree
Beschreibung
Die Funktion „initBT“ bekommt keinen Übergabeparameter und gibt einen Leeren Baum zurück, welcher als leeres Tupel repräsentiert wird.
Vorgehensweise
1. Es wird ein leeres Tupel {} zurückgeben
isEmptyBT: btree → bool
Beschreibung
Die Funktion prüft, ob der übergebene Btree leer ist und gibt ein Wahr oder Falsch zurück.
Vorgehensweise
1. Ein Prädikat (Btree == ∅) zurückgeben
equalBT: btree × btree → bool
Beschreibung
Diese Funktion prüft zwei Bäume auf semantische Gleichheit
Vorgehensweise
1. Wenn einer der beiden Bäume leer und der Andere nicht leer ist, falsch zurückgeben
2. Beide Bäume in Order bringen
3. Ein Prädikat zurückgeben (inOrder BaumA == inOrder BaumB)
### insertBT: btree × elem → btree
Beschreibung
Diese Funktion fügt einem Baum ein Blatt hinzu und gibt den Veränderten/Erweiterten Baum zurück
Vorgehensweise
1. Ist der Baum Leer, das Atom mit der Höhe 1 in Baumstruktur zurückgeben
2. das neue Element mit dem Atom des Baumknotens vergleichen
3. Ist das Element kleiner dem Atom des aktuellen Baumknotens, führe die Funktion rekursiv ab Punkt 1 für den neuen linken Teilbaum aus und gehe weiter zu Schritt 4, sonst springe zu Schritt 5.
4. Ist die Höhe vom neuen Linken Teilbaum kleiner oder gleich der Höhe des existierenden rechten Teilbaumes, oder wenn die Höhe des aktuellen Baumknotens subtrahiert mit mit der Höhe des neuen Linken Teils größer 0 ist, bleibt die Höhe des aktuellen Baumknotens ohne Veränderung, sonst muss die Höhe des aktuellen Baumknotens um 1 erhöht werden. Gebe den Baum zurück.

5. Ist das Element größer dem Atom des aktuellen Baumknotens, führe die Funktion rekursiv ab Punkt 2 für den neuen Rechten aus und gehe weiter zu Schritt 6
6. Ist die Höhe vom neuen rechten Teilbaum kleiner oder gleich der Höhe des existierenden linken Teilbaumes, oder wenn die Höhe des aktuellen Baumknotens subtrahiert mit mit der Höhe des neuen rechten Teilbaumes größer 0 ist, bleibt die Höhe des aktuellen Baumknotens ohne Veränderung, sonst muss die Höhe des aktuellen Baumknotens um 1 erhöht werden. Gebe den Baum zurück.
7. Wenn die Prädikate aus Schritt 3 oder 5 nicht zutreffen, muss das Element bereits in dem Baum vorhanden sein, in diesem Falle, mache nichts und gebe den Baum zurück.
### deleteBT: btree × elem → btree
Beschreibung
Diese Funktion löscht ein Element aus einem Baum. Und zwar ist es wichtig, dass der Baum danach immer noch der Datenstrukturdefinition entspricht.
Dabei ist zu beachten, dass das bloße „hochschieben“ von Teilbäumen beim Löschen nicht gemacht werden darf, da sonst die Definition der Datenstruktur verletzt wird. Zu sehen an folgendem Beispiel:

Vorgehensweise
1. Wenn der Baum leer ist, gebe einen leeren Baum zurück
2. Ist der Baum nicht leer, beginne bei dem Wurzel Baumknoten
3. Wenn das Element gleich dem Atom des aktuellen Baumknotens ist, gehe zu Schritt 4
Wenn das Element kleiner dem Atom des aktuellen Baumknotens ist, gehe zu Schritt 6
Wenn das Element größer dem Atom des aktuellen Baumknotens ist, gehe zu Schritt 7
4. Unterscheide 4 Fälle:
- sind der linke und rechte Teilbaum leer, gebe einen leeren Baum zurück
- ist der linke Teilbaum leer, gebe den Rechten zurück
- ist der rechte Teilbaum leer, geben den Linken zurück
- sind beide nicht leer, gehe zu Schritt 5
5. Finde das kleinste Element des rechten Teilbaumes, lösche das Element und ersetze damit das Element des aktuellen Baumknotens.
Wenn die Differenz zwischen der Höhe des aktuellen Baumknotens und der Höhe des neuen Rechten oder des bestehenden Linken Teilbaumes nicht 1 ist, dann muss die Höhe des aktuellen Baumknotens um 1 reduziert werden
6. Gehe in den linken Teilbaum, und springe zu Schritt 1
Wenn die Differenz zwischen der Höhe des aktuellen Baumknotens und der Höhe des Rechten oder neuen linken Teilbaumes nicht 1 ist, dann muss die Höhe des aktuellen Baumknotens um 1 reduziert werden
7. Gehe in den rechten Teilbaum, und springe zu Schritt 1
Wenn die Differenz zwischen der Höhe des aktuellen Baumknotens und der Höhe des neuen rechten oder bestehenden linken Teilbaumes nicht 1 ist, dann muss die Höhe des aktuellen Baumknotens um 1 reduziert werden

### findBT: btree × elem → integer
Beschreibung
Diese Funktion bekommt ein Baum in dem das übergeben Element gesucht ist und gibt die Höhe des Elementes innerhalb des Baumes zurück.
Vorgehensweise
1. Ist der Baum leer, kann das Element nicht vorhanden sein und gebe 0 zurück
2. Ist das Element gleich dem Atom des aktuellen Baumknotens, gebe die Höhe des aktuellen Baumknotens zurück
3. Ist das Element kleiner dem Atom des aktuellen Baumknotens, gehe in den linken Teilbaum und Springe zu Schritt 1
4. Ist das Element größer dem Atom des aktuellen Baumknotens, gehe in den rechten Teilbaum und Springe zu Schritt 1
### inOrderBT: btree → liste
Beschreibung
Diese Funktion bekommt einen Baum übergeben, und gibt diesen als sortierte Liste zurück
Vorgehensweise
1. Sind der linke und rechte Teilbaum leer, packe das  aktuelle Atom in eine Liste
2. Ist nur der linke Teilbaum leer, packe das Atom in eine Liste und füge die Elemente aus dem rekursiven Aufruf mit dem rechten Teilbaum rechts davon.
3. Ist nur der rechte Teilbaum leer, packe das Atom in eine Liste und füge die Elemente aus dem rekursiven Aufruf mit dem linken Teilbaum links davon.
4. Gibt es beide Teilbäume, packe das Atom in eine Liste und füge die Elemente aus dem rekursiven Aufruf mit dem linken Teilbaum links, und die Elemente aus dem rekursiven Aufruf mit dem rechten Teilbaum rechts davon.
