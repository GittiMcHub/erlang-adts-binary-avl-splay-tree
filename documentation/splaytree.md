# Quellenangaben
Für die Splay Baum Funktionen wurde auf folgende Quellen zurückgegriffen\
https://en.wikipedia.org/wiki/Splay_tree
Entwurf und Analyse von Algorithmen: Eine Einführung in die Algorithmik mit Java – Authoren Markus Nebel, Sebastian Wild\
Für diesen Entwurf wurden auch Teile aus den Entwürfen von [avltree](avltree.md) und [btree](btree.md) teilweise und /oder vollständig (Wort-wörtlich) übernommen.
Die Quellen der Rotationen und PrintBT sind im Entwurf von [avltree](avltree.md) genannt.

# Splay-Baum
\
Der Splay-Baum besteht aus den folgenden vier Komponenten.\
- Element\
- Höhe des Elements\
- Linker Teilbaum\
- Rechter Teilbaum\
\
# Erlang Implementierung
## Datenstruktur
Für die Implementierung in Erlang werden Tupel mit folgender Struktur {<Element>,<Höhe>,<linker Teilbaum>,<rechter Teilbaum>} verwendet. Ein leerer Baum wird als leeres Tupel „{}“ repräsentiert.\
Fehlerhafte Operationen auf auf dem Baum werden schlicht nicht ausgeführt und das Ergebnis sieht aus, als hätte diese Operation nicht stattgefunden.\
\
### Element
Das Element eines Knoten ist ein Integer Atom\
\
### Höhe
Die Höhe des Baumes bestimmt, wie viele Elemente es „nach Unten“ geht und wird als Integer Atom dargestellt.\
\
### Linker & Rechter Teil des Baumes
Das Element des linken Teilbaumes ist kleiner als das Integer Atom. Das Element des rechten Teilbaums ist größer als das Integer Atom. Die Teilbäume folgen der oben definierten Datenstruktur.\
\
## Schnittstellen
### initBT: ∅ → btree
Beschreibung\
Die Funktion „initBT“ bekommt keinen Übergabeparameter und gibt einen leeren Baum zurück, welcher als leeres Tupel repräsentiert wird.\
Vorgehensweise\
1. Es wird ein leeres Tupel {} zurückgeben\
\
### isEmptyBT: btree → bool
Beschreibung\
Die Funktion prüft, ob der übergebene Btree leer ist und gibt ein Wahr oder Falsch zurück. \
Vorgehensweise\
1. Prüfe ob btree ein leeres Tupel ist\
2. Gebe den Wahrheitswert zurück\
\
### equalBT: btree × btree → bool
Beschreibung\
Diese Funktion prüft zwei Bäume auf semantische Gleichheit\
Vorgehensweise\
1. Wenn einer der beiden Bäume leer und der Andere nicht leer ist, gebe „false“ zurück\
2. Beide Bäume in Order bringen\
3. Gebe den Wahrheitswert der Abfrage inOrder(BaumA) == inOrder(BaumB) zurück\
\
### eqBT: btree → bool
Beschreibung\
Diese Funktion prüft zwei Bäume auf semantische und syntaktische Gleichheit\
Vorgehensweise\
1. Wenn Beide Bäume leer sind, gebe „true“ zurück. \
2. Wenn einer der beiden Bäume leer und der Andere nicht leer ist, gebe „false“ zurück \
3. Überprüfe ob Atomwerte und Hohe von beiden gleich sind.\
3a) Wenn nicht, gebe „false“ zurück. \
3b) Wenn ja, dann rufe eqBT mit beiden rechten Teilen und mit beiden linken Teilen auf. \
\
### isBT: btree → bool
Beschreibung\
Diese Funktion prüft ob der Übergebene Baum ein Binärbaum ist. Außerdem werden die Datentypen geprüft.\
Vorgehensweise\
1. Bekomme ein Baum übergeben. Setze das Maximum als Grenze zu einer sehr großen Zahl und das Minimum auf 0. \
2. Prüfe, ob der übergebene Baum der Struktur {<Element>,<Höhe>,<linker Teilbaum>,<rechter Teilbaum>} entspricht oder ein leeres Tupel ist \
4. Prüfe, ob das Element und die Höhe Integer Atome, und die Teilbäume Tupel sind \
5. Prüfe, ob das Element kleiner dem Maximum und größer dem Minimum ist. \
6. Prüfe, ob die Kinder der Binärbaum Eigenschaft entsprechen (Linke Kinder kleiner, Rechte größer dem Element). \
7. Prüfe, ob die Höhe korrekt gebildet wurde\
8. Prüfe, ob die Teilbäume links und rechts ebenfalls Splay Bäume sind. Für den linken Teil, nehme das Maximum Atom als neues Maximum, für den Rechten, setze als neues Minimum das Atom. \
\
### insertBT: btree × elem → btree
Beschreibung\
Diese Funktion fügt einem Splay Baum ein Blatt hinzu und gibt den veränderten/erweiterten Splay Baum zurück\
Vorgehensweise\
2. InsertWithSplay:\
2a.  Wenn das Element größer als das Atom ist, muss insertWithSplay mit dem Rechten Baum durchgeführt werden. \
Wenn die Höhe des neuen Rechten Baumes gleich der Höhe des Atoms ist, soll die Höhe des Baumes um 1 erhöht werden.\
Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen rechten Teil) und den Marker «right» zurückliefern.\
Wenn der Marker «right» ist, wird die zag-zag Operation auf den Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.\
Wenn der Marker «left» ist, wird die zag-zig Operation auf den Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.\
2b.  Wenn das Element kleiner als das Atom ist, muss insertWithSplay mit dem Linken Baum durchgeführt werden. \
Wenn die Höhe des neuen Linken Baumes gleich der Höhe des Atoms ist, soll die Höhe des Baumes um 1 erhöht werden.\
Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen linken Teil) und den Marker «left» zurückliefern.\
Wenn der Marker «left» ist, wird die zig-zig Operation auf dem Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben. \
Wenn der Marker «right» ist, wird die zig-zag Operation auf dem Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben. \
2c. Wenn der Baum leer ist, setze hier das Element mit der Höhe 1. Gib diesen Leaf und den Marker «none» zurück.\
\
### findBT: btree × elem → integer
Beschreibung\
Diese Funktion bekommt ein Baum in dem das übergebene Element gesucht ist und gibt die Höhe des Elementes innerhalb des Baumes zurück. Hier bleibt der Baum unverändert, deswegen wird der originale Baum einfach als zusätzlicher Parameter gespeichert und sobald die richtige Höhe bestimmt wurde, wird auch der originale Baum zurückgegeben.              \
Vorgehensweise\
1. Der erste Funktionsaufruf ruft findWithSplay auf, dessen Rückgabewerte der Baum, die Höhe und der Marker sind. Dieser bestimmt die Rotation. \
1a) Wenn der Marker «none» ist, soll nichts gemacht werden und die Höhe und der Baum, den find geliefert hat wird einfach zurückgeben. \
1b) Wenn der Marker «left» ist, dann ist das hinzugefügte Element in dem linken Teilbaum, und es muss die zig-Operation durchgeführt werden.\
1c) Wenn der Marker «right» zeigt, dann entsprechend zag. \
Die Höhe und der Baum werden zurückgegeben. \
\
2. FindWithSplay:\
2a.  Wenn das gesuchte Element größer als das Atom ist, muss findWithSplay mit dem Rechten Baum durchgeführt werden. \
Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen rechten Teil),  die Höhe und den Marker «right» zurückliefern.\
Wenn der Marker «right» ist, wird die zag-zag Operation auf den Baum durchgeführt. Der veränderte Baum, die Höhe und der Marker «none» werden zurückgegeben.\
Wenn der Marker «left» ist, wird die zag-zig Operation auf den Baum durchgeführt. Der veränderte Baum, die Höhe und der Marker «none» werden zurückgegeben.\
2b.  Wenn das gesuchte Element kleiner als das Atom ist, muss findWithSplay mit dem Linken Baum durchgeführt werden. \
Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen linken Teil), die Höhe und den Marker «left» zurückliefern.\
Wenn der Marker «left» ist, wird die zig-zig Operation auf dem Baum durchgeführt. Der veränderte Baum, die Höhe und der Marker «none» werden zurückgegeben. \
Wenn der Marker «right» ist, wird die zig-zag Operation auf dem Baum durchgeführt. Der veränderte Baum, die Höhe und der Marker «none» werden zurückgegeben. \
2c. Wenn das gesuchte Element gleich dem Atom ist, gib den Baum ohne Änderung, die Höhe des Atoms und den Marker «none» zurück.\
2d. Wenn der Baum leer ist, gib den Baum ohne Änderung, 0 und den Marker «none» zurück.\
\
### findPT: btree × elem → integer ×  btree
Beschreibung\
Diese Funktion bekommt ein Baum in dem das übergebene Element gesucht ist und gibt die Höhe des Elementes innerhalb des Baumes zurück. Das gesuchte Element soll jetzt zur Wurzel gebracht werden. Da der Baum sich verändern wird, übergibt jeder Teil der Methode auch den (Teil-)Baum zurück.\
Vorgehensweise\
1. Ist der Baum leer, kann das Element nicht vorhanden sein - gebe 0 und den Baum zurück.\
2. Ist das Element gleich dem Atom des aktuellen Baumknotens, gebe die Höhe des aktuellen Baumknotens und den unveränderten Baum zurück.\
3. Ist das Element kleiner dem Atom des aktuellen Baumknotens, rufe diese Funktion mit dem Linken Teilbaum auf. Wir erthalten einen neuen Linken Teilbaum und die gesuchte Höhe. \
4. Prüfe, ob das Atom eine Wurzel ist. (Dafür steht der Wurzelwert als letzter Parameter in der Funktion) \
4a. Wenn nicht, sollte zig-zig, zig-zag, zag-zag oder zag-zig durchgeführt werden, wenn das addierte Element ein Enkelkind ist. Wenn das addierte Element ein Kind ist, soll keine Operation durchgeführt werden. \
4b. Wenn ja, sollte überprüft werden, ob das addierte Element ein Enkelkind ist. In diesem Fall soll   zig-zig, zig-zag, zag-zag oder zag-zig durchgeführt werden. Wenn dieses Element ein Kind ist, dann soll zig oder zag durchgeführt werden. \
Die gesuchte Höhe und den neuen Baum wird zurück gegeben.\
5. Ist das Element größer dem Atom des aktuellen Baumknotens, rufe diese Funktion mit dem rechten Teilbaum.\
Wir enthalten die gesuchte Höhe und den neuen rechten Teilbaum. Gehe zu Schritt 4.\
\
### inOrderBT: btree → list
Beschreibung\
Diese Funktion bekommt einen Baum übergeben, und gibt diesen als sortierte Liste zurück\
Vorgehensweise\
1. Sind der linke und rechte Teilbaum leer, packe das  aktuelle Atom in eine Liste\
2. Ist nur der linke Teilbaum leer, packe das Atom in eine Liste und füge die Elemente aus dem rekursiven Aufruf mit dem rechten Teilbaum rechts davon.\
3. Ist nur der rechte Teilbaum leer, packe das Atom in eine Liste und füge die Elemente aus dem rekursiven Aufruf mit dem linken Teilbaum links davon.\
4. Gibt es beide Teilbäume, packe das Atom in eine Liste und füge die Elemente aus dem rekursiven Aufruf mit dem linken Teilbaum links, und die Elemente aus dem rekursiven Aufruf mit dem rechten Teilbaum rechts davon.\
\
###0 printBT:  btree × filename → dot
Beschreibung\
Diese Funktion schreibt den Baum im dot Format1 in die angegebene Datei. Dabei werden nur die für AVL Bäume notwendigen Notationen unterstützt.\
Vorgehensweise\
1. Schreibe die Graphbezeichnung „digraph“ und den Namen „avltree“ in die Datei2. Füge eine geschweifte Klammer ein, um die stmt_list3 zu öffnen.\
Die geschriebene Zeile sollte also wie folgt aussehen: digraph avltree {\
2. Schreibe je eine Zeile für den linken und rechten Teilbaum wie folgt\
<Element des Knotens> →  <Element des Kindknotens> [label = <Höhe des Kindknotens>];\
3. Für leere Teilbäume wird nichts geschrieben.\
4. Rufe diese Funktion für vorhandene Teilbäume wieder auf\
5. Füge als Letzte Zeile die schließende geschweifte Klammer ein um die stmt_list zu schließen\
\
###1 deleteBT: btree × elem → btree
Beschreibung\
Diese Funktion löscht ein Element aus einem Baum. Das Element wird zur Wurzel bewegt, dann gelöscht und anschließend werden die zwei Teilbäume zu einem umgewandelt.\
Vorgehensweise\
Der erste Funktionsaufruf ruft deleteWithSplay auf, dessen Rückgabewerte der Baum und der Marker sind. Dieser bestimmt die Rotation. \
1a) Wenn der Marker «none» ist, soll nichts gemacht werden und die Höhe und der Baum, den dieses delete geliefert hat, werden zurückgegeben\
1b) Wenn der Marker «left» ist, dann ist das gelöschte Element in dem linken Teilbaum, und es muss die zig-Operation durchgeführt werden.\
1c) Wenn der Marker «right» zeigt, dann entsprechend zag. \
Danach wird das größte Element in dem Linken Teilbaum gesucht und auch dort sofort gelöscht. \
Die Wurzel wird jetzt mit diesem Element substituiert.\
Der Baum wird zurückgegeben. \
\
2. DeleteWithSplay:\
2a.  Wenn das gesuchte Element größer als das Atom ist, muss deleteWithSplay mit dem Rechten Baum durchgeführt werden. \
Wenn beide Höhen der Teilbäume um 2 kleiner der Höhe des Atoms ist, muss die Höhe des Baumes um 1 reduziert werden. \
Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen rechten Teil) und den Marker «right» zurückliefern.\
Wenn der Marker «right» ist, wird die zag-zag Operation auf den Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.\
Wenn der Marker «left» ist, wird die zag-zig Operation auf den Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.\
2b.  Wenn das gesuchte Element kleiner als das Atom ist, muss findWithSplay mit dem Linken Baum durchgeführt werden. \
Wenn beide Höhen der Teilbäume um 2 kleiner der Höhe des Atoms sind, muss die Höhe des Baumes um 1 reduziert.\
Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen linken Teil) und den Marker «left» zurückliefern.\
Wenn der Marker «left» ist, wird die zig-zig Operation auf dem Baum durchgeführt. Der veränderte Baum, die Höhe und der Marker «none» werden zurückgegeben. \
Wenn der Marker «right» ist, wird die zig-zag Operation auf dem Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben. \
2c. Wenn das gesuchte Element gleich dem Atom ist, gib den Baum ohne Änderung und den Marker «none» zurück.\
\
## Interne Funktionen
### findTheSmallest: btree → integer
Beschreibung\
Private Hilfsmethode um den niedrigsten Wert herauszufinden. Da der Linke Teilbaum immer kleiner Seiner Zahl ist, muss nur die Linke Seite durchsucht werden. Diese Methode wird beim Löschen benötigt.\
Vorgehensweise\
1. Ist der linke Teilbaum leer, gebe das Element des Knotens zurück\
2. Ist der linke Teilbaum nicht leer, führe diese Funktion auf den Linken Teilbaum aus\
### rotateLeft: btree → btree
Beschreibung\
Diese Funktion wird intern für die links Rotation eines (Teil-)Baumes verwendet.\
Vorgehensweise\
1. Sei z die Wurzel des (Teil-)Baumes. Der Teilbaum T1 das linke und y das rechte Kind von z. Sei T2 der linke Teilbaum von y und x das rechte Kind von y. x hat die Kinder T3 (links) und T4 (rechts)4\
2. Verteile die Bäume nun so, dass T2 das rechte Kind von z wird und y die neue Wurzel des (Teil-)Baumes mit x als rechtes und z als linkes Kind ist. Die Teilbäume von x bleiben in der Höhe unverändert\
3. Korrigiere die Höhe vom neuen Wurzelknoten y  mit der maximalen Höhe der beiden Kinder + 1\
4. Korrigiere die Höhe von z mit der maximalen Höhe der beiden Kinder +1\
5. Gebe den rotierten Baum zurück\
\
### rotateRight: btree → btree
Beschreibung\
Diese Funktion wird intern für die Rechtsrotation eines (Teil-)Baumes verwendet.\
Vorgehensweise\
1. Sei z die Wurzel des (Teil-)Baumes. Der Teilbaum T4 das rechte Kind und y das linke Kind von z. Sei T3 der rechte Teilbaum von y und x das linke Kind von y. x hat die Kinder T1 (links) und T2 (rechts)5\
2. Verteile die Bäume nun so, dass T3 das linke Kind von z wird und y die neue Wurzel des (Teil-)Baumes mit x als linkes und z als rechtes Kind ist.\
3. Korrigiere die Höhe vom neuen Wurzelknoten y  mit der maximalen Höhe der beiden Kinder + 1\
4. Korrigiere die Höhe von z mit der maximalen Höhe der beiden Kinder +1\
5. Gebe den rotierten Baum zurück\
\
### findTheBiggest: btree → integer
Beschreibung\
Private Hilfsmethode um den höschten Wert herauszufinden. Da der rechte Teilbaum immer größter Seiner Zahl ist, muss nur die rechte Seite durchsucht werden. Diese Methode wird beim Löschen benötigt.\
Vorgehensweise\
1. Ist der rechte Teilbaum leer, gebe das Element des Knotens zurück\
2. Ist der rechte Teilbaum nicht leer, führe diese Funktion auf den rechten Teilbaum aus
