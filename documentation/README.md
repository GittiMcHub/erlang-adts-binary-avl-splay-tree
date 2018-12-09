# Quellenangaben
Quellenangaben zu textpassagen sind mit Fußnoten markiert.

Für die Implementierung der Balancierung und Insert Funktionalität wurde maßgeblich auf
https://www.geeksforgeeks.org/avl-tree-set-1-insertion/, 
für die Implementierung der Löschfunktionalität hauptsächlich auf 
https://www.geeksforgeeks.org/avl-tree-set-2-deletion/
zurückgriffen.

Illustration 1:
https://de.wikipedia.org/wiki/Bin%C3%A4rbaum#/media/File:Binary-tree.svg, abgerufen 28.11.2018 19:43

Illustration 2: https://de.wikipedia.org/wiki/Bin%C3%A4rbaum#/media/File:Binary-tree.svg, abgerufen
28.11.2018 19:43. Abwandlung vom Original nach Creative-Commons-Lizenz „Namensnennung – Weitergabe unter gleichen Bedingungen 3.0 Deutschland“ 
Urheber: https://de.wikipedia.org/wiki/Benutzer:Nomen4Omen

Illustration 3:
TODO: ersetzen
Illustration 4:
TODO: ersetzen

Illustration 5 und 6
Inhalt von https://www.geeksforgeeks.org/avl-tree-set-1-insertion/, abgerufen 29.11.2018
Xpath: //*[@id="post-17679"]/div[2]/pre[3]

Illustration 7 und 8:
Inhalt von https://www.geeksforgeeks.org/avl-tree-set-1-insertion/, abgerufen 29.11.2018
Xpath: //*[@id="post-17679"]/div[2]/pre[5]

Illustration 9:
Inhalt von https://www.geeksforgeeks.org/avl-tree-set-1-insertion/, abgerufen 30.11.2018
Xpath: //*[@id="post-17679"]/div[2]/pre[4]

Illustration 10:
Inhalt von https://www.geeksforgeeks.org/avl-tree-set-1-insertion/, abgerufen 30.11.2018
Xpath: //*[@id="post-17679"]/div[2]/pre[2]

# AVL-Baum
Diese Ausarbeitung setzt ein grundlegendes Verständnis zu Binären Suchbäumen voraus und beschreibt die bei AVL-Bäumen spezifischen Eigenschaften.

Der AVL-Baum besteht aus den folgenden vier Komponenten.
- Element
- Höhe des Elements
- Balance Faktor
- Linker Teilbaum
- Rechter Teilbaum


## Begrifflichkeit und Definition

TODO:
Illustration 1: Bezeichnungen in einem Binärbaum

Die Elemente des linken und rechten Teilbaumes heißen „Kind-Elemente“, während aus Sicht der Kind-Elemente das Element das „Eltern-Element“ darstellt.
Ein Element in Kombination mit seiner Höhe und mit Referenz auf seine Kinder, wird „Knoten“ genannt. 
Ein Knoten ohne Kinder heißt auch „Blatt“, ein Knoten mit nur einem Kind auch „Halb-Blatt“ und ein Knoten ohne Elternknoten, bzw. der oberste Knoten eines Teilbaumes auch „Wurzel“. 
Ein Knoten mit zwei Kindern wird „innerer Knoten“ genannt.<sup>[1](#note1)</sup>
Die „Höhe“ ist ein Attribut des Knotens <sup>[2](#note2)</sup> . Die Höhe -1 beschreibt die maximale Anzahl an Knoten bis zum Ende des Baumes. 
Ein Blatt hat die Höhe 1, Halbblätter sowie innere Knoten haben daher immer die Höhe > 1. 
Die Höhe eines Knotens berechnet sich aus der Maximalen Höhe der Kinder + 1. 
Die Höhe der Wurzel beschreibt gleichzeitig die Höhe des gesamten Baumes.

TODO:
Illustration 2: Hier dargestellt - die Höhe der einzelnen Knoten sowie Darstellung des längsten Pfades

Ein Konten eines binären Baumes ist „höhen balanciert“ (folgend nur noch „balanciert“ genannt), wenn sich die Höhen der beiden Kinder um höchstens 1 unterscheiden. 
Ein Binärbaum wird als „AVL Baum“ oder „AVL ausgeglichen“ bezeichnet, wenn alle Knoten balanciert sind.<sup>[3](#note3)</sup>
Der „Balance Faktor“ berechnet sich aus der Höhe des rechten – der Höhe des linken Kindes.

Ein nicht balancierter Baum könnte im schlechtesten Falle zu einer Liste entarten und weist dann
bei den Operationen Suchen, Löschen und Einfügen eine Komplexität von O(n) auf.
Die Höhenbalancierung erwirkt, dass der AVL-Baum in jedem Fall eine möglichst kleine Höhe hat,
womit eine effiziente Laufzeit der Suchbaum-Operationen gewährleistet ist.
Aus der Definition des AVL-Baumes folgt direkt, dass jeder Teilbaum eines AVL-Baumes ebenfalls
ein AVL-Baum ist. Dadurch ermöglicht dieser effizientes Suchen, Löschen und Einfügen von
Elementen mit einer Komplexität von O(log n).<sup>[4](#note4)</sup><sup>[5](#note5)</sup>

## Balancierung – mit einfacher Rotation
Nach jeder Einfüge- oder Löschoperation muss sichergestellt werden, dass die AVL Eigenschaften
nicht verletzt sind. Ob die AVL Eigenschaft eines Knotens vorhanden ist, wird erkannt, wenn sich
die Höhe des linken Kindes von der seines rechten Kindes um höchstens ± 1 unterscheidet.<sup>[6](#note6)</sup>
obald ein Knoten in Ungleichgewicht kommt, wird mittels Links- oder Rechtsrotation die Höhe
der Teilbäume beeinflusst, um die Balance wiederherzustellen.<sup>[7](#note7)</sup>

Gegeben seien die beiden Knoten a und b. Bei der Linksrotation wird der Knoten a um eine Ebene
herabgesetzt und bekommt den Teilbaum L2 als Kind, während der Knoten a jetzt das Linke Kind
von b darstellt. Die Höhe des Teilbaum R hat sich um ein reduziert. Dieser Effekt wird genutzt, um
die Balance des Baumes zu beeinflussen. Die Rechtsrotation entspricht der Linksrotation in
spiegelverkehrter Verfahrensweise.

TODO: 
Illustration 3: Linksrotation (Überlast an Teilbaum R)
TODO:
Illustration 4: Rechtsrotation (Überlast am Teilbaum L)

Für das Balancieren eines Baumes werden vier Fälle unterschieden. 
Für die zwei folgenden Fälle reicht eine einfache Rotation aus, um die Balance wiederherzustellen.

### Rechts-Rechts-Überlast (Right-Right-Case)
In der gegeben Illustration 3 kam es beim Knoten a zu einem Ungleichgewicht durch den rechten
Teilbaum b, der wiederum eine Überlast in dessen rechten Teilbaum R hatte. Durch die
Linksrotation wurde dieser Teilbaum wieder ins Gleichgewicht gebracht.

### Links-Links-Überlast (Left-Left-Case)
In der gegeben Illustration 4 kam es beim Knoten a zu einem Ungleichgewicht durch den linken
Teilbaum b, der wiederum eine Überlasst in dessen linken Teilbaum L hatte. Auch hier wird durch
die Rechtsrotation die Balance wiederhergestellt werden.

##Balancierung – mit doppelter Rotation
Angenommen die Überlast aus der Illustration 3 wäre im Teilbaum L2, oder in Illustration 4 im
Teilbaum R2, dann würde eine einfache Rotation das Ungleichgewicht nicht korrigieren. Für die
folgenden zwei Fälle muss dann eine Doppelrotation stattfinden.

### Links-Rechts-Überlast (Left-Right-Case)
Bei der Links-Rechts-Überlast wird eine sogenannte „Doppelrotation Rechts“ vorgenommen. Im
Grunde wird vom Unbalancierten Knoten (hier z) erst mit dem Linken Teilbaum eine Links-
Rotation und anschließend mit dem Knoten selbst eine Rechts-Rotation durchführt.<sup>[8](#note8)</sup>

TODO: 
Illustration 5: Doppelrotation-Rechts - Schritt 1: Links-Rotation mit dem Knoten y

TODO:
Illustration 6: Doppelrotation-Rechts - Schritt 2: Rechts-Rotation mit dem Knoten z

### Rechts-Links-Überlast (Right-Left-Case)
Bei der Rechts-Links-Überlast wird eine sogenannte „Doppelrotation Links“ vorgenommen. Bei
dem vom Unbalanicerten Knoten aus erst das rechte Kind nach rechts, und anschließend der Knoten
selbst nach links rotiert wird.

TODO:
Illustration 7: Doppelrotation-Links - Schritt 1: Rechts-Rotation mit dem Knoten y

TODO:
Illustration 8: Doppelrotation-Links - Schritt 2: Links-Rotation mit dem Knoten z

# Vorgehensweisen bei der Implementierung
Für die Implementierung in Erlang werden Tupel mit folgender Struktur verwendet:

```
{<Element>,<Höhe>,<linker Teilbaum>,<rechter Teilbaum>}
```
Ein leerer Baum wird als leeres Tupel „{}“ repräsentiert.
Fehlerhafte Operationen auf auf dem Baum werden schlicht nicht ausgeführt und das Ergebnis sieht aus, als hätte diese Operation nicht stattgefunden.

## Schnittstellen

### initBT: ∅ → btree
1. Es wird ein leeres Tupel {} zurückgeben

### isEmptyBT: btree → bool
1. Prüfe ob btree ein leeres Tupel ist
2. Gebe den Wahrheitswert zurück

### equalBT: btree × btree → bool
1. Wenn einer der beiden Bäume leer und der Andere nicht leer ist, gebe „false“ zurück
2. Beide Bäume in Order bringen
3. Gebe den Wahrheitswert der Abfrage inOrder(BaumA) == inOrder(BaumB) zurück

### isBT: btree → bool
1. Bekomme ein Baum übergeben
2. Prüfe, ob der Übergebene Baum der Struktur {<Element>,<Höhe>,<linker Teilbaum>,<rechter Teilbaum>} entspricht oder ein leeres Tupel ist <sup>[10](#note10)</sup>
4. Prüfe, ob das Element und die Höhe Integer Atome, und die Teilbäume Tupel sind <sup>[11](#note11)</sup>
5. Prüfe, ob die Kinder der Binärbaum Eigenschaft entsprechen (Linke Kinder kleiner, Rechte größer dem Element)
6. Prüfe, ob die Höhe korrekt gebildet wurde (siehe Definition und Begrifflichkeit „Höhe“)
7. Prüfe, ob der Knoten AVL ausgeglichen ist: Ist einer der Teilbäume leer, nehme 0 als Höhe
8. Prüfe, ob die Teilbäume links und rechts ebenfalls AVL Bäume sind

### insertBT: btree × elem → btree
1. Ist der (Teil-)Baum Leer, wird das Atom mit der Höhe 1 in Baumstruktur zurückgeben
2. Vergleiche das Element mit dem Atom des Knotens:
- ist das Element kleiner dem Atom, füge das Element mit dieser Funktion in den Linken Teilbaum ein
- ist das Element größer dem Atom, füge das Element mit dieser Funktion in den rechten Teilbaum ein

3. Addiere 1 zur Höhe des Knotens, wenn sich die maximale Höhe der Kinder verändert hat
4. Prüfe, ob die Höhe des Knotens angepasst werden muss (d.h. die Höhe des neuen Kindes – die Höhe des Knotens ergibt 0)\
4a. Hat sich die Höhe nicht geändert, hat sich die Balance des Knotens nicht verändert und der Baum kann zurückgegeben werden\
4b. Muss die Höhe angepasst werden, addiere 1 zur maximalen Höhe der Kinder
5. Hat sich die Balance geändert, wird geprüft, ob die Balance noch stimmt (Differenz Höhe des rechten Kindes und Höhe des linken Kindes)\
5a. Liegt die Differenz zwischen -1 und 1, ist der Baum in Balance und kann zurückgegeben werden\
5b. Liegt die Differenz außerhalb von -1 und 1, muss balanciert werden
6. Unterscheide 4 Fälle<sup>[12](#note12)</sup>\
6a. Left-Left Case - Balancierung – mit einfacher Rotation\
Die Überlast kommt vom linken Kind im linken Teilbaum\
→ Führe eine Rechtsrotation an der Wurzel dieses (Teil-)Baumes durch\
(siehe rotateRight: btree → btree)
6b. Left-Right Case - Balancierung – mit doppelter Rotation\
Die Überlast kommt vom rechten Kind im linken Teilbaum\
→ Führe eine Linksrotation mit dem linken Teilbaum durch und anschließend eine Rechtsrotation an der Wurzel dieses (Teil-)Baumes\
(siehe rotateLeft: btree → btree und rotateRight: btree → btree)
6c. Right-Right Case - Balancierung – mit einfacher Rotation\
Die Überlast kommt vom rechten Kind im rechten Teilbaum\
→ Führe eine Linksrotation an der Wurzel dieses (Teil-)Baumes durch\
(siehe rotateLeft: btree → btree)
6d. Right-Left Case - Balancierung – mit doppelter Rotation\
Die Überlast kommt vom linken Kind im rechten Teilbaum\
→ Führe eine Rechtsrotation mit dem rechten Teilbaum durch und anschließend eine\
Linksrotation an der Wurzel dieses (Teil-)Baumes\
(siehe rotateRight: btree → btree und rotateLeft: btree → btree )
7. Gebe den (Teil-)Baum zurück

### deleteBT: btree × elem → btree
1. Vergleiche das Übergebene Element mit dem Atom des Baumknotens
2. Ist das Element ungleich dem Atom und der Knoten hat keine Kinder mehr, ist das Element nicht
vorhanden und der leere Baum wird zurückgegeben
3. Ist das Element kleiner dem Atom, versuche das Atom im linken Teilbaum zu löschen und fahre
mit Schritt 9 fort.
4. Ist das Element größter dem Atom, versuche es im rechten Teilbaum zu löschen und fahre mit
Schritt 9 fort.
5. Ist das Element gleich dem Atom, unterscheide folgende Fälle:\
5a. Hat der Baumknoten keine Kinder, kann das Element gelöscht werden und ein leerer Baum zurückgegeben werden\
5b. Hat der Baumknoten nur ein Kind, kann der entsprechende Teilbaum den Platz einnehmen und zurückgegeben werden\
5c. Der Baumknoten hat links und rechts Teilbäume und der Ablauf geht weiter
6. Suche und merke das kleinste Element des rechten Teilbaumes\
(siehe findTheSmallest: btree -> Integer)
7. Lösche das kleinste Element aus dem rechten Teilbaum
8. Ersetze das zu löschende Element des Baumknotens mit dem gemerkten kleinsten Element des rechten Teilbaumes
9. Prüfe, ob sich die Höhe geändert hat\
9a. Wenn sich die Höhe nicht geändert hat, gebe den (Teil-)Baum zurück\
9b. Hat sich die Höhe geändert, passe die Höhe an (Höhe -1)\
10. Prüfe, ob sich der Knoten noch AVL ausgeglichen ist (Höhe rechtes Kind – Höhe Linkes Kind zwischen -1 und 1)\
10a. Ist der Knoten in Balance, gebe den (Teil-)Baum zurück\
10b. Ist der Knoten im Ungleichgewicht , unterscheide 4 Fälle<sup>[13](#note13)</sup>\
11a. Left-Left Case - Balancierung – mit einfacher Rotation\
Die Überlast kommt vom linken Kind im linken Teilbaum\
→ Führe eine Rechtsrotation an der Wurzel dieses (Teil-)Baumes durch\
(siehe rotateRight: btree → btree)\
11b. Left-Right Case - Balancierung – mit doppelter Rotation\
Die Überlast kommt vom rechten Kind im linken Teilbaum\
→ Führe eine Linksrotation mit dem linken Teilbaum durch und anschließend eine\
Rechtsrotation an der Wurzel dieses (Teil-)Baumes\
(siehe rotateLeft: btree → btree und rotateRight: btree → btree)\
11c. Right-Right Case - Balancierung – mit einfacher Rotation\
Die Überlast kommt vom rechten Kind im rechten Teilbaum\
→ Führe eine Linksrotation an der Wurzel dieses (Teil-)Baumes durch\
(siehe rotateLeft: btree → btree)\
11d. Right-Left Case - Balancierung – mit doppelter Rotation\
Die Überlast kommt vom linken Kind im rechten Teilbaum\
→ Führe eine Rechtsrotation mit dem rechten Teilbaum durch und anschließend eine\
Linksrotation an der Wurzel dieses (Teil-)Baumes\
(siehe rotateRight: btree → btree und rotateLeft: btree → btree )
12. Gebe den veränderten Baum zurück

### findBT: btree × elem → integer
1. Ist der Baum leer, kann das Element nicht vorhanden sein und gebe 0 zurück
2. Ist das Element gleich dem Atom des aktuellen Baumknotens, gebe die Höhe des aktuellen Baumknotens zurück
3. Ist das Element kleiner dem Atom des aktuellen Baumknotens, gehe in den linken Teilbaum und Springe zu Schritt 1
4. Ist das Element größer dem Atom des aktuellen Baumknotens, gehe in den rechten Teilbaum und Springe zu Schritt 1

### inOrderBT: btree → list
1. Sind der linke und rechte Teilbaum leer, packe das aktuelle Atom in eine Liste
2. Ist nur der linke Teilbaum leer, packe das Atom in eine Liste und füge die Elemente aus dem rekursiven Aufruf mit dem rechten Teilbaum rechts davon.
3. Ist nur der rechte Teilbaum leer, packe das Atom in eine Liste und füge die Elemente aus dem rekursiven Aufruf mit dem linken Teilbaum links davon.
4. Gibt es beide Teilbäume, packe das Atom in eine Liste und füge die Elemente aus dem rekursiven Aufruf mit dem linken Teilbaum links, und die Elemente aus dem rekursiven Aufruf mit dem rechten Teilbaum rechts davon.

### printBT: btree × filename → dot
1. Schreibe die Graphbezeichnung „digraph“ und den Namen „avltree“ in die Datei<sup>[15](#note15)</sup> . Füge eine
geschweifte Klammer ein, um die stmt_list <sup>[16](#note16)</sup> zu öffnen.
Die geschriebene Zeile sollte also wie folgt aussehen: digraph avltree {
2. Schreibe je eine Zeile für den linken und rechten Teilbaum wie folgt\
<Element des Knotens> → <Element des Kindknotens> [label = <Höhe des Kindknotens>];
3. Für leere Teilbäume wird nichts geschrieben.
4. Rufe diese Funktion für vorhandene Teilbäume wieder auf
5. Füge als Letzte Zeile die schließende geschweifte Klammer ein um die stmt_list zu schließen


## Interne Funktionen
### findTheSmallest: btree → integer
Beschreibung
Private Hilfsmethode um den niedrigsten Wert herauszufinden. Da der Linke Teilbaum immer
kleiner Seiner Zahl ist, muss nur die Linke Seite durchsucht werden. Diese Methode wird beim
Löschen benötigt.

1. Ist der linke Teilbaum leer, gebe das Element des Knotens zurück
2. Ist der linke Teilbaum nicht leer, führe diese Funktion auf den Linken Teilbaum aus

## rotateLeft: btree → btree
Beschreibung
Diese Funktion wird intern für die links Rotation eines (Teil-)Baumes verwendet und führt die im Kapitel Balancierung – mit einfacher Rotation beschriebene Linksrotation durch.

TODO: 
Illustration 9: Vorlage für die Vorgehensweise bei Linksrotation

1. Sei z die Wurzel des (Teil-)Baumes. Der Teilbaum T1 das linke und y das rechte Kind von z. Sei T2 der linke Teilbaum von y und x das rechte Kind von y. x hat die Kinder T3 (links) und T4 (rechts) <sup>[17](#note17)</sup>
2. Verteile die Bäume nun so, dass T2 das rechte Kind von z wird und y die neue Wurzel des (Teil-)Baumes mit x als rechtes und z als linkes Kind ist. Die Teilbäume von x bleiben in der Höhe unverändert
3. Korrigiere die Höhe vom neuen Wurzelknoten y mit der maximalen Höhe der beiden Kinder + 1
4. Korrigiere die Höhe von z mit der maximalen Höhe der beiden Kinder +1
5. Gebe den rotierten Baum zurück 

### rotateRight: btree → btree
Beschreibung
Diese Funktion wird intern für die Rechtsrotation eines (Teil-)Baumes verwendet und führt die im
Kapitel Balancierung – mit einfacher Rotation beschriebene Rechtsrotation durch.

TODO: Illustration 10: Vorlage für die Vorgehensweise bei Rechtsrotation
Vorgehensweise
1. Sei z die Wurzel des (Teil-)Baumes. Der Teilbaum T4 das rechte Kind und y das linke Kind von
z. Sei T3 der rechte Teilbaum von y und x das linke Kind von y. x hat die Kinder T1 (links) und T2
(rechts) <sup>[18](#note18)</sup>
2. Verteile die Bäume nun so, dass T3 das linke Kind von z wird und y die neue Wurzel des
(Teil-)Baumes mit x als linkes und z als rechtes Kind ist.
3. Korrigiere die Höhe vom neuen Wurzelknoten y mit der maximalen Höhe der beiden Kinder + 1
4. Korrigiere die Höhe von z mit der maximalen Höhe der beiden Kinder +1
5. Gebe den rotierten Baum zurück

# Fußnoten
<a name="note1">1</a>: https://de.wikipedia.org/wiki/Bin%C3%A4rer_Suchbaum#Terminologie
<a name="note2">2</a>: https://de.wikipedia.org/wiki/Bin%C3%A4rbaum#Weitere_Begriffe , 28.11.2018
<a name="note3">3</a>: http://www-lehre.informatik.uni-osnabrueck.de/~ainf/2000/skript/node60.html , 28.11.2018
<a name="note4">4</a>: Binäre Suchbäume - Ein Leitprogramm von Timur Erdag und Björn Steffen – Seite 47 Publiziert auf EducETH: 14. April 2008
<a name="note5">5</a>: Binäre Suchbäume - Ein Leitprogramm von Timur Erdag und Björn Steffen – Seite 49 Publiziert auf EducETH: 14. April 2008
<a name="note6">6</a>: https://de.wikipedia.org/wiki/Balancierter_Baum#H%C3%B6henbalance , 29.11.2018
<a name="note7">7</a>: https://de.wikipedia.org/wiki/Bin%C3%A4rbaum#Rotation_in_bin%C3%A4ren_B%C3%A4umen , 29.11.2018
<a name="note8">8</a>: https://www.geeksforgeeks.org/avl-tree-set-1-insertion/ Abschnitt: b) Left Right Case, 29.11.2018
<a name="note9">9</a>: https://www.geeksforgeeks.org/avl-tree-set-1-insertion/ Abschnitt: d) Right Left Case, 29.11.2018
<a name="note10">10</a>: https://www.geeksforgeeks.org/avl-tree-set-1-insertion/ Abschnitt: d) Right Left Case, 29.11.2018
<a name="note11">11</a>: https://www.geeksforgeeks.org/avl-tree-set-1-insertion/ Abschnitt: d) Right Left Case, 29.11.2018
<a name="note11">12</a>: https://www.geeksforgeeks.org/avl-tree-set-2-deletion/
<a name="note13">13</a>: https://www.geeksforgeeks.org/avl-tree-set-1-insertion/
<a name="note14">14</a>: https://www.graphviz.org/doc/info/lang.html
<a name="note15">15</a>: http://erlang.org/doc/man/file.html#write_file-3
<a name="note16">16</a>: https://www.graphviz.org/doc/info/lang.html Xpath: /html/body/table/tbody/tr[2]/td[1]/i
<a name="note17">17</a>: https://www.geeksforgeeks.org/avl-tree-set-1-insertion/ Xpath: //*[@id="post-17679"]/div[2]/pre[4]
<a name="note18">18</a>: https://www.geeksforgeeks.org/avl-tree-set-1-insertion/ Xpath: //*[@id="post-17679"]/div[2]/pre[2]
<a name="note19">19</a>: Desktop User Guide for MicroStrategy 10, Seite 199
<a name="note20">20</a>: http://www-lehre.informatik.uni-osnabrueck.de/~ainf/2000/skript/node60.html , 30.11.2018
<a name="note21">21</a>: http://erlang.org/doc/efficiency_guide/listHandling.html , 01.12.2018
