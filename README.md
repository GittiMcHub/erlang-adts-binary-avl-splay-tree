# Erlang Implementierung der Binärbaum ADTs: B-, AVL- & Splay-Tree

Im Rahmen des Studiengangs "Angewandte Informatik" sollten für das Fach "Algorithmen und Datenstrukturen" diverse abstrakte Datentypen implementiert werden.

Dieses Repo enthält die Erlang Implementierungen für folgende ADTs:\
[Binärbaum](btree.erl)\
[AVL-Baum](avltree.erl)\
[Splay Baum](splaytree.erl)

Neben den Implementierungen gibt es die Entwürfe zu den einzelnen ADTs unter: [documentation/README.md](documentation/README.md)\
[Entwurf: Binärbaum](documentation/btree.md)\
[Entwurf: AVL-Baum](documentation/avltree.md)\
[Entwurf: Splay Baum](documentation/splaytree.md)



Status/TODOs:
- Illustrationen in die Dokumentation einfügen
- Hinweis auf Listhandling bei inOrderBT

## Getting Started

### Vorraussetzungen

-> https://www.erlang.org/

Installierbar auf Ubuntu z.B. mit

```
sudo apt install erlang
```

### Nutzung

Module kompillieren (Beispiele mit der Erlangshell "erl"):

```
erl> c(btree), c(avltree), c(splaytree).
```

Beispiel Element "99" einfügen:

```
erl> avltree:insertBT(avltree:initBT(), 99).
```
oder
```
erl> avltree:insertBT({}, 99).
```

# Schnittstellen

## Datenstruktur
Für die Implementierung in Erlang werden Tupel mit folgender Struktur verwendet:

```
{<Element>,<Höhe>,<linker Teilbaum>,<rechter Teilbaum>}
```
Ein leerer Baum wird als leeres Tupel „{}“ repräsentiert.
Fehlerhafte Operationen auf dem Baum werden schlicht nicht ausgeführt und das Ergebnis sieht aus, als hätte diese Operation nicht stattgefunden.

### Element
Das Element eines Knoten ist ein Integer Atom

### Höhe
Die Höhe des Baumes bestimmt, wie viele Elemente es „nach Unten“ geht und wird als Integer
Atom dargestellt.

### Linker & Rechter Teil des Baumes
Das Element des linken Teilbaumes ist kleiner als das Integer Atom. Das Element des rechten Teilbaums ist größer als das Integer Atom. Die Teilbäume folgen der oben definierten Datenstruktur.

## BTree
[Vorgehensweisen](documentation/btree.md)
### initBT: ∅ → btree
Die Funktion „initBT“ bekommt keinen Übergabeparameter und gibt einen leeren Baum zurück, welcher als leeres Tupel repräsentiert wird.

### isEmptyBT: btree → bool
Die Funktion prüft, ob der übergebene Btree leer ist und gibt ein Wahr oder Falsch zurück.

### equalBT: btree × btree → bool
Diese Funktion prüft zwei Bäume auf semantische Gleichheit

### insertBT: btree × elem → btree
Diese Funktion fügt einem Binär Baum ein Blatt hinzu und gibt den erweiterten Binär Baum zurück

### deleteBT: btree × elem → btree
Diese Funktion löscht ein Element aus einem Baum.

### findBT: btree × elem → integer
Diese Funktion bekommt ein Baum in dem das übergeben Element gesucht ist und gibt die Höhe des Elementes innerhalb des Baumes zurück.

### inOrderBT: btree → list
Diese Funktion bekommt einen Baum übergeben, und gibt Diesen als sortierte Liste zurück

## AVLTree
[Vorgehensweisen](documentation/avltree.md)
### initBT: ∅ → btree
Die Funktion „initBT“ bekommt keinen Übergabeparameter und gibt einen leeren Baum zurück, welcher als leeres Tupel repräsentiert wird.\
[Vorgehensweise](documentation/avltree.md#initbt---btree)

### isEmptyBT: btree → bool
Die Funktion prüft, ob der übergebene Btree leer ist und gibt ein Wahr oder Falsch zurück.
[Vorgehensweise](documentation/avltree.md#isemptybt-btree--bool)


### equalBT: btree × btree → bool
Diese Funktion prüft zwei Bäume auf semantische Gleichheit
[Vorgehensweise](documentation/avltree.md#equalbt-btree--btree--bool)

### isBT: btree → bool
Prüft, ob der Baum ein Binärbaum mit AVL Eigenschaften ist.
[Vorgehensweise](documentation/avltree.md/avltree.md#isbt-btree--bool)

### insertBT: btree × elem → btree
Diese Funktion fügt einem AVL Baum ein Blatt hinzu und gibt den erweiterten AVL Baum zurück
[Vorgehensweise](documentation/avltree.md#insertbt-btree--elem--btree)

### deleteBT: btree × elem → btree
Diese Funktion löscht ein Element aus einem Baum. Und zwar ist es wichtig, dass der Baum danach immer noch der Datenstrukturdefinition entspricht sowie AVL ausgeglichen ist.
[Vorgehensweise](documentation/avltree.md#deletebt-btree--elem--btree)

### findBT: btree × elem → integer
Diese Funktion bekommt ein Baum in dem das übergeben Element gesucht ist und gibt die Höhe des Elementes innerhalb des Baumes zurück.
[Vorgehensweise](documentation/avltree.md#findbt-btree--elem--integer)

### inOrderBT: btree → list
Diese Funktion bekommt einen Baum übergeben, und gibt Diesen als sortierte Liste zurück
[Vorgehensweise](documentation/avltree.md#inorderbt-btree--list)

### printBT: btree × filename → dot
Diese Funktion schreibt den Baum im dot Format (https://www.graphviz.org/doc/info/lang.html) in die angegebene Datei. Dabei werden nur die für AVL Bäume notwendigen Notationen unterstützt.
Die Dateistruktur für einen Binär Baum könnte beispielsweise wie folgt aussehen:\
```
digraph avltree {
10 → 5 [label = 1];
10 → 15 [label = 1];
}
```
[Vorgehensweise](documentation/avltree.md#printbt-btree--filename--dot)


## SplayTree
[Vorgehensweisen](documentation/splaytree.md)
### initBT: ∅ → btree
Die Funktion „initBT“ bekommt keinen Übergabeparameter und gibt einen leeren Baum zurück, welcher als leeres Tupel repräsentiert wird.

### isEmptyBT: btree → bool
Die Funktion prüft, ob der übergebene Btree leer ist und gibt ein Wahr oder Falsch zurück.

### equalBT: btree × btree → bool
Diese Funktion prüft zwei Bäume auf semantische Gleichheit.

### eqBT: btree × btree → bool
Diese Funktion prüft zwei Bäume auf semantische und syntaktische Gleichheit.

### isBT: btree → bool
Diese Funktion prüft ob der Übergebene Baum ein Binärbaum ist. Außerdem werden die Datentypen geprüft.

### insertBT: btree × elem → btree
Diese Funktion fügt einem Splay Baum ein Blatt hinzu und gibt den veränderten/erweiterten Splay Baum zurück

### deleteBT: btree × elem → btree
Diese Funktion löscht ein Element aus einem Baum. Das Element wird zur Wurzel bewegt, dann gelöscht und anschließend werden die zwei Teilbäume zu einem umgewandelt.

### findBT: btree × elem → integer, btree
Diese Funktion bekommt ein Baum in dem das übergebene Element gesucht ist und gibt die Höhe des Elementes innerhalb des Baumes zurück. Hier bleibt der Baum unverändert, deswegen wird der originale Baum einfach als zusätzlicher Parameter gespeichert und sobald die richtige Höhe bestimmt wurde, wird auch der originale Baum zurückgegeben.   

### findTP: btree × elem → integer, btree
Diese Funktion bekommt ein Baum in dem das übergebene Element gesucht ist und gibt die Höhe des Elementes innerhalb des Baumes zurück. Das gesuchte Element soll jetzt zur Wurzel gebracht werden. Da der Baum sich verändern wird, übergibt jeder Teil der Methode auch den (Teil-)Baum zurück.

### inOrderBT: btree → list
Diese Funktion bekommt einen Baum übergeben, und gibt diesen als sortierte Liste zurück.

### printBT: btree × filename → dot
Diese Funktion schreibt den Baum im dot Format (https://www.graphviz.org/doc/info/lang.html) in die angegebene Datei. Dabei werden nur die für AVL Bäume notwendigen Notationen unterstützt.


## Autor

* **GittiMcHub** - [GittiMcHub](https://github.com/GittiMcHub)
* Teampartner

## Lizenz

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
