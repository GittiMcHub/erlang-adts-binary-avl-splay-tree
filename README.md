# Erlang ADTs B-Tree and AVL-Tree

Diese Repo enthält eine Erlang Implementierung für die Abstrakten Datentypen (ADT) Binary Tree (btree.erl) und AVL Tree (avltree.erl)

Neben der Implementierung gibt es noch eine ausführliche Ausarbeitung über die Vorgehens- und Funktionsweise des AVL Baumes.

Siehe dazu: documentation

Status/TODOs:
- Illustrationen in Dokumentation
- Hinweis auf Listhandling bei inOrderBT
- Laufzeitmessung auch aufnehmen?

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
erl> c(btree), c(avltree).
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
Fehlerhafte Operationen auf auf dem Baum werden schlicht nicht ausgeführt und das Ergebnis sieht aus, als hätte diese Operation nicht stattgefunden.

### Element
Das Element eines Knoten ist ein Integer Atom

### Höhe
Die Höhe des Baumes bestimmt, wie viele Elemente es „nach Unten“ geht und wird als Integer
Atom dargestellt.

### Linker & Rechter Teil des Baumes
Das Element des linken Teilbaumes ist kleiner als das Integer Atom. Das Element des rechten Teilbaums ist größer als das Integer Atom. Die Teilbäume folgen der oben definierten Datenstruktur.

## BTree

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

### initBT: ∅ → btree
Die Funktion „initBT“ bekommt keinen Übergabeparameter und gibt einen leeren Baum zurück, welcher als leeres Tupel repräsentiert wird.

### isEmptyBT: btree → bool
Die Funktion prüft, ob der übergebene Btree leer ist und gibt ein Wahr oder Falsch zurück.

### equalBT: btree × btree → bool
Diese Funktion prüft zwei Bäume auf semantische Gleichheit

### isBT: btree → bool

### insertBT: btree × elem → btree
Diese Funktion fügt einem AVL Baum ein Blatt hinzu und gibt den erweiterten AVL Baum zurück

### deleteBT: btree × elem → btree
Diese Funktion löscht ein Element aus einem Baum. Und zwar ist es wichtig, dass der Baum danach immer noch der Datenstrukturdefinition entspricht sowie AVL ausgeglichen ist.

### findBT: btree × elem → integer
Diese Funktion bekommt ein Baum in dem das übergeben Element gesucht ist und gibt die Höhe des Elementes innerhalb des Baumes zurück.

### inOrderBT: btree → list
Diese Funktion bekommt einen Baum übergeben, und gibt Diesen als sortierte Liste zurück

### printBT: btree × filename → dot
Diese Funktion schreibt den Baum im dot Format (https://www.graphviz.org/doc/info/lang.html) in die angegebene Datei. Dabei werden nur die für AVL Bäume notwendigen Notationen unterstützt.
Die Dateistruktur für einen Binär Baum könnte beispielsweise wie folgt aussehen:
```
digraph avltree {
10 → 5 [label = 1];
10 → 15 [label = 1];
}
```

## Autor

* **GittiMcHub** - [GittiMcHub](https://github.com/GittiMcHub)
* Teampartner

## Lizenz

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details


