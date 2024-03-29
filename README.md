# FLP - projekt 1

## Autor

Vojtěch Dvořák (xdvora3o)

## Popis

Program `flp-fun` načte binární rozhodovací strom a neanotovaná data ve fromátu CSV a na standardní výstup vypíše na jednotlivých řádcích třídy daných objektů. Dále umožňuje natrénovat rozhodovací strom na základě anotované datové sady. Režim činnosti je vybírán přepínačem `-1` pro inferenci tříd, resp. `-2` pro trénování stromů. Pro více informací o argumentech programu je možné použít přepínače `-h`, `--help`.

Program disponuje všemi vlastnosti a veškerou funkcionalitou popsanou v zadání a pro větší přehlednost je rozdělen do několika modulů.

Ústředním modulem pro analýzu stromů a dat v CSV je modul `Parser`, v rámci kterého bylo vytvořeno obecné rámcové řešení pro definování gramatik popisující formáty vstupních souborů. `CommonParser`, `DecisionTreeParser` a `MDataParser` funkce z tohoto modulu využívají a obsahují funkce právě pro analýzu jednotlivých formátů. Moduly `DecisionTree` a `MData` obsahují definice datových typů pro uložení rozhodovacích stromů a dat a také funkce pro práci s nimi.

Zodpovědnostmi modulů `Classifier` a `Trainer` je pak samotná klasifikace objektů (inference) a trénování nových binárních rozhodovaích stromů. Zbývající moduly `ArgumentParser` a `Main` obsahují pořadě analyzátor argumentů příkazové řádky a hlavní tělo programu `flp-fun`.

## Příklady spuštění

Trénování:

```
$ cat data.csv
1.0, 2.0, A
1.0, 2.7, B
1.0, 4.1, C
1.0, 2.2, D
$ ./flp-fun -2 data.csv
Node: 1, 3.4
  Node: 1, 2.45
    Node: 1, 2.1
      Leaf: A
      Leaf: D
    Leaf: B
  Leaf: C
```


Klasifikace:

```
$ cat test.tree
Node: 1, 3.4
  Node: 1, 2.45
    Node: 1, 2.1
      Leaf: A
      Leaf: D
    Leaf: B
  Leaf: C
$ cat data.csv
1.0, 2.0
1.0, 2.7
1.0, 4.1
1.0, 2.2
$ ./flp-fun -1 test.tree data.csv
A
B
C
D
```



## Implementovaná rozšíření

Velké úsilí bylo investováno hlavně do parsingu vstupních souborů. Cílem byla snadná možné změna/rozšíření formátů vstupních souborů (viz `Parser`, `CommonParser`, `DecisionTreeParser` a `MDataParser`), zajištění detekce syntaktických a sémantických chyb doprovázené co nejpopisnějším chybovým hlášením (viz dále).

### Detekce chyb

Program detekuje chybné vstupy a vypíše na *standardní výstup* příslušné chybové hlášení. Při nalezení chyby je zároveň vracen nenulový návratový kód. Detekce chyb se týká jak chybných argumentů programu, tak chyb ve vstupních souborech (stromy i CSV formát).

Pokud například spustíme program s chybějícími argumenty přepínače `-2`:

```
$ ./flp-fun -1 test.tree
```

Získáme na výstupu:

```
Usage error: -1: missing path to file with data! (see --help)
```


Pokud jsou chyby ve vstupních souborech:

```
$ cat test.tree 
Node: 0, 5
  Node: TridaA
  Node: 1, 2
    Leaf: TridaB
    Leaf: TridaC

$ ./flp-fun -1 test.tree data.csv
```
```
Tree parser: Syntax error at (1,7): Got "TridaA", expected one of ["digit"]
```

Nebo:

```
$ cat data.csv 
1.0, 2.0, 4.1
1.0, 2.0, 4.1
1.0, 4.1
1.0, 2.0, 4.1

$ ./flp-fun -1 test_correct.tree data.csv
```
```
Data parser: Syntax error at (2,1): Got "" expected one of [","]
```


### Nápověda při chybách ve vstupních souborech

Jak je možné si všimnout výše, součástí chybových hlášení je také přibližná pozice chyby a názvy očekávaných tokenů, aby byla uživateli maximálně usnadněna oprava chyby:

```
$ cat test.tree 
Node: 0, 5
  Leaf: TridaA
  Node: 1, 2
    Error Leaf: TridaB
    Leaf: TridaC

$ ./flp-fun -1 test.tree data.csv
```
```
Tree parser: Syntax error at (3,4): Got "Error Leaf: TridaB", expected one of ["Leaf","Node"]
```

### Tolerance non-ASCII znaků

Rovněž byla testována také tolerance programu vůči diakritice ve vstupních souborech. Ta se však ukázala jako velmi závislá také na daném testovacím prostředí (pravděpodobně na jeho lokalizaci). Např.:

```
$ cat test.tree
Node: 0, 5
  Leaf: TřídaA
  Node: 1, 2
    Leaf: TřídaB
    Leaf: TřídaC

$ cat data.csv
1, 1
8, 10

```

Výstup na studentském serveru *Eva*:
```
eva ~/FLP> ./flp-fun -1 test.tree data.csv
TřídaA
TřídaC

```

Výstup na studentském serveru *Merlin* (skončí chybou na `hGetContents`):
```
~/FLP$ ./flp-fun -1 test.tree data.csv
flp-fun: test.tree: hGetContents: invalid argument (invalid byte sequence)
```

