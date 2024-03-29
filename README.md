# FLP - projekt 1

## Autor

Vojtěch Dvořák (xdvora3o)

## Popis řešení

Program `flp-fun` načte binární rozhodovací strom a neanotovaná data ve fromátu CSV a na standardní výstup vypíše na jednotlivých řádcích třídy daných objektů. Dále umožňuje natrénovat rozhodovací strom na základě anotované datové sady. Režim činnosti je vybírán přepínačem `-1` pro inferenci tříd, resp. `-2` pro trénování stromů. Pro více informací o argumentech programu je možné použít přepínače `-h`, `--help`.

Program disponuje všemi vlastnosti a veškerou funkcionalitou popsanou v zadání a pro větší přehlednost je rozdělen do několika modulů.

Ústředním modulem pro analýzu stromů a dat v CSV je modul `Parser`, v rámci kterého bylo vytvořeno obecné rámcové řešení pro definování gramatik popisující formáty vstupních souborů. `CommonParser`, `DecisionTreeParser` a `MDataParser` funkce z tohoto modulu využívají a obsahují funkce právě pro analýzu jednotlivých formátů.

Zodpovědnostmni modulů `Classifier` a `Trainer` je pak samotná klasifikace objektů (inference) a trénování nových binárních rozhodovaích stromů.


## Implementovaná rozšíření

### Detekce chyb a přehledná chybová hlášení s nápovědou

Program detekuje chybné vstupy a vypíše na standardní výstup příslušné chybové hlášení. To se týká jak chybných argumentů programu, tak chyb ve vstupních souborech. 




## Příklady

