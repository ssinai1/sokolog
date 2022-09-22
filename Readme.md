# Sokoban solver - warehouse work with prolog

This project is a by-product of learning prolog.

Tested with SWI-Prolog 8.4.2.

## Usage

In command line, run one of the following:

* For A* search:
```
$ swipl sokoban-astar.pl
```

* For tabled solver:
```
$ swipl sokoban-tabled.pl
```

Then run the query, such as
```
?- solution('levels/example-01.lvl', Plan, N). 
```

## Levels

Board symbols:
```
# - wall
@ - sokoban
$ - box
. - storage
+ - storage + sokoban
* - storage + box
```

You can find more levels in this format [here](http://sneezingtiger.com/sokoban/levels.html).