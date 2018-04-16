[![Build Status](https://travis-ci.org/zacharygraziano/uno.svg?branch=master)](https://travis-ci.org/zacharygraziano/uno)
[![codecov](https://codecov.io/gh/zacharygraziano/uno/branch/master/graph/badge.svg)](https://codecov.io/gh/zacharygraziano/uno)

# Uno

This is a simulation of the game UNO.

## Using it.

Make a `Uno` and call `run`:
```scala
val game = new Uno(numPlayers = 3)
val results = game.run
```

`run` is a `Stream[GameState]` which contains all changes to the game's state from start to finish. 

## Shortcomings

+ The default strategies are not very sophisticated.
+ The entire rule about a player yelling "UNO" is not taken into account.
+ No support for custom rules, e.g. "Progressive" Uno (where draw cards and skips can be "chained")

## License

The UNO game, its trademark, and brand is the property of Mattel, Inc. 
This code is licensed under the MIT License.

