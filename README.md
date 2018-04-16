[![Build Status](https://travis-ci.org/zacharygraziano/uno.svg?branch=master)](https://travis-ci.org/zacharygraziano/uno)
[![codecov](https://codecov.io/gh/zacharygraziano/uno/branch/master/graph/badge.svg)](https://codecov.io/gh/zacharygraziano/uno)
 [ ![Download](https://api.bintray.com/packages/dougietech/dougietech/uno/images/download.svg) ](https://bintray.com/dougietech/dougietech/uno/_latestVersion)

# Uno

This is a simulation of the game UNO.

## Using it.

Make a `Uno` and call `run`:
```scala
val game = new Uno(numPlayers = 3)
val results = game.run
```

`run` is a `Stream[GameState]` which contains all changes to the game's state from start to finish.

It's also published to bintray as a library, get it in sbt with:
```
externalResolvers += Resolver.bintrayRepo("dougietech", "dougietech")
libraryDependencies += "tech.dougie" %% "uno" % "<current-version>"
```

The current version is at the top of this page.

## Example
There's an example of using it it [here](https://github.com/zacharygraziano/uno/blob/master/src/it/scala/tech/dougie/uno/Example.scala). You can run it with `sbt it/test`.

## Shortcomings

+ The default strategies are not very sophisticated.
+ The entire rule about a player yelling "UNO" is not taken into account.
+ No support for custom rules, e.g. "Progressive" Uno (where draw cards and skips can be "chained")

## License

The UNO game, its trademark, and brand is the property of Mattel, Inc. 
This code is licensed under the MIT License.

