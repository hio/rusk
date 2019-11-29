---
title: "Rusk a Specification Language"
---

## Usage

```
rusk [options] file.rsk > output.md
```

read the file and output a markdown document.

### Command Line Options

* ``-V, --version``    -- show version.
* ``-h, --help``       -- show usage.
* ``--markdown``       -- generate markdown document. (default)
* ``--json``           -- generate json text.
* ``--json-pretty``    -- generate pretty json text.
* ``--title {title}``  -- set document title.


## Example

```
event greet@(do greeting);

state Greeting@(greeting)
{
	var greeted: Boolean = false;
	transition greet
		when greeted == false @[- not yet -]
		-->
	{
		post {
			target greeted;
			greeted' = true;
			state' = Bye;
		}@{-
			Hello!
		-}
	}
}

state Bye@(bye!)
{
	// ...
}
```

## Documents

* [./doc/syntax.md](./doc/syntax.md).
* [./doc/lib.md](./doc/lib.md).
* [./ChangeLog](./ChangeLog).
* [./TODO.md](./TODO.md).


## References

### KML

* 《日経Robo》ファナックが買収したロボベンチャー、ソフトの技術力で群を抜く
  仕様を厳密に記述する「形式手法」を実践、約2万行を記述
  https://tech.nikkeibp.co.jp/dm/atcl/mag/15/00140/00017/?P=2
* 協働ロボットCOROの開発における形式的仕様記述KMLの開発と適用
  https://www.slideshare.net/liferobotics/corokml
* KML文法を思い出す
  https://github.com/minekoa/til/blob/master/formalmethod/kml/example.md
* kmldoc
  https://github.com/minekoa/til/tree/master/formalmethod/kml/kmldoc


## License

MIT OR Apache-2.0
