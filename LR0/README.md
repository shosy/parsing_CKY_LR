## LR(0)の実行例
### 入力
* 文脈自由文法
    * S' -> S $
    * S -> ( L )
    * S -> x
    * L -> S
    * L -> L * S
* 文字列
    * ( x * x ) $
### 実行結果
```
$ make
$ ./lr0
state:	S -> . ( L ),  S -> . x,  S' -> . S $
read:	(
action:	shift
state:	L -> . L * S,  L -> . S,  S -> . ( L ),  S -> ( . L ),  S -> . x
read:	x
action:	shift
state:	S -> x .
read:	*
action:	reduce by S -> x
state:	L -> S .
read:	*
action:	reduce by L -> S
state:	L -> L . * S,  S -> ( L . )
read:	*
action:	shift
state:	L -> L * . S,  S -> . ( L ),  S -> . x
read:	x
action:	shift
state:	S -> x .
read:	)
action:	reduce by S -> x
state:	L -> L * S .
read:	)
action:	reduce by L -> L * S
state:	L -> L . * S,  S -> ( L . )
read:	)
action:	shift
state:	S -> ( L ) .
read:	$
action:	reduce by S -> ( L )
state:	S' -> S . $
read:	$
action:	accept
(* parse tree *)
S
	(
	L
		L
			S
				x
		*
		S
			x
	)

```
