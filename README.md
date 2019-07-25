# Solver for "Calculator - The Game"

A fun little game that deserves a solver. 

Call the `solution` function with: `solution [starting num] [goal num] [number of moves] [list of buttons] [portal]`

Operations have the following signature:

```ocaml
type operation =
    Add of int
  | Sub of int
  | Mul of int
  | Div of int
  | Square
  | Cube
  | Del
  | MulNeg
  | Append of int
  | Replace of string * string
  | Shift of int
  | Reverse
  | Sum
  | Inv10
  | Mirror
  | MetaInc of int
  | StoreSave
  | StoreUse
```

Portals are either `None` if not present or `Some (portal [portal entry] [portal exit])` where `entry < exit` and we start counting from the right (and from 0).

## Examples

#### Level 134
```ocaml
> solution 0 34 3 [Append 2; Append 3; MetaInc 1] None;;
[+]1 ➔ 3 ➔ 4
```

#### Level 155
```ocaml
> solution 9 3001 9 [Replace("39", "93"); StoreSave; StoreUse; Div 3; Replace("31", "00")] None;;
store (s) ➔ /3 ➔ store (u) ➔ store (s) ➔ store (u) ➔ 39=>93 ➔ 39=>93 ➔ /3 ➔ 31=>00
```

(store (s) is long press and store (u) is single press)

#### Level 168
```ocaml
> solution 26 99 6 [Sum; Inv10; Append 2] None;;
2 ➔ sum ➔ Inv10 ➔ 2 ➔ sum ➔ Inv10
```

#### Level 185
```ocaml
# solution 525 150 5 [Add 1; Append 6; Append 7; Div 2] (Some (portal 0 3));;
7 ➔ 6 ➔ /2 ➔ 6 ➔ +1
```

#### Level 199
```ocaml
# solution 3002 3507 6 [Append 7; Replace("3","5"); Inv10; Shift 1] (Some (portal 0 4));;
7 ➔ 3=>5 ➔ 7 ➔ shift> ➔ Inv10 ➔ 7
```
