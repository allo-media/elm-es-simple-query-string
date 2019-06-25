# Elm simple elastic query

This package allow to parse an [elastic simple query string](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html#_simple_query_string_syntax) into an AST.

`~N` tokenizer is not supported.

At this time, the default operator for elastic must be set to "AND" in elastic search services.

## Alphabet

* ( ) Group
* [WORD] Char+
* | OR
* \+ AND
* -- Exclude
* "" Exact
* \* prefix search
* Specific case : Spaces (signifying in some contexts and not in others)

## AST

```elm
type AST
  = And AST AST
  | Or AST AST
  | Exclude AST
  | Exact String
  | Word String
  | Prefix String
```

In our case, to follow the production rules `AST` will be `Expr`

```elm
type Expr
    = And Expr Expr
    | Exact String
    | Exclude Expr
    | Or Expr Expr
    | Prefix String
    | Word String
```

## Production rules

Unless explicitly indicated, spaces are ignored.

```
Query => ORExpr EOF
ORExpr => ANDExpr | ANDExpr "|" ORExpr
ANDExpr => EXCExpr | EXCExpr ("+"|\s+) ANDExpr
EXCExpr => "-" GRPExpr | GRPExpr
GRPExpr => WORD~"\*" | WORD | \" EXACTExpr \" | "(" ORExpr ")"
EXACTExpr => [^"]+
```


## Example of simple query String

big* (potatoes|\"french fries\") -salad
