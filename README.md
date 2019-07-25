# Elm simple elastic query

Parse and serialize [ElasticSearch](https://www.elastic.co/en) search query strings.

This package allows to parse an [elastic simple query string](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html#_simple_query_string_syntax)
into an [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree).

**Notes:**

  - `~N` operator is not supported.

[Demo](https://allo-media.github.io/elm-es-simple-query-string/)

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
