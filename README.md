# FGO - Functional GO

Our team is go developers, so we decided to fantasize on the topic: 
> What if Go was a purely functional language?

For daily development in Go, many functional elements are missing: `Optional[T]`, slice operations like `Map` and `Flatten`, `Pair[L, R]` and so on.

Yes, it **go**es against Go Way =)

## Syntax

Basic operations:
```
+ - / * = > >= > => // arithmetic
print // for strings
printint // for integers
```

If-else
```
if (* condition *) {
    * if branch *
} else {
    * else branch *
}
```

Functions
```
func * name * (* parameters *) {
    * body *
}
```

## Code samples

Hello World:
```
print "Hello!"
```

Sum of numbers:
```
printint 1 + 2
```

Factorial:
```
func fact (x) {
    if (x < 1) {
        1
    } else {
        x * fact(x - 1)
    }
}

printint fact(4)
```

And how is the factorial implemented on Go:
```
func fact(x int) int {
	if x <= 1 {
		return 1
	}
	return x * fact(x - 1)
}

func main() {
	fmt.Println(fact(4))
}
```

## Language Features

We have implemented:

* [ ] Named variables (`let`)
* [x] Recursion
* [ ] Lazy evaluation
* [x] Functions
* [x] Closures
* [ ] Library functions: File IO
* [ ] Lists / Sequences
* [ ] Library functions: Lists/Sequences

## Team

Name | Role in the project
------------------|---------------------
Daniil Aksenov | Text tokenization and other things
Alena Vasileva | Token parsing and other things
