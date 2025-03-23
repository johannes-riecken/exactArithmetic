package main

type S struct {
	w, h int
}

func foo() *S {
	w := 1
	h := 2
	return &S{w: w, h: h}
}
