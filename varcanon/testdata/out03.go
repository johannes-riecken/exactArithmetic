package main

type S struct {
	w, h int
}

func foo() *S {
	x := 1
	y := 2
	return &S{w: x, h: y}
}
