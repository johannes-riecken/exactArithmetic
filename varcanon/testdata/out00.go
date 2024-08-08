package main

func exampleFunc(x int, a []int, y int, b []int) {
	var z int
	_ = z
	z, x, b = len(a), len(b), []int{x, y}
}
