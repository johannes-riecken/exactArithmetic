package varcanon_test

import (
	"io"
	"log"
	"os"
	"testing"

	"exactArithmetic/varcanon"
)

func TestVarCanon(t *testing.T) {
	f, err := os.Open("testdata/in00.go")
	if err != nil {
		t.Fatal(err)
	}

	src, err := io.ReadAll(f)
	if err != nil {
		t.Fatal(err)
	}

	newSrc, err := varcanon.ReplaceVariableNames(string(src))
	if err != nil {
		t.Fatal(err)
	}

	want := `package main

func exampleFunc(x int, a []int, y int, b []int) {
	var z int
	_ = z
	z, x, b = len(a), len(b), []int{x, y}
}
`
	if newSrc != want {
		t.Fatalf("%q != %q", newSrc, want)
	}
}


func main() {
	err := mainAux()
	if err != nil {
		log.Fatal(err)
	}
}

func mainAux() error {
	return nil
}

