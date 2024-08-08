package varcanon_test

import (
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"testing"

	"exactArithmetic/varcanon"
)

func TestVarCanon(t *testing.T) {
	// loop over files matching testdata/in??.go
	// this requires importing "path/filepath"
	// for globbing we use "filepath.Glob"
	files, err := filepath.Glob("testdata/in??.go")
	if err != nil {
		t.Fatal(err)
	}
	for i, file := range files {
		f, err := os.Open(file)
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

		wantF, err := os.Open(fmt.Sprintf("testdata/out%02d.go", i))
		if err != nil {
			t.Fatal(err)
		}

		want, err := io.ReadAll(wantF)
		if err != nil {
			t.Fatal(err)
		}

		if newSrc != string(want) {
			t.Fatalf("%q != %q", newSrc, want)
		}
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
