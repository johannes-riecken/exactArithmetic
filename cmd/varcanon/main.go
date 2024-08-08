package main

import (
	"io"
	"log"
	"os"

	"exactArithmetic/varcanon"
)

func main() {
	if err := mainAux(); err != nil {
		log.Fatal(err)
	}
}

func mainAux() error {
	fh := os.Stdin
	if len(os.Args) > 1 {
		var err error
		fh, err = os.Open(os.Args[1])
		if err != nil {
			return err
		}
		defer fh.Close()
	}
	src, err := io.ReadAll(fh)
	if err != nil {
		return err
	}

	newSrc, err := varcanon.ReplaceVariableNames(string(src))
	if err != nil {
		return err
	}

	println(newSrc)
	return nil
}
