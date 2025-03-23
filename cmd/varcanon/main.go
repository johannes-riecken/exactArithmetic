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
	src, err := io.ReadAll(os.Stdin)
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
