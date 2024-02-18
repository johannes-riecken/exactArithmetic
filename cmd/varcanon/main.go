package main

import (
	"bytes"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"log"
)

var (
	syms     = []string{"x", "y", "z"}
	iSyms    = 0
	arrSyms  = []string{"a", "b", "c"}
	iArrSyms = 0
)

func genSym(n ast.Node) string {
	_, isArray := n.(*ast.ArrayType)
	if isArray {
		return genArrSym()
	}
	iSyms++
	return syms[iSyms-1]
}

func genArrSym() string {
	iArrSyms++
	return arrSyms[iArrSyms-1]
}

func replaceVariableNames(src string) (string, error) {
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, "", src, parser.ParseComments)
	if err != nil {
		return "", err
	}

	// Create a map to keep track of variable declarations
	varDecls := make(map[string]bool)

	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			replaceInFuncDecl(x, varDecls)
		case *ast.GenDecl:
			replaceInGenDecl(x, varDecls)
		case *ast.AssignStmt:
			replaceInAssignStmt(x, varDecls)
		}
		return true
	})

	var buf bytes.Buffer
	if err := format.Node(&buf, fset, node); err != nil {
		return "", err
	}

	return buf.String(), nil
}

func replaceInAssignStmt(x *ast.AssignStmt, varDecls map[string]bool) {
	for _, lh := range x.Lhs {
		if ident, ok := lh.(*ast.Ident); ok {
			if !varDecls[ident.Name] {
				if cl, ok := x.Rhs[0].(*ast.CompositeLit); ok && len(x.Rhs) == 1 {
					ident.Name = genSym(cl.Type)
					varDecls[ident.Name] = true
				}
			}
		}
	}
}

func replaceInGenDecl(x *ast.GenDecl, varDecls map[string]bool) {
	if x.Tok == token.VAR {
		for _, spec := range x.Specs {
			if valueSpec, ok := spec.(*ast.ValueSpec); ok {
				for _, name := range valueSpec.Names {
					name.Name = genSym(valueSpec.Type)
					varDecls[name.Name] = true
				}
			}
		}
	}
}

func replaceInFuncDecl(x *ast.FuncDecl, varDecls map[string]bool) {
	// Handle function parameters
	for _, field := range x.Type.Params.List {
		for _, name := range field.Names {
			name.Name = genSym(field.Type)
			varDecls[name.Name] = true
		}
	}
}

func main() {
	if err := mainAux(); err != nil {
		log.Fatal(err)
	}
}

func mainAux() error {
	src := `
	package main

	func exampleFunc(foo int, bar []int, baz int, quux []int) {
		var foobar int
		foobar, foo, quux = len(bar), len(quux), []int{foo, baz}
	}
	`
	newSrc, err := replaceVariableNames(src)
	if err != nil {
		return err
	}
	println(newSrc)
	return nil
}
