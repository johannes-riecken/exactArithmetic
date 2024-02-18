package main

import (
	"bytes"
	"go/ast"
	"go/format"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"log"
)

var (
	syms        = []string{"x", "y", "z"}
	iSyms       = 0
	arrSyms     = []string{"a", "b", "c"}
	iArrSyms    = 0
	varMappings = make(map[string]string)
)

func genSym(n ast.Node, name string) string {
	val, ok := varMappings[name]
	if ok {
		return val
	}
	_, isArray := n.(*ast.ArrayType)
	var ret string
	if isArray {
		ret = genArrSym()
	} else {
		iSyms++
		ret = syms[iSyms-1]
	}
	varMappings[name] = ret
	return ret
}

func genArrSym() string {
	iArrSyms++
	return arrSyms[iArrSyms-1]
}

func replaceVariableNames2(src string) (string, error) {
	// Step 1: Parse the Go code
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, "", src, parser.AllErrors)
	if err != nil {
		return "", err
	}

	// Step 2: Type Check
	conf := types.Config{Importer: importer.Default()}
	info := &types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
	}
	_, err = conf.Check("main", fset, []*ast.File{node}, info)
	if err != nil {
		return "", err
	}

	// Step 3: Traverse and Rename
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.Ident:
			if tv, ok := info.Types[x]; ok && x.Obj != nil && x.Obj.Kind == ast.Var {
				// Logic to generate new variable name based on type
				// For simplicity, just appending type info to the name
				typeName := tv.Type.String()
				x.Name = genSym2(x.Name, typeName == "[]int")
			}
		}
		return true
	})

	var buf bytes.Buffer
	if err := format.Node(&buf, fset, node); err != nil {
		return "", err
	}

	return buf.String(), nil
}

func genSym2(name string, isArray bool) string {
	var typ ast.Node
	if isArray {
		typ = &ast.ArrayType{}
	} else {
		typ = &ast.FuncType{} // wrong, but who cares
	}
	return genSym(typ, name)
}

func replaceVariableNames(src string) (string, error) {
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, "", src, parser.ParseComments)
	if err != nil {
		return "", err
	}

	// Create a map to keep track of variable declarations

	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			replaceInFuncDecl(x)
		case *ast.GenDecl:
			replaceInGenDecl(x)
		case *ast.AssignStmt:
			replaceInAssignStmt(x)
		}
		return true
	})

	var buf bytes.Buffer
	if err := format.Node(&buf, fset, node); err != nil {
		return "", err
	}

	return buf.String(), nil
}

func replaceInAssignStmt(x *ast.AssignStmt) {
	for _, lh := range x.Lhs {
		if ident, ok := lh.(*ast.Ident); ok {
			if cl, ok := x.Rhs[0].(*ast.CompositeLit); ok && len(x.Rhs) == 1 {
				ident.Name = genSym(cl.Type, ident.Name)
			}
		}
	}
}

func replaceInGenDecl(x *ast.GenDecl) {
	if x.Tok == token.VAR {
		for _, spec := range x.Specs {
			if valueSpec, ok := spec.(*ast.ValueSpec); ok {
				for _, name := range valueSpec.Names {
					name.Name = genSym(valueSpec.Type, name.Name)
				}
			}
		}
	}
}

func replaceInFuncDecl(x *ast.FuncDecl) {
	// Handle function parameters
	for _, field := range x.Type.Params.List {
		for _, name := range field.Names {
			name.Name = genSym(field.Type, name.Name)
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
	_ = foobar
	foobar, foo, quux = len(bar), len(quux), []int{foo, baz}
}
`
	newSrc, err := replaceVariableNames2(src)
	if err != nil {
		return err
	}
	println(newSrc)
	return nil
}
