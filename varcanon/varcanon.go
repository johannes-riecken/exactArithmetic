package varcanon

import (
	"bytes"
	"go/ast"
	"go/format"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
)

var (
	syms        = []string{"x", "y", "z", "w", "v", "s", "t", "u"}
	iSyms       = 0
	arrSyms     = []string{"a", "b", "c", "d", "e", "f", "g"}
	iArrSyms    = 0
	varMappings = make(map[string]string)
)

func genSym(name string, isArray bool) string {
	for _, v := range varMappings {
		if v == name {
			return name
		}
	}
	val, ok := varMappings[name]
	if ok {
		return val
	}
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

func resetGlobals() {
	syms = []string{"x", "y", "z", "w", "v", "s", "t", "u"}
	iSyms = 0
	arrSyms = []string{"a", "b", "c", "d", "e", "f", "g"}
	iArrSyms = 0
	varMappings = make(map[string]string)
}

func ReplaceVariableNames(src string) (string, error) {
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
		Defs:  make(map[*ast.Ident]types.Object),
		Uses:  make(map[*ast.Ident]types.Object),
	}
	_, err = conf.Check("main", fset, []*ast.File{node}, info)
	if err != nil {
		return "", err
	}

	// Step 3: Traverse and Rename
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			// Reset global variables when a new function is encountered
			resetGlobals()
		case *ast.Ident:
			if tv, ok := info.Types[x]; ok && x.Obj != nil && x.Obj.Kind == ast.Var {
				typeName := tv.Type.String()
				x.Name = genSym(x.Name, typeName == "[]int")
			}
			if tv, ok := info.Defs[x]; ok && x.Obj != nil && x.Obj.Kind == ast.Var {
				typeName := tv.Type().String()
				x.Name = genSym(x.Name, typeName == "[]int")
			}
			if tv, ok := info.Uses[x]; ok && x.Obj != nil && x.Obj.Kind == ast.Var {
				typeName := tv.Type().String()
				x.Name = genSym(x.Name, typeName == "[]int")
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
