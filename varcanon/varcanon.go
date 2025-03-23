package varcanon

import (
	"bytes"
	"go/ast"
	"go/format"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"strings"
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
	var parentStack []ast.Node
	ast.Inspect(node, func(n ast.Node) bool {
		if n == nil {
			if len(parentStack) > 0 {
				parentStack = parentStack[:len(parentStack)-1]
			}
			return true
		}
		parentStack = append(parentStack, n)

		switch x := n.(type) {
		case *ast.FuncDecl:
			// Reset global variables when a new function is encountered
			resetGlobals()
		case *ast.KeyValueExpr:
			// Ensure key and value idents do not share the same pointer
			if identKey, ok := x.Key.(*ast.Ident); ok {
				x.Key = &ast.Ident{
					Name:    identKey.Name,
					NamePos: identKey.NamePos,
				}
			}
		case *ast.Ident:
			// Skip field names within structs or interfaces
			if isField(parentStack) {
				return true
			}

			if tv, ok := info.Types[x]; ok && x.Obj != nil && x.Obj.Kind == ast.Var {
				x.Name = genSym(x.Name, isCompoundType(tv))
			}
			if tv, ok := info.Defs[x]; ok && x.Obj != nil && x.Obj.Kind == ast.Var {
				typeName := tv.Type().String()
				x.Name = genSym(x.Name, strings.HasPrefix(typeName, "[]"))
			}
			if tv, ok := info.Uses[x]; ok && x.Obj != nil && x.Obj.Kind == ast.Var {
				typeName := tv.Type().String()
				x.Name = genSym(x.Name, strings.HasPrefix(typeName, "[]"))
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

// Helper function to determine if a node is a struct or interface field
func isField(parentStack []ast.Node) bool {
	for i := len(parentStack) - 1; i >= 0; i-- {
		switch parentStack[i].(type) {
		case *ast.Field:
			// Check if the parent of this field is a struct type or interface type
			if len(parentStack) > i-1 {
				switch parentStack[i-2].(type) {
				case *ast.StructType, *ast.InterfaceType:
					return true
				}

			}
		}
	}
	return false
}

func isCompoundType(tv types.TypeAndValue) bool {
	typeName := tv.Type.String()
	return !tv.IsBuiltin() && (strings.HasPrefix(typeName, "[]") || strings.HasPrefix(typeName, "map["))
}
