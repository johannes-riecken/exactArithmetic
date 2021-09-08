package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"golang.org/x/tools/go/ast/astutil"
	"log"
	"math"
)

func mainAux() error {
	fset := token.NewFileSet() // positions are relative to fset
	src := `package main

import "fmt"

type Int struct {
        Value int
}

func (x Int) String() string {
	return fmt.Sprintf("%v", x.Value)
}

func (x Int) ShowsPrec(d int) string {
	return fmt.Sprintf("%v", x.Value)
}

type Expr interface {
	Mult(y Int) Expr
	ShowsPrec(d int) string
}

type BinExpr struct {
	Op string
	X Expr
	Y Expr
}

var precs = map[string]int {
	"*": 7,
	"+": 6,
}

func parenthesize(b bool, s string) string {
	if b {
		return "(" + s + ")"
	}
	return s
}

func (x BinExpr) ShowsPrec(d int) string {
	if precs[x.Op] == 0 {
		panic("invalid precendence")
	}
	return parenthesize(d > precs[x.Op], fmt.Sprintf("%v %v %v", x.X.ShowsPrec(precs[x.Op] + 1), x.Op, x.Y.ShowsPrec(precs[x.Op] + 1)))
}

func (x BinExpr) Mult(y Int) Expr {
	return BinExpr{Op: "*", X: x, Y: y}
}

func (x BinExpr) String() string {
	return fmt.Sprintf("(%v %v %v)", x.X, x.Op, x.Y)
}

func (x Int) Add(y Int) Expr {
	return BinExpr{Op: "+", X: x, Y: y}
}

func (x Int) Mult(y Int) Expr {
	return BinExpr{Op: "*", X: x, Y: y}
}

func main() {
	fmt.Println(42 + 5)
}`

	f, err := parser.ParseFile(fset, "src.go", src, parser.ParseComments)
	if err != nil {
		return err
	}
	astutil.Apply(f, pre, post)
	var buf bytes.Buffer
	if err := format.Node(&buf, fset, f); err != nil {
		return err
	}
	fmt.Printf("%s", buf.Bytes())
	return nil
}

func valspec(name, typ string) *ast.ValueSpec {
	return &ast.ValueSpec{Names: []*ast.Ident{ast.NewIdent(name)},
		Type: ast.NewIdent(typ),
	}
}

func main() {
	if err := mainAux(); err != nil {
		log.Fatal(err)
	}
	_ = math.Pow(1 + math.Sqrt(3), 2)
}

func post(c *astutil.Cursor) bool {
	if v, ok := c.Node().(*ast.BinaryExpr); ok {
		if _, ok := v.X.(*ast.CompositeLit); !ok {
			return true
		}
		if _, ok := v.Y.(*ast.CompositeLit); !ok {
			return true
		}
		c.Replace(&ast.CallExpr{
			Fun:      &ast.SelectorExpr{
				X:   v.X,
				Sel: &ast.Ident{Name: "Add"},
			},
			Args:     []ast.Expr{v.Y},
		})
		return true
		_ = v
	}
	return true
}

func pre(c *astutil.Cursor) bool {
	if v, ok := c.Node().(*ast.BasicLit); ok && v.Kind == token.INT {
		c.Replace(&ast.CompositeLit{Type: &ast.Ident{Name: "Int"}, Elts: []ast.Expr{&ast.KeyValueExpr{Key: &ast.Ident{Name: "Value"}, Value: v}}})
	}
	return true
}
