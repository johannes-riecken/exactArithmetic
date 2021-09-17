package main

import (
	"exactArithmetic/parse"
	"exactArithmetic/scan"
	"fmt"
)

//func Run(p *parse.Parser) (success bool) {
//	exprs, ok := p.Line()
//}
func main() {
	p := &parse.Parser{
		Tokens: []scan.Token{
			{
				Type: scan.Number,
				Text: "42",
			},
			{
				Type: scan.Op,
				Text: "+",
			},
			{
				Type: scan.Number,
				Text: "9000",
			},
		},
	}
	exprs, ok := p.Line()
	if ok {
		fmt.Println(exprs)
	}
}