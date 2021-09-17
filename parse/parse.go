package parse

import (
	"exactArithmetic/scan"
	"exactArithmetic/value"
)

type Parser struct {
	Tokens []scan.Token
}

func (p *Parser) next() scan.Token {
	tok := p.peek()
	if tok.Type != scan.EOF {
		p.Tokens = p.Tokens[1:]
	}
	return tok
}

func (p *Parser) peek() scan.Token {
	if len(p.Tokens) == 0 {
		return scan.Token{Type: scan.EOF}
	}
	return p.Tokens[0]
}

func (p *Parser) number() value.Expr {
	return nil
}

// TOP rule
func (p *Parser) Line() ([]value.Expr, bool) {
	return []value.Expr{p.addExpr()}, true
}

// The boolean reports whether the expression is valid.
func (p *Parser) addExpr() value.Expr {
	expr := p.number()
	tok := p.next()
	return &binary{
		left: expr,
		op: tok.Text,
		right: p.addExpr(),
	}
}

type binary struct {
	op string
	left value.Expr
	right value.Expr
}