package scan

import (
	"log"
	"strings"
)

type Scanner struct {
	input string
	start int
	pos int
	tokens chan Token
}

type Type int

type Token struct {
	Type Type
	Text string
}

const (
	EOF Type = iota
	Number
	Op
)

func (l *Scanner) emit(t Type) {
	l.tokens <- Token{Type: t}
}

type stateFn func(*Scanner) stateFn

func (l *Scanner) backup() {
	l.pos--
}

func (l *Scanner) next() rune {
	c := l.input[l.pos]
	l.pos++
	return rune(c)
}

func (l *Scanner) accept(valid string) bool {
	if strings.ContainsRune(valid, l.next()) {
		return true
	}
	l.backup()
	return false
}

func (l *Scanner) acceptRun(valid string) {
	for l.accept(valid) {
	}
}

func (l *Scanner) peek() rune {
	return rune(l.input[l.pos])
}

func lexAny(l *Scanner) stateFn {
	if x := l.peek(); strings.ContainsRune("0123456789", x) {
		return lexNumber(l)
	}
	log.Fatal("unexpected input")
	return lexAny(l)
}

func lexNumber(l *Scanner) stateFn {
	p := l.pos
	l.acceptRun("0123456789")
	if l.pos > p {
		l.emit(Number)
	}
	return lexAny
}