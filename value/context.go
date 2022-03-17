package value

import "strconv"

type Expr interface {

}

type Int int64

func SetIntString(s string) (Int, error) {
	inputBase := 10
	intBits := 64
	i, err := strconv.ParseInt(s, inputBase, intBits)
	return Int(i), err
}

