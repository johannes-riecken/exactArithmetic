package main

import (
	"fmt"
	"golang.org/x/mobile/exp/sprite/clock"
	"image"
	"image/png"
	_ "image/png"
	"log"
	"os"
	"time"
)

//func Run(p *parse.Parser) (success bool) {
//	exprs, ok := p.Line()
//}

func mainAux() error {
	f, err := os.Open("/Users/rieckenj/Pictures/00.png")
	if err != nil {
		return err
	}
	defer f.Close()
	m, _, err := image.Decode(f)
	if err != nil {
		return err
	}
	out, err := os.Create("/Users/rieckenj/Pictures/00_out.png")
	if err != nil {
		return err
	}
	bounds := m.Bounds()
	img := image.NewRGBA64(bounds)
	for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
		for x := bounds.Min.X; x < bounds.Max.X; x++ {
			f := clock.CubicBezier(0.25, 0.75, 0.75, 0.75)
			if y == 0 {
				fmt.Println((float32(f(clock.Time(bounds.Min.X), clock.Time(bounds.Max.X), clock.Time(x)))))
			}
			refColor := m.At(int(float32(bounds.Max.X) * float32(f(clock.Time(bounds.Min.X), clock.Time(bounds.Max.X), clock.Time(x)))), y)
			img.Set(x, y, refColor)
		}
	}
	png.Encode(out, img)

	//src := strings.NewReader(`foo = "bar" | "baz" .`)
	//g, err := ebnf.Parse("foo.go", src)
	//if err != nil {
	//	return err
	//}
	//spew.Dump(g)
	return nil
}

// bouncingOctagon calculates the coordinates of a bouncing octagon at time t.
func bouncingOctagon(coords []image.Point, t time.Duration) (newCoords []image.Point) {
	// set initial height to 10.
	height := 10.0
	// set initial velocity to 0.
	velocity := 0.0
	// set initial acceleration to 0.
	acceleration := 0.0
	// set gravity to gravity on earth.
	gravity := 9.81
	// set friction to 0.5.
	friction := 0.5
	// set time step to 1/60.
	timeStep := time.Second / 60
	// set time to 0.
	time := 0.0
	// calculate height after t nanoseconds.
	for time < float64(t) {
        // calculate new velocity.
        velocity = velocity + (acceleration*timeStep.Seconds())
        // calculate new height.
        height = height + (velocity*timeStep.Seconds())
        // calculate new acceleration.
        acceleration = -(gravity*height) - (friction*velocity)
        // calculate new time.
        time = time + timeStep.Seconds()
    }
	// calculate new coordinates.

	return newCoords
}

func main() {
	err := mainAux()
	if err != nil {
		log.Fatal(err)
	}
	//p := &parse.Parser{
	//	Tokens: []scan.Token{
	//		{
	//			Type: scan.Number,
	//			Text: "42",
	//		},
	//		{
	//			Type: scan.Op,
	//			Text: "+",
	//		},
	//		{
	//			Type: scan.Number,
	//			Text: "9000",
	//		},
	//	},
	//}
	//exprs, ok := p.Line()
	//if ok {
	//	fmt.Printf("%T: %v\n", exprs[0], exprs[0])
	//}
	//fmt.Println(5 ^= true)

	//fset := token.NewFileSet()
	//src := "package main\n" +
	//	"func main() {\n  }"
	//f, err := parser.ParseFile(fset, "", src, 0)
	//if err != nil {
	//	log.Fatal(err)
	//}
	//printer.Fprint(os.Stdout, fset, f)

	/*
	Execution proceeds from right to left, except that when a right parenthesis is encountered, the segment enclosed by it and its matching left parenthesis is executed, and its result replaces the entire segment and its enclosing parentheses.
	2. 	Adverbs and conjunctions are executed before verbs; the phrase ,"2-a is equivalent to (,"2)-a , not to ,"(2-a) . Moreover, the left argument of an adverb or conjunction is the entire verb phrase that precedes it.
	*/
	// Thus, in the phrase +/ . */b , the rightmost adverb / applies to the verb derived from the phrase +/ . * , not to the verb * .
	/*
	3. 	A verb is applied dyadically if possible; that is, if preceded by a noun that is not itself the right argument of a conjunction.
	4. 	Certain trains form verbs and adverbs, as described in § F.
	5. 	To ensure that these summary parsing rules agree with the precise parsing rules prescribed below, it may be necessary to parenthesize an adverbial or conjunctival phrase that produces anything other than a noun or verb.
	*/
}