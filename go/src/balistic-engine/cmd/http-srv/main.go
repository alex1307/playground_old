package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
)

func main() {
	// Hello world, the web server

	helloHandler := func(w http.ResponseWriter, req *http.Request) {
		var s = "<p style=\"color:blue;font-size:32px;\">Hello, world!!!!  🤌\n </p>"
		var s1 = "<p style=\"color:blue;font-size:16px;\">Hello, world!!!!  😀\n </p>"
		var bs = []byte(s)
		var rs = []rune(s)
		log.Println(bs)
		log.Println(rs)
		io.WriteString(w, s+s1)
	}

	http.HandleFunc("/hello", helloHandler)
	http.HandleFunc("/collections", collectionsHandler)
	http.HandleFunc("/struct", structHandler)
	http.HandleFunc("/slices", sliceHandler)
	log.Println("Listing for requests at http://localhost:8000/hello")
	log.Fatal(http.ListenAndServe(":8000", nil))
}

func collectionsHandler(w http.ResponseWriter, req *http.Request) {

	totalWins := map[string]int{"Arsenal": 1, "Chelsea": 2}
	totalWins["Everton"] = 0
	totalWins["Leeds"] = 2

	v, ok := totalWins["Man City"]
	if ok {
		fmt.Println("Man city: {}", v)
	} else {
		fmt.Println("Not found")
	}
	fmt.Println(totalWins["Arsenal"])
	totalWins["Chelsea"]++
	fmt.Println(totalWins["Kittens"])
	totalWins["Man Utd"] = 3

	var display = fmt.Sprintf("Collections: total wins: %v", totalWins)
	io.WriteString(w, display)
}

func structHandler(w http.ResponseWriter, req *http.Request) {
	type Person struct {
		Name string `json:"just_name"`
		Age  int    `json:"age"`
		Pet  string `json:"pet"`
	}

	var person = Person{Name: "John", Age: 30, Pet: "cat"}

	json, err := json.Marshal(person)
	if err != nil {
		io.WriteString(w, "error")
	}
	fmt.Println("json: ", json)
	io.WriteString(w, string(json))
}

func sliceHandler(w http.ResponseWriter, req *http.Request) {
	x := make([]int, 0, 5)
	x = append(x, 1, 2, 3, 4)
	y := x[:2:2]
	z := x[2:4:4]
	fmt.Println(cap(x), cap(y), cap(z))
	y = append(y, 30, 40, 50)
	x = append(x, 60)
	z = append(z, 70, 80, 90)
	io.WriteString(w, fmt.Sprint("x: {}", x))
	io.WriteString(w, fmt.Sprint("y: {}", y))
	io.WriteString(w, fmt.Sprint("z: {}", z))
}
