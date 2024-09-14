package main

import (
	"net/http"
)

func main() {
	mux := http.NewServeMux()
	mux.Handle("GET /get", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("OK"))
	}))

	mux.Handle("POST /post", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("OK"))
	}))
	http.ListenAndServe(":8899", mux)
}
