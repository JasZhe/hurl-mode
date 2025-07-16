package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"strconv"
	"time"
)

type User struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
	Email string `json:"email"`
}

var users = []User{
	{ID: 1, Name: "John Doe", Email: "john@example.com"},
	{ID: 2, Name: "Jane Smith", Email: "jane@example.com"},
}

func main() {
	// Basic endpoints
	http.HandleFunc("/", homeHandler)
	http.HandleFunc("/health", healthHandler)
	http.HandleFunc("/echo", echoHandler)
	
	// REST API endpoints
	http.HandleFunc("/api/users", usersHandler)
	http.HandleFunc("/api/users/", userHandler)
	
	// Test-specific endpoints
	http.HandleFunc("/delay/", delayHandler)
	http.HandleFunc("/status/", statusHandler)
	http.HandleFunc("/headers", headersHandler)
	
	port := ":8080"
	fmt.Printf("Test server starting on http://localhost%s\n", port)
	log.Fatal(http.ListenAndServe(port, nil))
}

func homeHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{
		"message": "Hurl test server",
		"version": "1.0.0",
	})
}

func healthHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{
		"status": "ok",
		"timestamp": time.Now().Format(time.RFC3339),
	})
}

func echoHandler(w http.ResponseWriter, r *http.Request) {
	body, _ := io.ReadAll(r.Body)
	
	response := map[string]interface{}{
		"method": r.Method,
		"headers": r.Header,
		"body": string(body),
		"query": r.URL.Query(),
	}
	
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

func usersHandler(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "GET":
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(users)
	case "POST":
		var user User
		if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
			http.Error(w, "Invalid JSON", http.StatusBadRequest)
			return
		}
		user.ID = len(users) + 1
		users = append(users, user)
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusCreated)
		json.NewEncoder(w).Encode(user)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

func userHandler(w http.ResponseWriter, r *http.Request) {
	idStr := r.URL.Path[len("/api/users/"):]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid user ID", http.StatusBadRequest)
		return
	}
	
	for i, user := range users {
		if user.ID == id {
			switch r.Method {
			case "GET":
				w.Header().Set("Content-Type", "application/json")
				json.NewEncoder(w).Encode(user)
				return
			case "DELETE":
				users = append(users[:i], users[i+1:]...)
				w.WriteHeader(http.StatusNoContent)
				return
			}
		}
	}
	
	http.Error(w, "User not found", http.StatusNotFound)
}

func delayHandler(w http.ResponseWriter, r *http.Request) {
	secondsStr := r.URL.Path[len("/delay/"):]
	seconds, err := strconv.Atoi(secondsStr)
	if err != nil || seconds < 0 || seconds > 10 {
		http.Error(w, "Invalid delay (0-10 seconds)", http.StatusBadRequest)
		return
	}
	
	time.Sleep(time.Duration(seconds) * time.Second)
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"delayed": seconds,
		"message": fmt.Sprintf("Delayed for %d seconds", seconds),
	})
}

func statusHandler(w http.ResponseWriter, r *http.Request) {
	statusStr := r.URL.Path[len("/status/"):]
	status, err := strconv.Atoi(statusStr)
	if err != nil || status < 100 || status > 599 {
		http.Error(w, "Invalid status code", http.StatusBadRequest)
		return
	}
	
	w.WriteHeader(status)
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"status": status,
		"message": http.StatusText(status),
	})
}

func headersHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("X-Custom-Header", "test-value")
	w.Header().Set("X-Request-ID", "12345")
	
	json.NewEncoder(w).Encode(map[string]interface{}{
		"received_headers": r.Header,
		"method": r.Method,
	})
}
