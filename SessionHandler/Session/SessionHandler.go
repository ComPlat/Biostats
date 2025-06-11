package main

import(
	"fmt"
	"os"
	"strconv"
	"os/exec"
	"time"
)

type session struct {
	port int
	cmd *exec.Cmd
	error_chan chan error
}

func start_process(port int) (*session, error) {
	waitDone := make(chan error)
	cmd := exec.Command("Rscript", "app.R", strconv.Itoa(port))
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err := cmd.Start()
	if err != nil {
		return &session{port, cmd, waitDone}, err
	}
	go func() {
		waitDone <- cmd.Wait()
	}()
	return &session{port, cmd, waitDone}, nil
}

func kill_process(s *session) {
	s.cmd.Process.Kill()
}

func main() {
	var sessions[] session;
	port := 3840
	s, err := start_process(port)
	if err != nil {
		fmt.Println("Shiny app couldn't be started due to:", err)
	}
	sessions = append(sessions, *s)

	time.Sleep(30 * time.Second)

	kill_process(&sessions[0])

	// Handle exit
	err = <- sessions[0].error_chan
	if err != nil {
		fmt.Println("Shiny app exited with error:", err)
	}

}
