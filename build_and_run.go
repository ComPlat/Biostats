package main

import (
    "os"
    "os/exec"
    "os/signal"
    "syscall"
)

func main() {
    signals := make(chan os.Signal, 1)
    signal.Notify(signals, os.Interrupt, syscall.SIGTERM)
    go func() {
        <-signals
        stopCmd := exec.Command("sudo", "systemctl", "stop", "shiny-server")
        stopCmd.Run()
        os.Exit(0)
    }()
    buildCmd := exec.Command("docker", "build", "-t", "my-shiny-app", ".")
    if err := buildCmd.Run(); err != nil {
        os.Exit(1) 
    }
    startCmd := exec.Command("sudo", "systemctl", "start", "shiny-server")
    if err := startCmd.Run(); err != nil {
        os.Exit(1) 
    }
    cmd := exec.Command("docker", "run", "--net=host", "--rm", "-p", "3838:3838", "my-shiny-app")
    if err := cmd.Start(); err != nil {
        os.Exit(1)
    }
    <-signals
}
