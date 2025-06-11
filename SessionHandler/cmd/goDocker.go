
package main

import (
	"context"
	"fmt"
	"time"

	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/client"
)

func main() {
	ctx := context.Background()

	cli, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		panic(err)
	}

	imageName := "rocker/shiny"

	// Pull the image
	reader, err := cli.ImagePull(ctx, imageName, types.ImagePullOptions{})
	if err != nil {
		panic(err)
	}
	defer reader.Close() // discard pull logs

	fmt.Println("Image pulled.")

	// Create the container
	resp, err := cli.ContainerCreate(ctx, &container.Config{
		Image: imageName,
	}, nil, nil, nil, "")
	if err != nil {
		panic(err)
	}

	fmt.Println("Container created:", resp.ID)

	// Start the container
	if err := cli.ContainerStart(ctx, resp.ID, types.ContainerStartOptions{}); err != nil {
		panic(err)
	}
	fmt.Println("Container started.")

	// Wait before stopping
	time.Sleep(10 * time.Second)

	// Stop the container
	timeout := 5 * time.Second
	err = cli.ContainerStop(ctx, resp.ID, container.StopOptions{Timeout: &timeout})
	if err != nil {
		panic(err)
	}
	fmt.Println("Container stopped.")

	// Remove the container
	err = cli.ContainerRemove(ctx, resp.ID, types.ContainerRemoveOptions{})
	if err != nil {
		panic(err)
	}
	fmt.Println("Container removed.")
}
