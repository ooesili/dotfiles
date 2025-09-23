package main

import (
	"context"
	"fmt"
	"os"
)

func main() {
	if err := mainErr(context.Background()); err != nil {
		fmt.Fprintf(os.Stderr, "error: %s\n", err)
		os.Exit(1)
	}
}

func mainErr(ctx context.Context) error {
	return nil
}
