package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"net"

	pb "balistic-engine/pkg/balistic"

	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"
)

var (
	port = flag.Int("port", 50051, "The server port")
)

// server is used to implement helloworld.GreeterServer.
type server struct {
	pb.UnimplementedBalisticServiceServer
}

// SayHello implements helloworld.GreeterServer
func (s *server) Calculate(ctx context.Context, in *pb.BalisticRequest) (*pb.BalisticResponse, error) {
	log.Printf("Received: %v", in.GetAngle())
	return &pb.BalisticResponse{MaxDistance: 100, MaxHeight: 10, Time: 1}, nil
}

func (s *server) Hello(ctx context.Context, in *pb.Empty) (*pb.HelloResponse, error) {
	log.Printf("123")
	return &pb.HelloResponse{MyMessage: "oopps"}, nil
}

func main() {
	flag.Parse()
	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", *port))
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	pb.RegisterBalisticServiceServer(s, &server{})
	reflection.Register(s)
	log.Printf("server listening at %v", lis.Addr())
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
