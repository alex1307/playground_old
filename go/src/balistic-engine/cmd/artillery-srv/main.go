package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"net"
	"time"

	api "balistic-engine/grpc/api"
	"balistic-engine/pkg/artillery"
	"balistic-engine/pkg/math"

	"github.com/google/uuid"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"
)

var (
	port = flag.Int("port", 50051, "The server port")
)

// // server is used to implement helloworld.GreeterServer.
// type server struct {
// 	pb.UnimplementedBalisticServiceServer
// }

// // SayHello implements helloworld.GreeterServer
// func (s *server) Calculate(ctx context.Context, in *pb.BalisticRequest) (*pb.BalisticResponse, error) {
// 	log.Printf("Received: %v", in.GetAngle())
// 	return &pb.BalisticResponse{MaxDistance: 100, MaxHeight: 10, Time: 1}, nil
// }

// func (s *server) Hello(ctx context.Context, in *pb.Empty) (*pb.HelloResponse, error) {
// 	log.Printf("123")
// 	return &pb.HelloResponse{MyMessage: "oopps"}, nil
// }

type grpcServer struct {
	api.UnimplementedArtilleryServiceServer
	artillery artillery.Artillery
}

func NewArtilleryGrpcServer(artillery artillery.Artillery) grpcServer {
	return grpcServer{
		artillery: artillery,
	}
}

func (g *grpcServer) CreateSystem(ctx context.Context, in *api.Request) (*api.MissleSystem, error) {

	return &api.MissleSystem{
		Name: in.Name,
	}, nil
}

func (g *grpcServer) AddLauncher(ctx context.Context, in *api.LauncherRequest) (*api.MissleSystem, error) {
	ms := g.artillery.AddLauncherToMissleSystem(&artillery.Launcher{
		ID: uuid.NewString(),
		Coordinates: math.Coordinates{
			X: float64(in.Coordinate.X),
			Y: float64(in.Coordinate.Y),
		},
	})
	var launchers []*api.Launcher
	for _, launcher := range ms.Launcher {
		launchers = append(launchers, &api.Launcher{
			Id:         launcher.ID,
			SystemName: in.SystemName,
			Coordinate: &api.Coordinates{
				X: float32(launcher.Coordinates.X),
				Y: float32(launcher.Coordinates.Y),
			},
		})
	}

	return &api.MissleSystem{
		Name:      ms.Name,
		Launchers: launchers,
	}, nil
}

func (g *grpcServer) LoadLauncher(ctx context.Context, in *api.LoadRequest) (*api.LauncherList, error) {
	var shootingConfig []artillery.ShootingConfiguration
	if in.Config != nil {
		for _, config := range in.Config {
			radians, _ := math.DegreesToRadians(float64(config.Degrees))
			shootingConfig = append(shootingConfig, artillery.ShootingConfiguration{
				Radians:  radians,
				Velocity: float64(config.Velocity),
			})
		}
	}
	g.artillery.LoadLauncher(*in.LauncherId, int(in.NumberOfMissles), shootingConfig)
	filtered := g.artillery.Filter(*in.LauncherId)
	var config []*api.ShootingConfig
	response := &api.LauncherList{}
	for _, launcher := range filtered {
		for _, conf := range launcher.Configs {
			degrees, _ := math.RadiansToDegrees(conf.Radians)
			config = append(config, &api.ShootingConfig{
				Degrees:  int32(degrees),
				Velocity: int32(conf.Velocity),
			})
		}

		json := api.Launcher{
			Id:         launcher.ID,
			SystemName: in.SystemName,
			Coordinate: &api.Coordinates{
				X: float32(launcher.Coordinates.X),
				Y: float32(launcher.Coordinates.Y),
			},
			NumberOfMissles: int32(launcher.AvailableAmmunitions()),
			Config:          config,
		}
		response.Launchers = append(response.Launchers, &json)

	}

	return response, nil
}

func (g *grpcServer) GetSystem(ctx context.Context, in *api.Empty) (*api.MissleSystem, error) {
	return &api.MissleSystem{}, nil
}

func (g *grpcServer) Execute(in *api.Action, stream api.ArtilleryService_ExecuteServer) error {
	for i := 0; i < 10; i++ {
		stream.Send(&api.Projectile{
			System: "test",
			Id:     uuid.NewString(),
			Coordinates: &api.Coordinates{
				X: float32(i),
				Y: float32(i),
			},
		})
		time.Sleep(1 * time.Second)
	}
	return nil
}

func main() {
	flag.Parse()
	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", *port))
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	grpc_artillery := artillery.NewMissleSystem("Ukraine")
	grpc_srv := NewArtilleryGrpcServer(grpc_artillery)
	// pb.RegisterBalisticServiceServer(s, &server{})
	api.RegisterArtilleryServiceServer(s, &grpc_srv)
	reflection.Register(s)
	log.Printf("server listening at %v", lis.Addr())
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
