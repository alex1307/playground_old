// Code generated by protoc-gen-go-grpc. DO NOT EDIT.
// versions:
// - protoc-gen-go-grpc v1.2.0
// - protoc             v3.21.9
// source: resources/artillery.proto

package api

import (
	context "context"
	grpc "google.golang.org/grpc"
	codes "google.golang.org/grpc/codes"
	status "google.golang.org/grpc/status"
)

// This is a compile-time assertion to ensure that this generated file
// is compatible with the grpc package it is being compiled against.
// Requires gRPC-Go v1.32.0 or later.
const _ = grpc.SupportPackageIsVersion7

// ArtilleryServiceClient is the client API for ArtilleryService service.
//
// For semantics around ctx use and closing/ending streaming RPCs, please refer to https://pkg.go.dev/google.golang.org/grpc/?tab=doc#ClientConn.NewStream.
type ArtilleryServiceClient interface {
	ConfigureMissleSystem(ctx context.Context, in *ConfigurationRequest, opts ...grpc.CallOption) (*ConfigurationRequest, error)
}

type artilleryServiceClient struct {
	cc grpc.ClientConnInterface
}

func NewArtilleryServiceClient(cc grpc.ClientConnInterface) ArtilleryServiceClient {
	return &artilleryServiceClient{cc}
}

func (c *artilleryServiceClient) ConfigureMissleSystem(ctx context.Context, in *ConfigurationRequest, opts ...grpc.CallOption) (*ConfigurationRequest, error) {
	out := new(ConfigurationRequest)
	err := c.cc.Invoke(ctx, "/api.ArtilleryService/ConfigureMissleSystem", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// ArtilleryServiceServer is the server API for ArtilleryService service.
// All implementations must embed UnimplementedArtilleryServiceServer
// for forward compatibility
type ArtilleryServiceServer interface {
	ConfigureMissleSystem(context.Context, *ConfigurationRequest) (*ConfigurationRequest, error)
	mustEmbedUnimplementedArtilleryServiceServer()
}

// UnimplementedArtilleryServiceServer must be embedded to have forward compatible implementations.
type UnimplementedArtilleryServiceServer struct {
}

func (UnimplementedArtilleryServiceServer) ConfigureMissleSystem(context.Context, *ConfigurationRequest) (*ConfigurationRequest, error) {
	return nil, status.Errorf(codes.Unimplemented, "method ConfigureMissleSystem not implemented")
}
func (UnimplementedArtilleryServiceServer) mustEmbedUnimplementedArtilleryServiceServer() {}

// UnsafeArtilleryServiceServer may be embedded to opt out of forward compatibility for this service.
// Use of this interface is not recommended, as added methods to ArtilleryServiceServer will
// result in compilation errors.
type UnsafeArtilleryServiceServer interface {
	mustEmbedUnimplementedArtilleryServiceServer()
}

func RegisterArtilleryServiceServer(s grpc.ServiceRegistrar, srv ArtilleryServiceServer) {
	s.RegisterService(&ArtilleryService_ServiceDesc, srv)
}

func _ArtilleryService_ConfigureMissleSystem_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(ConfigurationRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(ArtilleryServiceServer).ConfigureMissleSystem(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/api.ArtilleryService/ConfigureMissleSystem",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(ArtilleryServiceServer).ConfigureMissleSystem(ctx, req.(*ConfigurationRequest))
	}
	return interceptor(ctx, in, info, handler)
}

// ArtilleryService_ServiceDesc is the grpc.ServiceDesc for ArtilleryService service.
// It's only intended for direct use with grpc.RegisterService,
// and not to be introspected or modified (even as a copy)
var ArtilleryService_ServiceDesc = grpc.ServiceDesc{
	ServiceName: "api.ArtilleryService",
	HandlerType: (*ArtilleryServiceServer)(nil),
	Methods: []grpc.MethodDesc{
		{
			MethodName: "ConfigureMissleSystem",
			Handler:    _ArtilleryService_ConfigureMissleSystem_Handler,
		},
	},
	Streams:  []grpc.StreamDesc{},
	Metadata: "resources/artillery.proto",
}
