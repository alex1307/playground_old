package test

import (
	"balistic-engine/pkg/artillery"
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"sync"
	"testing"

	"github.com/google/uuid"
	"github.com/stretchr/testify/assert"
)

func TestSendAndRecive(t *testing.T) {
	config.Setup()
	srv := message.NewServer()
	reciever1 := make(chan message.Payload, 10)
	reciever2 := make(chan message.Payload, 10)
	srv.Subscribe("r1", reciever1)
	srv.Subscribe("r2", reciever2)
	payload1 := &artillery.TrajectoryCoordinates{ID: uuid.NewString(), TeamID: "test", Coordinates: math.Coordinates{X: 1, Y: 1}}
	payload2 := &artillery.TrajectoryCoordinates{ID: uuid.NewString(), TeamID: "test", Coordinates: math.Coordinates{X: 2, Y: 2}}
	message1 := message.NewMessage("r1", []string{"r2"}, payload1)
	message2 := message.NewMessage("r2", []string{"r1"}, payload2)
	var wg sync.WaitGroup
	srv.Send(message1)
	srv.Send(message2)
	wg.Add(1)
	go func(r chan message.Payload) {
		defer wg.Done()
		msg := <-r
		assert.Equal(t, 2.0, msg.GetCoordinates().X, "Expected X to be 1")
		assert.Equal(t, payload2.GetID(), msg.GetID(), "Expected id to be equal")
	}(reciever1)
	wg.Add(1)
	go func(r chan message.Payload) {
		defer wg.Done()
		msg := <-r
		assert.Equal(t, 1.0, msg.GetCoordinates().X, "Expected X to be 1")
		assert.Equal(t, payload1.GetID(), msg.GetID(), "Expected id to be equal")
	}(reciever2)
	wg.Wait()
	srv.Close()
}
