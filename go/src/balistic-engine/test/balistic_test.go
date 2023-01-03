package test

import (
	"balistic-engine/pkg/config"
	balistic "balistic-engine/pkg/math"
	"fmt"
	"math"
	"testing"

	"github.com/stretchr/testify/assert"
	"go.uber.org/zap"
)

const VELOCITY = 50.0
const DEGREES = 60

var RADIANS, _ = balistic.DegreesToRadians(DEGREES)
var ELAPSED_TIME, _ = balistic.ElapsedTime(POSITION, -VELOCITY, RADIANS)
var POSITION = balistic.Coordinates{X: 0.0, Y: 1.0, T: 0.0}

func TestPosition(t *testing.T) {

	Time := 0.5
	Radians, _ := balistic.DegreesToRadians(float64(DEGREES))
	position, err := balistic.Position(POSITION, VELOCITY, Radians, Time)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Printf(" {%f, %f} \n", position.X, position.Y)
}

func TestElapsedTime(t *testing.T) {
	fmt.Printf("Elapsed time: %fs\n", ELAPSED_TIME)
}

func TestMaxRange(t *testing.T) {
	MaxRange, err := balistic.MaxRange(POSITION, ELAPSED_TIME, VELOCITY, RADIANS)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Printf("Max range: %fm \n", MaxRange)
}

func TestMaxAltitude(t *testing.T) {
	MaxAltitude, err := balistic.MaxAltitude(POSITION, VELOCITY, DEGREES)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Printf("Max altitude: %fm \n", MaxAltitude)
}

func TestVelocity(t *testing.T) {
	config.Setup()
	Time := 0.5
	velocity1, _ := balistic.Velocity(VELOCITY, RADIANS, Time)
	velocity2, _ := balistic.Velocity(-VELOCITY, RADIANS, Time)

	fmt.Println("Velocity:")
	t.Log("Log velocity")
	config.AppLogger.Info("Velocity : ",
		zap.Float64("Velocity_X", velocity1.X),
		zap.Float64("Velocity_Y", velocity1.Y))
	config.AppLogger.Info("Velocity : ",
		zap.Float64("Velocity_X", velocity2.X),
		zap.Float64("Velocity_Y", velocity2.Y))
	assert.Equal(t, velocity1.X, -velocity2.X, "Expected to be eual by abs value")
	assert.Equal(t, math.Abs(velocity1.X), math.Abs(-velocity2.X), "Expected to be eual by abs value")
	assert.Equal(t, velocity1.Y, velocity2.Y, "Expected to be eual by abs value")
}
