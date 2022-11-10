package test

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"fmt"
	"testing"

	"go.uber.org/zap"
)

const VELOCITY = 8.0
const DEGREES = 60

var RADIANS, _ = math.DegreesToRadians(DEGREES)
var ELAPSED_TIME, _ = math.ElapsedTime(POSITION, VELOCITY, RADIANS)
var POSITION = math.Coordinates{X: 0.0, Y: 1.0, T: 0.0}

func TestPosition(t *testing.T) {

	Time := 0.5
	Radians, _ := math.DegreesToRadians(float64(DEGREES))
	position, err := math.Position(POSITION, VELOCITY, Radians, Time)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Printf(" {%f, %f} \n", position.X, position.Y)
}

func TestElapsedTime(t *testing.T) {
	fmt.Printf("Elapsed time: %fs\n", ELAPSED_TIME)
}

func TestMaxRange(t *testing.T) {
	MaxRange, err := math.MaxRange(POSITION, ELAPSED_TIME, VELOCITY, RADIANS)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Printf("Max range: %fm \n", MaxRange)
}

func TestMaxAltitude(t *testing.T) {
	MaxAltitude, err := math.MaxAltitude(POSITION, VELOCITY, DEGREES)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Printf("Max altitude: %fm \n", MaxAltitude)
}

func TestVelocity(t *testing.T) {
	logger := config.Setup()
	Time := 0.5
	velocity, err := math.Velocity(VELOCITY, RADIANS, Time)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Println("Velocity:")
	t.Log("Log velocity")
	logger.Info("Velocity : ",
		zap.Float64("Velocity_X", velocity.X),
		zap.Float64("Velocity_Y", velocity.Y))
}
