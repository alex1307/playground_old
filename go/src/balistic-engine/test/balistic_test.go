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
const HEIGTH = 1.0

func TestPosition(t *testing.T) {
	Time := 0.5
	Radians, _ := math.DegreesToRadians(float64(DEGREES))
	position, err := math.Position(VELOCITY, Radians, HEIGTH, Time)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Printf(" {%f, %f} \n", position.X, position.Y)
}

func TestElapsedTime(t *testing.T) {
	Radians, _ := math.DegreesToRadians(DEGREES)
	ElapsedTime, err := math.ElapsedTime(VELOCITY, Radians, HEIGTH)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Printf("Elapsed time: %fs\n", ElapsedTime)
}

func TestMaxRange(t *testing.T) {
	Radians, _ := math.DegreesToRadians(DEGREES)
	MaxRange, err := math.MaxRange(VELOCITY, Radians, HEIGTH)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Printf("Max range: %fm \n", MaxRange)
}

func TestMaxAltitude(t *testing.T) {
	Radians, _ := math.DegreesToRadians(DEGREES)
	MaxAltitude, err := math.MaxAltitude(VELOCITY, Radians, HEIGTH)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Printf("Max altitude: %fm \n", MaxAltitude)
}

func TestVelocity(t *testing.T) {
	logger := config.Setup()
	Radians, _ := math.DegreesToRadians(DEGREES)
	Time := 0.5
	velocity, err := math.Velocity(VELOCITY, Radians, HEIGTH, Time)
	if err != nil {
		t.Errorf("Invalid parameters")
	}
	fmt.Println("Velocity:")
	t.Log("Log velocity")
	logger.Info("Velocity : ",
		zap.Float64("Velocity_X", velocity.X),
		zap.Float64("Velocity_Y", velocity.Y))
}
