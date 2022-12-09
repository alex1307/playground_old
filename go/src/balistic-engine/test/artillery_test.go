package test

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"testing"

	"github.com/stretchr/testify/assert"
	"go.uber.org/zap"
)

var ONE_SHOT = 1
var COUNT = 3

var COORDINATES = math.Coordinates{X: 0, Y: 0, T: 0}

func TestProjectilePosition(t *testing.T) {
	radians, _ := math.DegreesToRadians(45)
	coordinates, _ := math.Position(COORDINATES, 100, radians, 14.4209)
	config.AppLogger.Info("Coordinates: ", zap.Float64("x", coordinates.X), zap.Float64("y", coordinates.Y), zap.Float64("t", coordinates.T))
	assert.Equal(t, 1019.7116, float64(int64(coordinates.X*1_0000))/1_0000, "expected x is  1019.7162")
	assert.Equal(t, 0.0045, float64(int64(coordinates.Y*1_0000))/1_0000, "expected x is  0.0000")
}
