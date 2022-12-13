package test

import (
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/monitoring"
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestRadarTraces10Projectiles(t *testing.T) {
	Setup()
	assert.True(t, LAUNCHER.IsReady(), "Expected launcher to be loaded")
	assert.Equal(t, 10, LAUNCHER.AvailableAmmunitions(), "Expected launcher to be loaded")
	LAUNCHER.AutoFire(200)
	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)
	space := monitoring.NewSpace(100.0, [2]math.Coordinates{
		{X: 0, Y: 0, T: 0},
		{X: 100, Y: 100, T: 0},
	})
	radar := monitoring.NewRadar(BROKER, space)
	go PROJECTILES.Start(ctx)
	go radar.StartTracing(ctx)
	time.Sleep(1 * time.Second)
	ids := PROJECTILES.GetProjectilesIds()
	detected := radar.GetDetectedProjectiles()
	cancel()
	assert.Equal(t, 10, len(ids), "Expected 10 projectiles")
	assert.Equal(t, 10, len(detected), "Expected 10 projectiles")
}
