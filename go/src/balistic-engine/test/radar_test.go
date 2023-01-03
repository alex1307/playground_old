package test

import (
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/monitor"
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

var number_of_projectiles = 25

func TestRadarTraces10Projectiles(t *testing.T) {
	Setup()
	LAUNCHER.Load(number_of_projectiles)
	LAUNCHER2.Load(number_of_projectiles)
	assert.True(t, LAUNCHER.IsReady(), "Expected launcher to be loaded")
	assert.Equal(t, number_of_projectiles, LAUNCHER.AvailableAmmunitions(), "Expected launcher to be loaded")
	assert.True(t, LAUNCHER2.IsReady(), "Expected launcher to be loaded")
	assert.Equal(t, number_of_projectiles, LAUNCHER2.AvailableAmmunitions(), "Expected launcher to be loaded")
	go LAUNCHER.AutoFire(200)
	go LAUNCHER2.AutoFire(200)
	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)
	space := monitor.NewSpace(100.0, [2]math.Coordinates{
		{X: 0, Y: 0, T: 0},
		{X: 300, Y: 300, T: 0},
	})
	radar := monitor.NewRadar(BROKER, space)
	go PROJECTILES.Start(ctx)
	go radar.StartTracing(ctx)
	time.Sleep(25 * time.Second)
	ids := PROJECTILES.GetProjectilesIds()
	detected := radar.GetDetectedProjectiles()
	cancel()
	assert.Equal(t, 2*number_of_projectiles, len(ids), "Expected 20 projectiles")
	assert.Equal(t, 13, len(detected), "Expected 20 detected projectiles")
}
