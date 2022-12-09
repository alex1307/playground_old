package test

import (
	"balistic-engine/pkg/artillery"
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/monitoring"
	"context"
	"testing"

	"github.com/google/uuid"
	"github.com/stretchr/testify/assert"
	"go.uber.org/zap"
)

func TestScenario_1(t *testing.T) {
	config.Setup()
	RADIANS_45, _ = math.DegreesToRadians(45)
	radar := monitoring.NewRadarStation(2)
	space := monitoring.NewSpace(120.0, [2]math.Coordinates{
		{X: 0, Y: 0, T: 0},
		{X: 1200.0, Y: 1200.0, T: 20.0},
	})
	anti_missle := artillery.TrajectoryCoordinates{
		ID: uuid.NewString(),
		Coordinates: math.Coordinates{
			X: 418.08,
			Y: 244.73,
			T: 5.76,
		},
		TeamID: "Ukraine",
	}
	id := uuid.NewString()
	radar.Subscribe(id)
	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)
	go func() {
		radar.StartTracing(id, ctx)
		config.AppLogger.Info("The radar is started", zap.String("id", id))
	}()
	missle := artillery.Missle{
		ID:       uuid.NewString(),
		Velocity: 100.0,
		Radians:  RADIANS_45,
		Source: math.Coordinates{
			X: 0,
			Y: 0,
			T: 0,
		},
	}
	radar.TraceProjectile(id, missle)
	go Cancel(18, cancel)
	space.Move(anti_missle)
	go func() {
		space.Notify(50, ctx)
	}()
	go func(radar *monitoring.RadarStation, space *monitoring.Space, cancel context.CancelFunc) {

		for uuid := range space.GetCallback() {
			config.AppLogger.Info("### callback", zap.String("id", uuid))
			radar.Down(uuid)
		}
		config.AppLogger.Info("### callback is finished. Canceling the context")
		cancel()
	}(radar, space, cancel)
	for cc := range radar.ProjectileMonitor() {
		i := space.GetIndex(cc.ID)
		config.AppLogger.Info("projectile", zap.String("id", cc.ID),
			zap.Float64("x", cc.Coordinates.X),
			zap.Float64("y", cc.Coordinates.Y),
			zap.Float64("t", cc.Coordinates.T),
			zap.Int("index", i))

		space.Move(cc)
	}

	index := space.GetIndex(missle.ID)
	assert.Equal(t, 102, index, "index should be 102")
}
