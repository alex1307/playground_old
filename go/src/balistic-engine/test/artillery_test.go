package test

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/launcher"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"balistic-engine/pkg/projectile"
	"context"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"go.uber.org/zap"
)

var ONE_SHOT = 1
var COUNT = 3

func TestProjectilePosition(t *testing.T) {
	config.Setup()
	radians, _ := math.DegreesToRadians(45)
	coordinates, _ := math.Position(COORDINATES, 100, radians, 14.4209)
	config.AppLogger.Info("Coordinates: ", zap.Float64("x", coordinates.X), zap.Float64("y", coordinates.Y), zap.Float64("t", coordinates.T))
	assert.Equal(t, 1019.7116, float64(int64(coordinates.X*1_0000))/1_0000, "expected x is  1019.7162")
	assert.Equal(t, 0.0045, float64(int64(coordinates.Y*1_0000))/1_0000, "expected x is  0.0000")
}

func TestLaunch10MisslesAndCheckStatus(t *testing.T) {
	Setup()
	assert.True(t, LAUNCHER.IsReady(), "Expected launcher to be loaded")
	assert.Equal(t, 10, LAUNCHER.AvailableAmmunitions(), "Expected launcher to be loaded")
	LAUNCHER.AutoFire(200)
	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)
	go func(projectile *projectile.ProjectileHandler, ctx context.Context) {
		projectile.Start(ctx)
	}(PROJECTILES, ctx)
	time.Sleep(3 * time.Second)
	ids := PROJECTILES.GetProjectilesIds()
	cancel()
	assert.Equal(t, 10, len(ids), "Expected 10 projectiles")
	for _, id := range ids {
		config.AppLogger.Info("Projectile id: ", zap.String("id", id))
		info := PROJECTILES.GetProjectile(id)
		config.AppLogger.Info("Projectile info: ",
			zap.String("id", id),
			zap.Float64("max range", info.MaxRange),
			zap.Time("fired at", info.FireAt),
			zap.Float64("time to live", info.TimeToLive))
		assert.Equal(t, config.Fired, info.Status, "Expected projectile to be fired")
	}
}

func Test10MisslesFired5TakenDown(t *testing.T) {
	Setup()
	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)
	assert.True(t, LAUNCHER.IsReady(), "Expected launcher to be loaded")
	assert.Equal(t, 10, LAUNCHER.AvailableAmmunitions(), "Expected launcher to be loaded")
	LAUNCHER.AutoFire(200)
	go func(projectile *projectile.ProjectileHandler, ctx context.Context) {
		projectile.Start(ctx)
	}(PROJECTILES, ctx)
	time.Sleep(1 * time.Second)
	ids := PROJECTILES.GetProjectilesIds()
	for i, id := range ids {
		if i%2 == 0 {
			config.AppLogger.Info("Getting down id: ", zap.String("id", id))
			message := message.NewMessage("xxx", []string{"projectile"}, &launcher.Missle{ID: id})
			go BROKER.Send(message)
		}
	}
	time.Sleep(1 * time.Second)
	takenDown := PROJECTILES.GetByStatus(config.TakenDown)
	fired := PROJECTILES.GetByStatus(config.Fired)
	assert.Equal(t, 5, len(takenDown), "Expected 5 failed")
	assert.Equal(t, 5, len(fired), "Expected 5 fired")

	for _, id := range ids {
		info := PROJECTILES.GetProjectile(id)
		config.AppLogger.Info("Projectile info: ",
			zap.String("id", id),
			zap.Float64("max range", info.MaxRange),
			zap.Time("fired at", info.FireAt),
			zap.Float64("time to live", info.TimeToLive),
			zap.String("status", info.Status.String()))

	}
	cancel()
}

func Test10MisslesFired5DownAnd5Missed(t *testing.T) {
	Setup()
	ctx := context.Background()
	assert.True(t, LAUNCHER.IsReady(), "Expected launcher to be loaded")
	assert.Equal(t, 10, LAUNCHER.AvailableAmmunitions(), "Expected launcher to be loaded")
	LAUNCHER.AutoFire(200)
	var wg sync.WaitGroup
	go func(projectile *projectile.ProjectileHandler, ctx context.Context) {
		projectile.StartAndWait(ctx, &wg)
	}(PROJECTILES, ctx)

	time.Sleep(1 * time.Second)
	ids := PROJECTILES.GetProjectilesIds()
	for i, id := range ids {
		if i%2 == 0 {
			config.AppLogger.Info("Getting down id: ", zap.String("id", id))
			message := message.NewMessage("xxx", []string{"projectile"}, &launcher.Missle{ID: id})
			go BROKER.Send(message)
		}
	}
	wg.Wait()
	takenDown := PROJECTILES.GetByStatus(config.TakenDown)
	fired := PROJECTILES.GetByStatus(config.Fired)
	assert.Equal(t, 5, len(takenDown), "Expected 5 failed")
	assert.Equal(t, 5, len(fired), "Expected 5 fired")
}

func TestRadarWith10Missles(t *testing.T) {
	Setup()
	ctx := context.Background()
	assert.True(t, LAUNCHER.IsReady(), "Expected launcher to be loaded")
	assert.Equal(t, 10, LAUNCHER.AvailableAmmunitions(), "Expected launcher to be loaded")
	LAUNCHER.AutoFire(200)
	var wg sync.WaitGroup
	go func(projectile *projectile.ProjectileHandler, ctx context.Context) {
		projectile.StartAndWait(ctx, &wg)
	}(PROJECTILES, ctx)

	time.Sleep(1 * time.Second)
	ids := PROJECTILES.GetProjectilesIds()
	for i, id := range ids {
		if i%2 == 0 {
			config.AppLogger.Info("Getting down id: ", zap.String("id", id))
			message := message.NewMessage("xxx", []string{"projectile"}, &launcher.Missle{ID: id})
			go BROKER.Send(message)
		}
	}
	wg.Wait()
	takenDown := PROJECTILES.GetByStatus(config.TakenDown)
	fired := PROJECTILES.GetByStatus(config.Fired)
	assert.Equal(t, 5, len(takenDown), "Expected 5 failed")
	assert.Equal(t, 5, len(fired), "Expected 5 fired")
}
