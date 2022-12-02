package test

import (
	"balistic-engine/pkg/artillery"
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/monitoring"
	"context"
	"sync"
	"testing"
	"time"

	"github.com/google/uuid"
	"github.com/stretchr/testify/assert"
	"go.uber.org/zap"
)

var LAUNCHER_ID string
var RADIANS_45, RADIANS_30, RADIANS_60 float64
var NUMBER_OF_LAUNCHERS int = 2

func LoadSystem(config []artillery.ShootingConfiguration) *artillery.MissleSystem {
	system := artillery.NewMissleSystem("Ukraine")
	launcher := artillery.NewLauncher(math.Coordinates{X: 0, Y: 0, T: 0})
	LAUNCHER_ID = launcher.ID
	system.AddLauncherToMissleSystem(launcher)
	launcher_id := LAUNCHER_ID
	system.LoadLauncher(launcher_id, NUMBER_OF_LAUNCHERS, config)
	return system
}

func TestTraceProjectile(t *testing.T) {
	config.Setup()
	RADIANS_45, _ = math.DegreesToRadians(45)
	shooting_config := []artillery.ShootingConfiguration{
		{
			Velocity: 100.0,
			Radians:  RADIANS_45,
		}}
	system := LoadSystem(shooting_config)

	launchers := system.Filter(LAUNCHER_ID)
	assert.NotNil(t, launchers, "launchers is nil")
	assert.Equal(t, NUMBER_OF_LAUNCHERS, len(launchers), "only one launcher should be loaded")
	config.AppLogger.Info("launcher", zap.String("launcher", LAUNCHER_ID))
	launcher := launchers[0]
	assert.Equal(t, 10, launcher.AvailableAmmunitions(), "capasity should be 10")
	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)
	go func() {
		time.Sleep(5 * time.Second)
		cancel()
	}()
	var wg sync.WaitGroup
	wg.Add(1)
	go func(ctx context.Context) {
		defer wg.Done()
		go func() {
			system.ManualFire(LAUNCHER_ID, 100.0, RADIANS_45)
		}()
		time.Sleep(50 * time.Millisecond)
		cc := system.Monitoring(ctx)
		for coords := range cc {
			select {
			case <-ctx.Done():
				return
			default:
				config.AppLogger.Info("coords",
					zap.Float64("x", coords.Coordinates.X),
					zap.Float64("y", coords.Coordinates.Y),
					zap.Float64("t", coords.Coordinates.T))
			}

		}
	}(ctx)
	wg.Wait()
}

func TestAutomaticFire(t *testing.T) {
	config.Setup()
	RADIANS_45, _ = math.DegreesToRadians(45)
	RADIANS_30, _ = math.DegreesToRadians(30)
	RADIANS_60, _ = math.DegreesToRadians(60)
	shooting_config := []artillery.ShootingConfiguration{
		{
			Velocity: 100.0,
			Radians:  RADIANS_45,
		},
		{
			Velocity: 100.0,
			Radians:  RADIANS_30,
		},
		{
			Velocity: 100.0,
			Radians:  RADIANS_60,
		}}
	system := LoadSystem(shooting_config)
	ctx := context.Background()
	launchers := system.Filter(LAUNCHER_ID)
	assert.NotNil(t, launchers, "launchers is nil")
	assert.Equal(t, NUMBER_OF_LAUNCHERS, len(launchers), "only one launcher should be loaded")
	config.AppLogger.Info("launcher", zap.String("launcher", LAUNCHER_ID))
	var wg sync.WaitGroup
	go func() {
		config.AppLogger.Info("automatic fire....")
		wg.Add(1)
		go func() {
			defer wg.Done()
			system.AutomaticFire("*", 200)
		}()

	}()

	wg.Add(1)
	go func(ctx context.Context) {
		defer wg.Done()

		time.Sleep(50 * time.Millisecond)
		cc := system.Monitoring(ctx)
		counter := 0
		for coords := range cc {
			select {
			case <-ctx.Done():
				return
			default:

				if counter%33 == 0 {
					config.AppLogger.Info("coords",
						zap.String("id", coords.ID),
						zap.Float64("x", coords.Coordinates.X),
						zap.Float64("y", coords.Coordinates.Y),
						zap.Float64("t", coords.Coordinates.T))
				}
				counter++
			}
			// counter++

		}
		assert.Equal(t, 1000, counter, "counter should be 1000")
	}(ctx)
	time.Sleep(10 * time.Second)
	wg.Wait()
}

func TestRadar(t *testing.T) {
	config.Setup()
	RADIANS_45, _ = math.DegreesToRadians(45)
	radar := monitoring.NewRadarStation(2)
	id := uuid.NewString()
	radar.Subscribe(id)
	config.AppLogger.Info("subscribed", zap.String("id", id))
	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)
	go func() {
		radar.StartTracing(id, ctx)
		config.AppLogger.Info("The radar is started")
	}()

	radar.TraceProjectile(id, artillery.Missle{
		ID:       uuid.NewString(),
		Velocity: 100.0,
		Radians:  RADIANS_45,
		Source: math.Coordinates{
			X: 0,
			Y: 0,
			T: 0,
		}})
	time.Sleep(1 * time.Second)
	radar.TraceProjectile(id, artillery.Missle{
		ID:       uuid.NewString(),
		Velocity: 100.0,
		Radians:  RADIANS_45,
		Source: math.Coordinates{
			X: 0,
			Y: 0,
			T: 0,
		}})
	counter := 0
	// go func() {
	// 	time.Sleep(3 * time.Second)
	// 	if radar.IsActive() {
	// 		ids := radar.GetActiveIDs()
	// 		for i, id := range ids {
	// 			if i%10 == 0 {
	// 				config.AppLogger.Info("Getting down...", zap.String("id", id))
	// 				radar.Down(id)
	// 			}
	// 		}
	// 	}
	// }()
	go func() {
		for i := 0; i <= 18; i++ {
			time.Sleep(1 * time.Second)
			if i%5 == 0 {
				config.AppLogger.Info("waiting", zap.Int("elapsed seconds: ", i))
			}
		}
		config.AppLogger.Info("canceling...")
		cancel()
	}()
	for cc := range radar.ProjectileMonitor() {
		counter++

		if counter%33 == 0 {
			config.AppLogger.Info("coords",
				zap.String("id", cc.ID),
				zap.Float64("x", cc.Coordinates.X),
				zap.Float64("y", cc.Coordinates.Y),
				zap.Float64("t", cc.Coordinates.T))
		}

	}
	assert.Equal(t, 2*(artillery.TIME_INTERVALS+1), counter, "counter should be 200")

}

func TestRadarStation(t *testing.T) {
	config.Setup()
	RADIANS_45, _ = math.DegreesToRadians(45)
	radar := monitoring.NewRadarStation(2)
	ctx1 := context.Background()
	ctx1, cancel1 := context.WithCancel(ctx1)
	ctx2 := context.Background()
	ctx2, cancel2 := context.WithCancel(ctx2)
	id1 := uuid.NewString()
	id2 := uuid.NewString()
	radar.Subscribe(id1)
	radar.Subscribe(id2)
	go func() {
		radar.StartTracing(id1, ctx1)
		config.AppLogger.Info("The radar is started", zap.String("id", id1))
	}()
	go func() {
		radar.StartTracing(id2, ctx2)
		config.AppLogger.Info("The radar is started", zap.String("id", id2))
	}()
	radar.TraceProjectile(id2, artillery.Missle{
		ID:       uuid.NewString(),
		Velocity: 100.0,
		Radians:  RADIANS_45,
		Source: math.Coordinates{
			X: 0,
			Y: 0,
			T: 0,
		}})
	time.Sleep(1 * time.Second)
	radar.TraceProjectile(id1, artillery.Missle{
		ID:       uuid.NewString(),
		Velocity: 50.0,
		Radians:  RADIANS_45,
		Source: math.Coordinates{
			X: 0,
			Y: 0,
			T: 0,
		}})

	go cancel(18, cancel1)
	go cancel(18, cancel2)
	counter := 0
	for cc := range radar.ProjectileMonitor() {
		counter++

		if counter%33 == 0 {
			config.AppLogger.Info("coords",
				zap.String("id", cc.ID),
				zap.Float64("x", cc.Coordinates.X),
				zap.Float64("y", cc.Coordinates.Y),
				zap.Float64("t", cc.Coordinates.T))
		}

	}
	assert.Equal(t, 2*(artillery.TIME_INTERVALS+1), counter, "counter should be 202")
}

func cancel(wait_time int, cancel context.CancelFunc) {
	time.Sleep(time.Duration(wait_time) * time.Second)
	cancel()
}
