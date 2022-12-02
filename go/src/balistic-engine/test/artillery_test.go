package test

import (
	"balistic-engine/pkg/artillery"
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"go.uber.org/zap"
)

var ONE_SHOT = 1
var COUNT = 3

var COORDINATES = math.Coordinates{X: 0, Y: 0, T: 0}

func TestMissle(t *testing.T) {
	missle := artillery.NewMissle(100.0)
	assert.NotEmpty(t, missle.ID, "expected id is not empty")
	assert.Equal(t, float64(100), missle.Velocity, "expected velocity is 100")
}

func TestLauncher(t *testing.T) {
	missle := artillery.NewMissle(100.0)
	launcher := artillery.NewLauncher(math.Coordinates{X: 0, Y: 0, T: 0})
	radians, _ := math.DegreesToRadians(45)
	assert.NotEmpty(t, missle.ID, "expected id is not empty")
	assert.NotEmpty(t, launcher.ID, "expected id is not empty")
	assert.False(t, launcher.IsReady(), "expected status is Unloaded")
	assert.Equal(t, 0, launcher.AvailableAmmunitions(), "capasity should be 0")

	launcher.Load(1)
	assert.True(t, launcher.IsReady(), "expected status is Ready(true)")
	assert.Equal(t, 1, launcher.AvailableAmmunitions(), "capasity should be 10")
	var wg sync.WaitGroup
	wg.Add(1)

	config.AppLogger.Info("Firing...")
	go func() {
		defer wg.Done()
		projectile := launcher.Fire(missle.ID, 100.00, radians)
		assert.Equal(t, artillery.Fired, projectile.GetStatus(), "expected status is Fired")
	}()
	start := time.Now()
	time.Sleep(50 * time.Millisecond)
	elapsed := time.Since(start)
	config.AppLogger.Info("Fired! Done!", zap.Duration("elapsed", elapsed))
	wg.Wait()
	assert.False(t, launcher.IsReady(), "expected status is NOT Ready(false)")

}

// Path: test/launcher_test.go
// Compare this snippet from pkg/artillery/enums.go:
// package artillery
//
// type ProjectileStatus int
// type LauncherStatus int

// const VELOCITY_FACTOR = 0.985
// const TIME_INTERVALS = 100
// const TIME_FACTOR = 1.5
// const INTERVALS = TIME_FACTOR * TIME_INTERVALS

// const (
// 	Fired ProjectileStatus = iota + 1
// 	Success
// 	Failed
// )

// const (
// 	Unloaded LauncherStatus = iota + 1
// 	Loaded
// 	InUse
// )

func TestMissleSystem(t *testing.T) {
	var coordinates []math.Coordinates = make([]math.Coordinates, 1)
	ms := artillery.LaunchMissleSystem("Ukraine", coordinates)
	ms.Load()
	wait := ms.Launcher[0].AvailableAmmunitions() * 200
	assert.True(t, ms.Launcher[0].IsReady(), "expected status is Ready(true)")
	assert.Greater(t, ms.Launcher[0].AvailableAmmunitions(), 10, "min capacity is 10")
	go func(ms *artillery.MissleSystem) {
		ms.Fire(200)

	}(ms)
	time.Sleep(100 * time.Millisecond)
	assert.Equal(t, 1, len(ms.Launcher), "expected projectiles is 1")
	assert.True(t, ms.Launcher[0].IsReady(), "expected status is Ready(true)")
	time.Sleep(time.Duration(wait)*time.Millisecond + 500*time.Millisecond)
	assert.False(t, ms.Launcher[0].IsReady(), "expected status is NOT Ready(false)")
	// for i := 0; i < 5; i++ {
	// 	radians, _ := math.DegreesToRadians(15 + float64(i*15))
	// 	launcher.Load(ONE_SHOT)
	// 	projectile := launcher.Fire(200, missle.ID, 100.00, radians, logger)
	// 	assert.Equal(t, artillery.Fired, projectile.GetStatus(), "expected status is Fired")
	// }
	// var wg sync.WaitGroup
	// for _, p := range projectiles {
	// 	wg.Add(1)
	// 	go func(p *artillery.Proectile) {
	// 		defer wg.Done()
	// 		//assert.Equal(t, artillery.Fired, p.GetStatus(), "expected status is Fired")
	// 		for coords := range p.Trajectory() {
	// 			if coords.Coordinates.T == p.TTL() {
	// 				logger.Info("Coordinates: ",
	// 					zap.Time("time", coords.Fired_at),
	// 					zap.Int("time now: ", coords.Fired_at.Nanosecond()+int(coords.Coordinates.T)*1_000_000_000),
	// 					zap.String("ID", coords.ID),
	// 					zap.Float64("x", coords.Coordinates.X), zap.Float64("y", coords.Coordinates.Y), zap.Float64("t", coords.Coordinates.T))
	// 				assert.Equal(t, float64(int64(p.MaxRange()*1_0000))/1_0000, float64(int64(coords.Coordinates.X*1_0000))/1_0000, "expected x is  1019.7162")
	// 				assert.Equal(t, 0.0, float64(int64(coords.Coordinates.Y*1_0000))/1_0000, "expected x is  0.0")
	// 			}

	// 		}
	// 	}(p)
	// }

	// wg.Wait()

	// for _, p := range projectiles {
	// 	logger.Info("Projectile: ", zap.String("ID", p.ID), zap.String("Status", p.GetStatus().String()))
	// 	assert.Equal(t, artillery.Success, p.GetStatus(), "expected status is Success")
	// }
}

func Test50PercentageDown(t *testing.T) {
	var coordinates []math.Coordinates = make([]math.Coordinates, 1)
	ms := artillery.LaunchMissleSystem("Ukraine", coordinates)
	ms.Load()
	loaded := ms.Launcher[0].AvailableAmmunitions()
	wait := ms.Launcher[0].AvailableAmmunitions() * 200
	assert.True(t, ms.Launcher[0].IsReady(), "expected status is Ready(true)")
	assert.Greater(t, ms.Launcher[0].AvailableAmmunitions(), 10, "min capacity is 10")
	go func(ms *artillery.MissleSystem) {
		ms.Fire(200)

	}(ms)
	time.Sleep(100 * time.Millisecond)
	assert.Equal(t, 1, len(ms.Launcher), "expected projectiles is 1")
	assert.True(t, ms.Launcher[0].IsReady(), "expected status is Ready(true)")
	time.Sleep(time.Duration(wait)*time.Millisecond + 300*time.Millisecond)
	assert.False(t, ms.Launcher[0].IsReady(), "expected status is NOT Ready(false)")
	var wg sync.WaitGroup
	// down := 0
	// for _, p := range artillery.PROJECTILES {

	// 	if down%2 == 0 {
	// 		go func(p *artillery.Proectile) {
	// 			p.Down()
	// 		}(p)
	// 	}
	// 	down++
	// }
	// for _, p := range artillery.PROJECTILES {
	// 	config.AppLogger.Info("Projectile: ", zap.String("ID", p.ID), zap.String("Status", p.GetStatus().String()))
	// 	wg.Add(1)
	// 	go func(p *artillery.Proectile) {
	// 		defer wg.Done()
	// 		for coords := range p.Trajectory() {
	// 			if coords.Coordinates.T == p.TTL() {
	// 				config.AppLogger.Info("Coordinates: ",
	// 					zap.String("ID", coords.ID),
	// 					zap.Float64("x", coords.Coordinates.X), zap.Float64("y", coords.Coordinates.Y), zap.Float64("t", coords.Coordinates.T))
	// 				assert.Equal(t, float64(int64(p.MaxRange()*1_0000))/1_0000, float64(int64(coords.Coordinates.X*1_0000))/1_0000, "expected X is  1019.7162")
	// 				assert.Equal(t, 0.0, float64(int64(coords.Coordinates.Y*1_0000))/1_0000, "expected Y is  0.0")
	// 			}

	// 		}
	// 	}(p)
	// }
	wg.Wait()
	down_counter := 0
	success_counter := 0
	// for _, p := range artillery.PROJECTILES {
	// 	if p.GetStatus() == artillery.Success {
	// 		success_counter++
	// 	} else {
	// 		down_counter++
	// 	}
	// }

	if loaded%2 == 0 {
		assert.Equal(t, down_counter, success_counter, "expected counters to be equal")
	} else {
		assert.Equal(t, down_counter, success_counter+1, "expected counters to be equal")
	}

}

func TestTrajectories(t *testing.T) {
	var coordinates []math.Coordinates = make([]math.Coordinates, 1)
	ms := artillery.LaunchMissleSystem("Ukraine", coordinates)
	ms.Load()
	loaded := ms.Launcher[0].AvailableAmmunitions()
	wait := ms.Launcher[0].AvailableAmmunitions() * 200
	// initial_capacity := ms.Launcher[0].AvailableAmmunitions()
	assert.True(t, ms.Launcher[0].IsReady(), "expected status is Ready(true)")
	assert.Greater(t, ms.Launcher[0].AvailableAmmunitions(), 10, "min capacity is 10")
	go func(ms *artillery.MissleSystem) {
		ms.Fire(200)

	}(ms)
	time.Sleep(100 * time.Millisecond)
	assert.Equal(t, 1, len(ms.Launcher), "expected projectiles is 1")
	assert.True(t, ms.Launcher[0].IsReady(), "expected status is Ready(true)")
	time.Sleep(time.Duration(wait)*time.Millisecond + 250*time.Millisecond)
	assert.False(t, ms.Launcher[0].IsReady(), "expected status is NOT Ready(false)")

	var wg sync.WaitGroup
	counter := 0
	// for _, p := range artillery.PROJECTILES {
	// 	config.AppLogger.Info("Projectile: ", zap.String("ID", p.ID), zap.String("Status", p.GetStatus().String()))
	// 	assert.Equal(t, artillery.Fired, p.GetStatus(), "expected status is Fired")
	// 	wg.Add(1)
	// 	go func(p *artillery.Proectile, counter *int) {
	// 		defer wg.Done()
	// 		for coords := range p.Trajectory() {
	// 			*counter++
	// 			if coords.Coordinates.T == p.TTL() {
	// 				config.AppLogger.Info("counter: ", zap.Int("counter", *counter))
	// 				config.AppLogger.Info("Coordinates: ",
	// 					zap.String("ID", coords.ID),
	// 					zap.Float64("x", coords.Coordinates.X), zap.Float64("y", coords.Coordinates.Y), zap.Float64("t", coords.Coordinates.T))
	// 				assert.Equal(t, float64(int64(p.MaxRange()*1_0000))/1_0000, float64(int64(coords.Coordinates.X*1_0000))/1_0000, "expected X is  1019.7162")
	// 				assert.Equal(t, 0.0, float64(int64(coords.Coordinates.Y*1_0000))/1_0000, "expected Y is  0.0")
	// 			}

	// 		}
	// 	}(p, &counter)
	// }
	wg.Wait()
	assert.Equal(t, loaded*(artillery.TIME_INTERVALS+1), counter, "expected counter is 1")
	// for p := range artillery.PROJECTILES {
	// 	assert.Equal(t, artillery.Success, artillery.PROJECTILES[p].GetStatus(), "expected status is Success")
	// }
}

func TestProjectilePosition(t *testing.T) {
	radians, _ := math.DegreesToRadians(45)
	coordinates, _ := math.Position(COORDINATES, 100, radians, 14.4209)
	config.AppLogger.Info("Coordinates: ", zap.Float64("x", coordinates.X), zap.Float64("y", coordinates.Y), zap.Float64("t", coordinates.T))
	assert.Equal(t, 1019.7116, float64(int64(coordinates.X*1_0000))/1_0000, "expected x is  1019.7162")
	assert.Equal(t, 0.0045, float64(int64(coordinates.Y*1_0000))/1_0000, "expected x is  0.0000")
}
