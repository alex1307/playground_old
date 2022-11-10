package test

import (
	"balistic-engine/pkg/artilery"
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"go.uber.org/zap"
)

var logger = config.Setup()

func TestMissle(t *testing.T) {
	missle := artilery.NewMissle(100.0)
	assert.NotEmpty(t, missle.ID, "expected id is not empty")
	assert.Equal(t, float64(100), missle.Velocity, "expected velocity is 100")
}

func TestLauncher(t *testing.T) {
	missle := artilery.NewMissle(100.0)
	launcher := artilery.NewLauncher(math.Coordinates{X: 0, Y: 0, T: 0})

	assert.NotEmpty(t, missle.ID, "expected id is not empty")
	assert.NotEmpty(t, launcher.ID, "expected id is not empty")
	assert.Equal(t, artilery.Unloaded, launcher.GetStatus(), "expected status is Unloaded")
	assert.Empty(t, launcher.Missles, "missles should be empty")
	assert.Equal(t, 0, launcher.Capasity, "capasity should be 0")

	launcher.Load(10, *missle, 45)
	assert.Equal(t, artilery.Loaded, launcher.GetStatus(), "expected status is Fired")
	assert.Equal(t, 10, len(launcher.Missles), "missles should be empty")
	assert.Equal(t, 10, launcher.Capasity, "capasity should be 10")
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		logger.Info("Firing...")
		start := time.Now()
		projectiles := launcher.Fire(200)
		elapsed := time.Since(start)
		logger.Info("Fired! Done!", zap.Duration("elapsed", elapsed))
		assert.Equal(t, 10, len(projectiles), "fired projectiles should be 10")
	}()
	logger.Info("Keep going and wait a bit...")
	time.Sleep(50 * time.Millisecond)
	assert.Equal(t, artilery.InUse, launcher.GetStatus(), "expected status is InUse")
	wg.Wait()
	assert.Equal(t, artilery.Unloaded, launcher.GetStatus(), "expected status is Fired")
}
