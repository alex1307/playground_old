package test

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/launching"
	"balistic-engine/pkg/math"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"go.uber.org/zap"
)

var STORE = make(map[string]*launching.Missle)

func TestMissleDown(t *testing.T) {
	logger := config.Setup()
	missle := launching.New(math.Coordinates{X: 0.0, Y: 0.0, T: 0.0}, 100.0, 60)
	assert.Equal(t, "ready", missle.Status, "expected status is ready")
	logger.Info("Missle  ", zap.String("id", missle.ID), zap.String("Status", missle.Status))
	trajectory := missle.Trajectory(logger)
	assert.Equal(t, "launched", missle.Status, "expected status is launched")
	go func(missle launching.Missle) {
		time.Sleep(2 * time.Second)
		missle.Down()
	}(*missle)
	counter := 0
	for val := range trajectory {
		counter++
		if counter%10 == 0 {
			logger.Info("Missle position: ",
				zap.String("id", missle.ID),
				zap.Float64("X", val.X),
				zap.Float64("Y", val.Y),
				zap.Float64("Time", val.T),
				zap.String("Status", missle.Status))
		}
	}
	assert.Equal(t, "failure", missle.Status, "expected status is failure")
	logger.Info("Missle  ", zap.String("id", missle.ID), zap.String("Status", missle.Status))
}

func TestMissleSuccess(t *testing.T) {
	logger := config.Setup()
	missle := launching.New(math.Coordinates{X: 0.0, Y: 0.0, T: 0.0}, 10.0, 60)
	assert.Equal(t, "ready", missle.Status, "expected status is ready")
	logger.Info("Missle  ", zap.String("id", missle.ID), zap.String("Status", missle.Status))
	go func() {
		trajectory := missle.Trajectory(logger)
		assert.Equal(t, "launched", missle.Status, "expected status is launched")
		for t := range trajectory {
			func(t math.Coordinates) {
				// do nothing
			}(t)
		}
		logger.Info("started...lets wait for 2 seconds")
	}()

	logger.Info("sleeping at:", zap.Int("now: ", time.Now().Second()))
	time.Sleep(2 * time.Second)
	logger.Info("sleeping at:", zap.Int("after: ", time.Now().Second()))
	assert.Equal(t, "success", missle.Status, "expected status is success")
}

func TestMulitpleMissles(t *testing.T) {
	logger := config.Setup()
	var wg launching.WaitGroupCount
	wg.Add(5)
	wg.Add(5)
	logger.Info("waiting gropus are: ", zap.Int("count", wg.GetCount()))
	for i := 0; i < 10; i++ {
		missle := launching.New(math.Coordinates{X: 0.0, Y: 0.0, T: 0.0}, 100.0, 60)
		STORE[missle.ID] = missle
	}
	assert.Equal(t, 10, len(STORE), "expected 10 missles")
	go func() {
		time.Sleep(16 * time.Second)
		counter := 0
		for _, missle := range STORE {
			if counter%2 == 0 {
				missle.Down()
			}
			counter++
		}
	}()
	for _, missle := range STORE {
		_, ok := launching.Launched[missle.ID]
		if !ok {
			go func(missle launching.Missle) {
				trajectory := missle.Trajectory(logger)
				launching.Launched[missle.ID] = true
				assert.Equal(t, "launched", missle.Status, "expected status is launched")
				for t := range trajectory {
					func(t math.Coordinates) {

						logger.Info("Trajectory: ", zap.String("id", missle.ID),
							zap.Float64("X", t.X),
							zap.Float64("Y", t.Y),
							zap.Float64("Time", t.T))
					}(t)
				}
				wg.Done()
			}(*missle)
		}

	}
	wg.Wait()
}

func Down(missle launching.Missle) {
	missle.Down()
}
