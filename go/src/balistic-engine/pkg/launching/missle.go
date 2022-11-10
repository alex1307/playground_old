package launching

import (
	"balistic-engine/pkg/math"
	"sync"
	"sync/atomic"
	"time"

	"github.com/google/uuid"
	"go.uber.org/zap"
)

var Store = make(map[string]*Missle)
var Launched = make(map[string]bool)

type WaitGroupCount struct {
	sync.WaitGroup
	count int64
}

func (wg *WaitGroupCount) Add(delta int) {
	atomic.AddInt64(&wg.count, int64(delta))
	wg.WaitGroup.Add(delta)
}

func (wg *WaitGroupCount) Done() {
	atomic.AddInt64(&wg.count, -1)
	wg.WaitGroup.Done()
}

func (wg *WaitGroupCount) GetCount() int {
	return int(atomic.LoadInt64(&wg.count))
}

type Missle struct {
	ID            string
	done          chan struct{}
	Launch        math.Coordinates
	Velocity      float64
	Radians       float64
	MaxRange      float64
	MaxAlt        float64
	TimeInSeconds float64
	TimeInMillis  float64
	Status        string
}

type A struct {
}

// 1589 000 000
func New(position math.Coordinates, velocity float64, angle float64) *Missle {
	radians, _ := math.DegreesToRadians(angle)
	time, _ := math.ElapsedTime(position, velocity, radians)
	maxRange, _ := math.MaxRange(position, time, velocity, radians)
	maxAlt, _ := math.MaxAltitude(position, velocity, radians)
	return &Missle{uuid.Must(uuid.NewRandom()).String(),
		make(chan struct{}),
		position, velocity, radians, maxRange, maxAlt, time, time * 1000, "ready"}
}

func (m *Missle) CurrentPosition(time float64) math.Coordinates {
	position, _ := math.Position(m.Launch, m.Velocity, m.Radians, time)
	return position
}

func (m *Missle) Trajectory(logger *zap.Logger) chan math.Coordinates {
	c := make(chan math.Coordinates, 10)
	intervals, _ := math.TimeInterval(m.TimeInMillis, 100)
	m.Status = "launched"
	go func(logger *zap.Logger, c chan math.Coordinates) {
		defer close(c)
		for _, t := range intervals {

			select {
			case <-m.done:
				m.Status = "failure"
				logger.Info("DOWN!!!")
				return
			case <-time.After(time.Duration(int(m.TimeInMillis/100)) * time.Millisecond):
				c <- m.CurrentPosition(t)
			}
		}
		logger.Info("SUCCESS!!!")
		m.Status = "success"
	}(logger, c)

	return c
}

func (m *Missle) Down() {
	m.done <- struct{}{}
}
