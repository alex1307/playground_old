package artilery

import (
	"balistic-engine/pkg/math"
	"time"
)

type Proectile struct {
	ID     string
	done   chan struct{}
	tracer chan math.Coordinates
	status ProjectileStatus
}

func Fire(id string, coordinates math.Coordinates, velocity, radians float64) *Proectile {
	projectile := &Proectile{ID: id,
		status: Fired,
		done:   make(chan struct{}),
		tracer: make(chan math.Coordinates, 10)}
	go func(projectile *Proectile) {
		defer close(projectile.tracer)
		defer close(projectile.done)
		elapsed_time, _ := math.ElapsedTime(coordinates, velocity, radians)
		duration := time.Duration(elapsed_time) * time.Millisecond
		intervals, _ := math.TimeInterval(float64(duration.Milliseconds()), TIME_INTERVALS)
		waiting_time := time.Duration(duration.Milliseconds()/TIME_INTERVALS) * time.Millisecond
		for _, t := range intervals {
			select {
			case <-projectile.done:
				projectile.status = Failed
				return
			case <-time.After(waiting_time):
				coordinates, _ = math.Position(coordinates, velocity, radians, t)
				projectile.tracer <- coordinates
			}
		}
	}(projectile)
	return projectile
}

func (p *Proectile) GetStatus() ProjectileStatus {
	return p.status
}

func (p *Proectile) Trajectory() chan math.Coordinates {
	return p.tracer
}
