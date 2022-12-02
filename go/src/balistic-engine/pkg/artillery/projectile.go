package artillery

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"time"

	"go.uber.org/zap"
)

type Proectile struct {
	ID        string
	done      chan struct{}
	tracer    chan TrajectoryCoordinates
	status    ProjectileStatus
	ttl       float64
	max_range float64
}

type TrajectoryCoordinates struct {
	ID          string
	TeamID      string
	Coordinates math.Coordinates
}

func Fire(id string, source math.Coordinates, velocity, radians float64) *Proectile {
	projectile := &Proectile{ID: id,
		status: Fired,
		done:   make(chan struct{}),
		tracer: make(chan TrajectoryCoordinates, 10)}
	go func(projectile *Proectile) {
		config.AppLogger.Info("Firing projectile", zap.String("id", projectile.ID))
		defer close(projectile.tracer)
		defer close(projectile.done)
		elapsed_time, _ := math.ElapsedTime(source, velocity, radians)
		max_range, _ := math.MaxRange(source, elapsed_time, velocity, radians)
		projectile.ttl = elapsed_time
		projectile.max_range = max_range
		intervals, _ := math.TimeInterval(elapsed_time, TIME_INTERVALS)
		waiting_time := int((elapsed_time / TIME_INTERVALS) * 1000)
		config.AppLogger.Info("Report",
			zap.String("id", projectile.ID),
			zap.Float64("elapsed_time", elapsed_time),
			zap.Float64("max_range", max_range),
			zap.Int("intervals", len(intervals)),
			zap.Int("waiting_time", waiting_time))
		for _, t := range intervals {
			select {
			case <-projectile.done:
				config.AppLogger.Error("Projectile down", zap.String("id", projectile.ID))
				projectile.status = Failed
				return
			case <-time.After(time.Duration(waiting_time) * time.Millisecond):
				coordinates, _ := math.Position(source, velocity, radians, t)
				projectile.tracer <- TrajectoryCoordinates{projectile.ID, "Team A", coordinates}
			}
		}
		config.AppLogger.Info("Projectile success", zap.String("id", projectile.ID))
		projectile.Success()

	}(projectile)
	return projectile
}

func (p *Proectile) GetStatus() ProjectileStatus {
	return p.status
}

func (p *Proectile) Trajectory() chan TrajectoryCoordinates {
	return p.tracer
}

func (p *Proectile) Down() {
	p.done <- struct{}{}
}

func (p *Proectile) Success() {
	p.status = Success
}

func (p *Proectile) TTL() float64 {
	return p.ttl
}

func (p *Proectile) MaxRange() float64 {
	return p.max_range
}
