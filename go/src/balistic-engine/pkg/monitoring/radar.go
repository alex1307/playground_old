package monitoring

import (
	"balistic-engine/pkg/artillery"
	"balistic-engine/pkg/config"
	"context"
	"time"

	"go.uber.org/zap"
)

type Radar interface {
	Subscribe(id string)
	GetSubscriptions() []string
	GetActiveIDs() []string
	IsActive() bool
	TraceProjectile(id string, missle artillery.Missle)
	ProjectileMonitor() chan artillery.TrajectoryCoordinates
}

type RadarStation struct {
	source      map[string]chan artillery.Missle
	radar       chan artillery.TrajectoryCoordinates
	projectiles map[string]*artillery.Proectile
}

func NewRadarStation(capacity int) *RadarStation {
	return &RadarStation{
		radar:       make(chan artillery.TrajectoryCoordinates, capacity*artillery.TIME_INTERVALS/2),
		projectiles: make(map[string]*artillery.Proectile, capacity),
		source:      make(map[string]chan artillery.Missle, 2),
	}
}

func (r *RadarStation) IsActive() bool {
	return len(r.projectiles) > 0
}

func (r *RadarStation) GetActiveIDs() []string {
	var active []string
	for id := range r.projectiles {
		active = append(active, id)
	}
	return active
}

func (r *RadarStation) ProjectileMonitor() chan artillery.TrajectoryCoordinates {
	return r.radar
}

func (r *RadarStation) Subscribe(id string) {
	r.source[id] = make(chan artillery.Missle, 10)
}

func (r *RadarStation) TraceProjectile(id string, missle artillery.Missle) {
	if source, ok := r.source[id]; ok {
		source <- missle
	}
}

func (r *RadarStation) StartTracing(id string, ctx context.Context) {
	if missles, ok := r.source[id]; ok {
		for {
			select {
			case <-ctx.Done():
				config.AppLogger.Info("Radar station is shutting down", zap.String("id", id))
				close(missles)
				delete(r.source, id)
				if len(r.source) == 0 {
					close(r.radar)
				}
				return
			case missle := <-missles:
				go func(radar *RadarStation, missle artillery.Missle) {
					projectile := artillery.Fire(missle.ID, missle.Source, missle.Velocity, missle.Radians)
					go func(r *RadarStation, projectile *artillery.Proectile) {
						for cc := range projectile.Trajectory() {
							r.radar <- cc
						}
						delete(r.projectiles, projectile.ID)
					}(r, projectile)
					r.projectiles[id] = projectile
				}(r, missle)
			case <-time.After(1 * time.Second):
				config.AppLogger.Info("Waiting")
			}
		}

	}
}

func (r *RadarStation) Down(id string) {
	if projectile, ok := r.projectiles[id]; ok {
		projectile.Down()
		delete(r.projectiles, id)
	}
}
