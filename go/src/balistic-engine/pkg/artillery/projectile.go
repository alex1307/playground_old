package artillery

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"context"
	"reflect"
	"time"

	"github.com/google/uuid"
	"go.uber.org/zap"
)

func (t *TrajectoryCoordinates) GetID() string {
	return t.ID
}

func (t *TrajectoryCoordinates) GetTeamID() string {
	return t.TeamID
}

func (t *TrajectoryCoordinates) GetCoordinates() math.Coordinates {
	return t.Coordinates
}

func NewProjectile(broker *message.Server) *Proectile {
	done := make(chan message.Payload, 100)
	missles := make(chan message.Payload, 100)
	broker.Subscribe("projectile", done)
	broker.Subscribe("missle", missles)
	return &Proectile{
		broker:  broker,
		done:    done,
		missles: missles}
}

func (p *Proectile) Fire(id string, source math.Coordinates, velocity, radians float64) string {
	uuid := uuid.NewString()
	go func(id string, done *chan message.Payload) {
		elapsed_time, _ := math.ElapsedTime(source, velocity, radians)
		max_range, _ := math.MaxRange(source, elapsed_time, velocity, radians)
		intervals, _ := math.TimeInterval(elapsed_time, TIME_INTERVALS)
		waiting_time := int64((elapsed_time / TIME_INTERVALS) * 1000)
		unix_time_now := time.Now().Unix() * 1000
		config.AppLogger.Info("Report",
			zap.String("id", p.ID),
			zap.Float64("elapsed_time", elapsed_time),
			zap.Float64("max_range", max_range),
			zap.Int("intervals", len(intervals)),
			zap.Int64("waiting_time", waiting_time))
		for _, t := range intervals {
			select {
			case shutted_down := <-*done:
				if shutted_down.GetID() == p.ID {
					p.status = Failed
					config.AppLogger.Info("Projectile down", zap.String("id", id))
					return
				}

			case <-time.After(time.Duration(waiting_time) * time.Millisecond):
				coordinates, _ := math.Position(source, velocity, radians, t)
				message := NewMessage("projectile", []string{"radar"}, &TrajectoryCoordinates{
					ID:          id,
					TeamID:      "Team A",
					SystemTime:  unix_time_now + waiting_time,
					Coordinates: coordinates})
				p.broker.Send(message)
			}
		}

	}(uuid, &p.done)
	return uuid
}

func (p *Proectile) Start(ctx context.Context) chan message.Payload {
	for {
		select {
		case m := <-p.missles:
			var missle Missle = reflect.ValueOf(&m).Elem().Interface().(Missle)
			p.Fire(missle.GetID(), missle.GetCoordinates(), missle.Velocity, missle.Radians)
		case <-time.After(5 * time.Second):
			config.AppLogger.Info("waiting for missles")
		case <-ctx.Done():
			{
				config.AppLogger.Info("Shutting down projectile")
				break
			}
		}
	}

}

func (p *Proectile) GetStatus() ProjectileStatus {
	return p.status
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

func (p *Proectile) GetID() string {
	return p.ID
}

func (m *Missle) GetID() string {
	return m.ID
}

func (m *Missle) GetTeamID() string {
	return m.TeamID
}

func (m *Missle) GetCoordinates() math.Coordinates {
	return m.Coordinates
}
