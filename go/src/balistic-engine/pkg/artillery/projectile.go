package artillery

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"context"
	"reflect"
	"sync"
	"time"

	"github.com/google/uuid"
	"go.uber.org/zap"
)

type ProjectileHandler struct {
	ID          string
	broker      *message.Server
	done        chan message.Payload
	missles     chan message.Payload
	projectiles map[string]ProjectileInfo
	wg          *sync.WaitGroup
}

type ProjectileInfo struct {
	ID         string
	Status     ProjectileStatus
	TimeToLive float64
	MaxRange   float64
	FireAt     time.Time
	cancel     context.CancelFunc
}

func (t *TrajectoryCoordinates) GetID() string {
	return t.ID
}

func (t *TrajectoryCoordinates) GetTeamID() string {
	return t.TeamID
}

func (t *TrajectoryCoordinates) GetCoordinates() math.Coordinates {
	return t.Coordinates
}

func NewProjectile(broker *message.Server) *ProjectileHandler {
	done := make(chan message.Payload, 100)
	missles := make(chan message.Payload, 100)
	broker.Subscribe("projectile", done)
	broker.Subscribe("missle", missles)
	return &ProjectileHandler{
		broker:      broker,
		done:        done,
		missles:     missles,
		projectiles: make(map[string]ProjectileInfo, 100)}
}

func (p *ProjectileHandler) Fire(id string, team string, source math.Coordinates, velocity, radians float64, wg *sync.WaitGroup) string {
	uuid := uuid.NewString()
	ctx, cancel := context.WithCancel(context.Background())
	wg.Add(1)
	go func(uuid string, done chan message.Payload,
		ctx context.Context, cancel context.CancelFunc) {
		defer wg.Done()
		elapsed_time, _ := math.ElapsedTime(source, velocity, radians)
		max_range, _ := math.MaxRange(source, elapsed_time, velocity, radians)
		info := ProjectileInfo{
			ID:         id,
			Status:     Fired,
			FireAt:     time.Now(),
			MaxRange:   max_range,
			TimeToLive: elapsed_time,
			cancel:     cancel}
		p.projectiles[uuid] = info
		intervals, _ := math.TimeInterval(elapsed_time, TIME_INTERVALS)
		waiting_time := int64((elapsed_time / TIME_INTERVALS) * 1000)
		unix_time_now := time.Now().Unix() * 1000
		for _, t := range intervals {
			select {
			case <-ctx.Done():
				config.AppLogger.Info("Shutting down projectile", zap.String("id", id))
				if found, ok := p.projectiles[uuid]; ok {
					found.Status = TakenDown
					p.projectiles[uuid] = found
					config.AppLogger.Info("Status updated", zap.String("id", id))
				} else {
					config.AppLogger.Info("Status not updated", zap.String("id", id))
				}
				return
			case shutted_down := <-done:
				if found, ok := p.projectiles[shutted_down.GetID()]; ok {
					found.cancel()
					config.AppLogger.Info("Shutting down projectile",
						zap.String("id", found.ID),
						zap.String("uuid", shutted_down.GetID()))
				}
			case <-time.After(time.Duration(waiting_time) * time.Millisecond):
				coordinates, _ := math.Position(source, velocity, radians, t)
				if p.broker.IsSubscribed("radar") {
					message := message.NewMessage("projectile", []string{"radar"}, &TrajectoryCoordinates{
						ID:          uuid,
						TeamID:      team,
						SystemTime:  unix_time_now + waiting_time,
						Coordinates: coordinates})
					p.broker.Send(message)
				}
			}
		}

	}(uuid, p.done, ctx, cancel)
	return uuid
}

func (p *ProjectileHandler) FireAndForget(id string, team string, source math.Coordinates, velocity, radians float64) string {
	uuid := uuid.NewString()
	ctx, cancel := context.WithCancel(context.Background())
	go func(uuid string, done chan message.Payload,
		ctx context.Context, cancel context.CancelFunc) {
		elapsed_time, _ := math.ElapsedTime(source, velocity, radians)
		max_range, _ := math.MaxRange(source, elapsed_time, velocity, radians)
		info := ProjectileInfo{
			ID:         id,
			Status:     Fired,
			FireAt:     time.Now(),
			MaxRange:   max_range,
			TimeToLive: elapsed_time,
			cancel:     cancel}
		p.projectiles[uuid] = info
		intervals, _ := math.TimeInterval(elapsed_time, TIME_INTERVALS)
		waiting_time := int64((elapsed_time / TIME_INTERVALS) * 1000)
		unix_time_now := time.Now().Unix() * 1000
		for _, t := range intervals {
			select {
			case <-ctx.Done():
				config.AppLogger.Info("Shutting down projectile", zap.String("id", id))
				if found, ok := p.projectiles[uuid]; ok {
					found.Status = TakenDown
					p.projectiles[uuid] = found
					config.AppLogger.Info("Status updated", zap.String("id", id))
				} else {
					config.AppLogger.Info("Status not updated", zap.String("id", id))
				}
				return
			case shutted_down := <-done:
				if found, ok := p.projectiles[shutted_down.GetID()]; ok {
					found.cancel()
					config.AppLogger.Info("Shutting down projectile",
						zap.String("id", found.ID),
						zap.String("uuid", shutted_down.GetID()))
				}
			case <-time.After(time.Duration(waiting_time) * time.Millisecond):
				coordinates, _ := math.Position(source, velocity, radians, t)
				if p.broker.IsSubscribed("radar") {
					message := message.NewMessage("projectile", []string{"radar"}, &TrajectoryCoordinates{
						ID:          uuid,
						TeamID:      "Team A",
						SystemTime:  unix_time_now + waiting_time,
						Coordinates: coordinates})
					p.broker.Send(message)
				}
				// } else {
				// 	config.AppLogger.Debug("Radar not subscribed")
				// }

			}
		}

	}(uuid, p.done, ctx, cancel)
	return uuid
}

func (p *ProjectileHandler) Start(ctx context.Context) {
	for {
		select {
		case m := <-p.missles:
			var missle Missle = reflect.ValueOf(m).Elem().Interface().(Missle)
			p.FireAndForget(missle.GetID(), missle.GetTeamID(), missle.GetCoordinates(), missle.Velocity, missle.Radians)
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

func (p *ProjectileHandler) StartAndWait(ctx context.Context, wg *sync.WaitGroup) {
	p.wg = wg
	for {
		select {
		case m := <-p.missles:
			var missle Missle = reflect.ValueOf(m).Elem().Interface().(Missle)
			p.Fire(missle.GetID(), missle.GetTeamID(), missle.GetCoordinates(), missle.Velocity, missle.Radians, wg)
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

func (p *ProjectileHandler) GetID() string {
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

func (p *ProjectileHandler) GetProjectilesIds() []string {
	var ids []string
	for k := range p.projectiles {
		ids = append(ids, k)
	}
	return ids
}

func (p *ProjectileHandler) GetProjectile(id string) ProjectileInfo {
	return p.projectiles[id]
}

func (p *ProjectileHandler) GetByStatus(status ProjectileStatus) []ProjectileInfo {
	if len(p.projectiles) == 0 {
		return []ProjectileInfo{}
	}
	var projectiles []ProjectileInfo
	for _, v := range p.projectiles {
		if v.Status == status {
			projectiles = append(projectiles, v)
		}
	}
	return projectiles
}
