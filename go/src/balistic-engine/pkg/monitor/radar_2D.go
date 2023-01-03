package monitor

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"balistic-engine/pkg/projectile"
	"context"
	"fmt"
	"reflect"
	"time"

	"go.uber.org/zap"
)

func NewRadar(broker *message.Server, space Space) *Radar {
	source := make(chan message.Payload, 100)
	broker.Subscribe("radar", source)
	return &Radar{
		space:  space,
		broker: broker,
		source: source,
	}
}

func NewError(ID []string) TakenDown {
	return TakenDown{ID: ID}
}

func (TakenDown TakenDown) Error() string {
	for _, id := range TakenDown.ID {
		config.AppLogger.Error("Projectile taken down", zap.String("ID", id))
	}
	return fmt.Sprintf("Projectiles taken down: %v", TakenDown.ID)
}

func NewSpace(seed float64, corners [2]math.Coordinates) Space {

	if corners[0].X == corners[1].X && corners[0].Y == corners[1].Y {
		panic("points must be different")
	}

	if seed <= 0 {
		panic("seed must be positive")
	}

	if corners[0].X > corners[1].X {
		corners[0].X, corners[1].X = corners[1].X, corners[0].X
	}

	if corners[0].Y > corners[1].Y {
		corners[0].Y, corners[1].Y = corners[1].Y, corners[0].Y
	}
	return Space{
		seed:        seed,
		corners:     corners,
		lenght_seed: (corners[1].X - corners[0].X) / seed,
		height_seed: (corners[1].Y - corners[0].Y) / seed,
		active:      make(map[string]int, 100),
		projectiles: make(map[string]projectile.TrajectoryCoordinates, 100),
		statisctic:  make(map[string]projectile.ProjectileInfo, 100),
	}
}

func (r *Radar) StartTracing(ctx context.Context) {
	go func(space *Space) {
		for {
			select {
			case tc := <-r.source:
				{

					var coordinates projectile.TrajectoryCoordinates = reflect.ValueOf(tc).Elem().Interface().(projectile.TrajectoryCoordinates)
					// config.AppLogger.Info("Radar received coordinates",
					// 	zap.String("ID", coordinates.ID),
					// 	zap.String("Team", coordinates.GetTeamID()),
					// 	zap.Float64("X", coordinates.Coordinates.X),
					// 	zap.Float64("Y", coordinates.Coordinates.Y))
					err := space.Move(coordinates)
					if err != nil {
						for _, id := range err.(TakenDown).ID {
							message := message.NewMessage("ANY", []string{"projectile"}, &projectile.TrajectoryCoordinates{
								ID: id})
							r.broker.Send(message)
							config.AppLogger.Info("Projectile taken down",
								zap.String("ID", id))
							delete(r.space.projectiles, id)
						}

					}

				}
			case <-time.After(5 * time.Second):
				{
					config.AppLogger.Info("Space monitoring",
						zap.Int("Number of active projectiles", len(r.space.active)))
					continue
				}
			case <-ctx.Done():
				return
			}
		}
	}(&r.space)
}

func (s Space) CoordinatesToIndex(coordinate math.Coordinates) int {
	x_index := int(coordinate.X/s.lenght_seed) + 1
	y_index := int(coordinate.Y / s.height_seed)
	return y_index*int(s.seed) + x_index
}

func (s *Space) Move(trajectory projectile.TrajectoryCoordinates) error {
	index := s.CoordinatesToIndex(trajectory.Coordinates)
	// config.AppLogger.Info("Index",
	// 	zap.Int("index", index),
	// 	zap.String("Team", trajectory.TeamID),
	// 	zap.Float64("X", trajectory.Coordinates.X),
	// 	zap.Float64("Y", trajectory.Coordinates.Y))
	for id, idx := range s.active {
		if idx == index && id != trajectory.ID {
			return NewError([]string{id, trajectory.ID})
		}
	}
	s.active[trajectory.ID] = index
	return nil
	// config.AppLogger.Info("Monitoring projectile",
	// 	zap.String("ID", trajectory.ID),
	// 	zap.String("Team", trajectory.TeamID),
	// 	zap.Int("Index", index))
}

func (s *Space) GetIndex(uuid string) int {
	index, found := s.active[uuid]
	if found {
		return index
	}
	config.AppLogger.Error("Not found", zap.String("id", uuid))
	return -1
}

func (s *Space) GetActiveProjectiles(index int) []string {
	ids := make([]string, 0)
	for i, p := range s.active {
		if p == index {
			ids = append(ids, i)
		}
	}
	return ids

}

func (s *Space) GetAll() []projectile.TrajectoryCoordinates {
	var all []projectile.TrajectoryCoordinates
	for _, v := range s.projectiles {
		all = append(all, v)
	}
	return all
}

func (r *Radar) GetDetectedProjectiles() []string {
	config.AppLogger.Info("all active projectiles", zap.Int("count", len(r.space.active)))
	keys := make([]string, 0)
	for k := range r.space.active {
		keys = append(keys, k)
	}
	return keys
}
