package monitoring

import (
	"balistic-engine/pkg/artillery"
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"context"
	"reflect"
	"time"

	"go.uber.org/zap"
)

type Space struct {
	seed        float64
	corners     [2]math.Coordinates
	lenght_seed float64
	height_seed float64
	monitor     map[int][]artillery.TrajectoryCoordinates
	active      map[string]int
}

type Radar struct {
	space  Space
	broker *message.Server
	source chan message.Payload
}

func NewRadar(broker *message.Server, space Space) *Radar {
	source := make(chan message.Payload, 100)
	broker.Subscribe("radar", source)
	return &Radar{
		space:  space,
		broker: broker,
		source: source,
	}
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
		monitor:     make(map[int][]artillery.TrajectoryCoordinates, 100),
		active:      make(map[string]int, 100),
	}
}

func (r *Radar) StartTracing(ctx context.Context) {
	go func(space *Space) {
		for {
			select {
			case tc := <-r.source:
				{
					var coordinates artillery.TrajectoryCoordinates = reflect.ValueOf(tc).Elem().Interface().(artillery.TrajectoryCoordinates)
					space.Move(coordinates)

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

func (s *Space) Move(trajectory artillery.TrajectoryCoordinates) {
	index := s.CoordinatesToIndex(trajectory.Coordinates)
	if projectiles, ok := s.monitor[index]; ok {
		remove_idx := -1

		for i, p := range projectiles {
			if p.TeamID != trajectory.TeamID {
				remove_idx = i
				break
			}
		}
		if remove_idx > -1 {
			updated := append(projectiles[:remove_idx], projectiles[remove_idx+1:]...)
			s.monitor[index] = updated
			delete(s.active, trajectory.ID)
			return
		} else {
			s.monitor[index] = append(projectiles, trajectory)
			s.active[trajectory.ID] = index
			return
		}
	}
	from_index, found := s.active[trajectory.ID]
	if found && from_index != index {
		if projectiles, ok := s.monitor[from_index]; ok {
			remove_idx := -1
			for i, p := range projectiles {
				if p.ID == trajectory.ID {
					remove_idx = i
					break
				}
			}
			if remove_idx > -1 {
				updated := append(projectiles[:remove_idx], projectiles[remove_idx+1:]...)
				s.monitor[from_index] = updated
			}
		}
	} else if found {
		return
	}
	s.active[trajectory.ID] = index
	s.monitor[index] = append(s.monitor[index], trajectory)
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
	projectiles, ok := s.monitor[index]
	if ok {
		ids := make([]string, len(projectiles))
		for i, p := range projectiles {
			ids[i] = p.ID
		}
		return ids
	}
	return nil
}

func (s *Space) GetAll() []artillery.TrajectoryCoordinates {
	var all []artillery.TrajectoryCoordinates
	for _, v := range s.monitor {
		all = append(all, v...)
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
